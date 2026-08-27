/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package io.github.apexdevtools.apexls

import com.nawforce.apexlink.types.apex.FullDeclaration
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.diagnostics.TimingCollector

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** Timings for one load phase, aggregated over every span that reported against it. */
private[apexls] final case class LoadPhase(
  phase: String,
  count: Long,
  totalNanos: Long,
  maxNanos: Long,
  threads: Int
)

/** Collects load phase timings from LoggerOps timed spans.
  *
  * Only spans on a fixed allow list are kept, and only the phase name they map to is retained. The
  * span labels of the fine grained spans carry file paths, so recording anything outside the allow
  * list would leak workspace content into benchmark output.
  */
private[apexls] final class LoadPhaseCollector extends TimingCollector {
  private val accumulators = new ConcurrentHashMap[String, LoadPhaseCollector.Accumulator]()

  override def record(label: String, elapsedNanos: Long): Unit = {
    LoadPhaseCollector
      .phaseOf(label)
      .foreach(phase => {
        accumulators
          .computeIfAbsent(phase, _ => new LoadPhaseCollector.Accumulator)
          .add(elapsedNanos, Thread.currentThread().getName)
      })
  }

  /** Phases seen so far, ordered by name so that output is stable across runs. */
  def phases: Seq[LoadPhase] = {
    accumulators.asScala
      .map(entry => entry._2.toPhase(entry._1))
      .toSeq
      .sortBy(_.phase)
  }

  /** Number of distinct threads that ran the given phase, zero if it was never seen. */
  def threadsFor(phase: String): Int = {
    Option(accumulators.get(phase)).map(_.threadCount).getOrElse(0)
  }
}

private[apexls] object LoadPhaseCollector {
  final val ClassParseFile = "classParseFile"

  private val ClassParseBatch   = """^Parsed \d+ classes$""".r
  private val TriggerParseBatch = """^Parsed \d+ triggers$""".r

  /** Map a timed span label onto a phase name, dropping labels that are not load phases. */
  private[apexls] def phaseOf(label: String): Option[String] = {
    label match {
      case "Org created"                             => Some("orgCreate")
      case "Workspace scanned"                       => Some("workspaceScan")
      case "Modules deployed"                        => Some("moduleDeploy")
      case "Loaded summary classes"                  => Some("summaryCacheLoad")
      case "Closed plugins (unused analysis)"        => Some("unusedAnalysis")
      case TypeDeclaration.MethodMapSpan             => Some("validateMethodMap")
      case TypeDeclaration.ConstructorMapSpan        => Some("validateConstructorMap")
      case FullDeclaration.BodyDeclarationSpan       => Some("validateBodyDeclarations")
      case FullDeclaration.OuterDependencySpan       => Some("validateOuterDependencies")
      case ClassParseBatch()                         => Some("classParseBatch")
      case TriggerParseBatch()                       => Some("triggerParseBatch")
      case _ if label.startsWith("Parsed ")          => Some(ClassParseFile)
      case _ if label.startsWith("Validated ")       => Some("validateFile")
      case _ if label.startsWith("Indexer scanned ") => Some("indexerScan")
      case _                                         => None
    }
  }

  private final class Accumulator {
    private var count      = 0L
    private var totalNanos = 0L
    private var maxNanos   = 0L
    private val threads    = new ConcurrentHashMap[String, java.lang.Boolean]()

    def add(elapsedNanos: Long, thread: String): Unit = {
      threads.put(thread, java.lang.Boolean.TRUE)
      synchronized {
        count += 1
        totalNanos += elapsedNanos
        if (elapsedNanos > maxNanos) maxNanos = elapsedNanos
      }
    }

    def threadCount: Int = threads.size()

    def toPhase(phase: String): LoadPhase = {
      synchronized { LoadPhase(phase, count, totalNanos, maxNanos, threads.size()) }
    }
  }
}
