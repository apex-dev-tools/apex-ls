/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.api

import com.nawforce.pkgforce.diagnostics.LoggerOps

sealed trait AvailableParser { val shortName: String }
case object ANTLRParser extends AvailableParser {
  val shortName: String = "ANTLR".toLowerCase
  override def toString = "ANTLR"
}
case object OutlineParserSingleThreaded extends AvailableParser {
  val shortName: String = "OutlineSingle".toLowerCase
  override def toString = "Outline Parser - Single Threaded"
}
case object OutlineParserMultithreaded extends AvailableParser {
  val shortName: String = "OutlineMulti".toLowerCase
  override def toString = "Outline Parser - Multithreaded"
}

/** Deprecated, external analysis provider (SPI) support has been removed and this no longer has
  * any effect.
  */
sealed trait AnalysisMode {
  val shortName: String         = toString.toLowerCase
  override def toString: String = shortName
}
case object NoAnalysis extends AnalysisMode {
  override def toString: String = "NoAnalysis"
}
case object RefreshAnalysis extends AnalysisMode {
  override def toString: String = "RefreshAnalysis"
}
case object LoadAndRefreshAnalysis extends AnalysisMode {
  override def toString: String = "LoadAndRefreshAnalysis"
}

/** Indexer configuration settings, if file events are observed at an interval < rescanTriggerTimeMs
  * a re-scan is queued. The re-scan is performed after quietPeriodForRescanMs has elapsed without
  * any further file events. Both must be non-zero for the indexer to be active.
  */
case class IndexerConfiguration(rescanTriggerTimeMs: Long, quietPeriodForRescanMs: Long) {
  val enabled: Boolean = rescanTriggerTimeMs > 0 && quietPeriodForRescanMs > 0
}

/** Deprecated, external analysis provider (SPI) support has been removed and this no longer has
  * any effect. Retained as a no-op stub to avoid breaking existing clients.
  */
case class ExternalAnalysisConfiguration(
  mode: AnalysisMode,
  params: Map[String, List[(String, List[String])]]
)

object ExternalAnalysisConfiguration {
  def apply(
    mode: String,
    params: Map[String, List[(String, List[String])]] = Map()
  ): ExternalAnalysisConfiguration = {
    val analysisMode = mode.toLowerCase match {
      case NoAnalysis.shortName             => NoAnalysis
      case RefreshAnalysis.shortName        => RefreshAnalysis
      case LoadAndRefreshAnalysis.shortName => LoadAndRefreshAnalysis
      case _ => throw new IllegalArgumentException(s"Unexpected analysis mode '$mode'")
    }
    new ExternalAnalysisConfiguration(analysisMode, params)
  }
}

/** Collection of Ops functions for changing global behaviours */
object ServerOps {

  /** Thread counts accepted by [[setBlockPrefetchThreads]]. */
  val validBlockPrefetchThreads: Seq[Int] = Seq(0, 2, 4)

  private var autoFlush                      = true
  private val defaultExternalAnalysis        = ExternalAnalysisConfiguration(RefreshAnalysis, Map())
  private var currentParser: AvailableParser = OutlineParserMultithreaded
  private var indexerConfiguration           = IndexerConfiguration(0, 0)
  @volatile private var blockPrefetchThreads = 0

  def isAutoFlushEnabled: Boolean = {
    autoFlush
  }

  def setAutoFlush(enable: Boolean): Boolean = {
    val current = autoFlush
    autoFlush = enable
    current
  }

  def getExternalAnalysis: ExternalAnalysisConfiguration = {
    LoggerOps.info(
      "getExternalAnalysis is deprecated and no longer has any effect. " +
        "External analysis provider (SPI) support has been removed."
    )
    defaultExternalAnalysis
  }

  def setExternalAnalysis(config: ExternalAnalysisConfiguration): ExternalAnalysisConfiguration = {
    LoggerOps.info(
      "setExternalAnalysis is deprecated and no longer has any effect. " +
        "External analysis provider (SPI) support has been removed."
    )
    defaultExternalAnalysis
  }

  def getCurrentParser: AvailableParser = {
    currentParser
  }

  def setCurrentParser(newParser: String): AvailableParser = {
    newParser.toLowerCase match {
      case ANTLRParser.shortName                 => setCurrentParser(ANTLRParser)
      case OutlineParserSingleThreaded.shortName => setCurrentParser(OutlineParserSingleThreaded)
      case OutlineParserMultithreaded.shortName  => setCurrentParser(OutlineParserMultithreaded)
      case _                                     => currentParser
    }
  }

  def setCurrentParser(newParser: AvailableParser): AvailableParser = {
    val previousParser = currentParser
    newParser match {
      case ANTLRParser =>
        LoggerOps.info(
          "Parser setting 'ANTLR' is deprecated and no longer has any effect. " +
            "Apex class parsing now always uses OutlineParser. " +
            "Please remove this setting before 7.0.0."
        )
      case _ =>
        currentParser = newParser
    }
    previousParser
  }

  def getBlockPrefetchThreads: Int = {
    blockPrefetchThreads
  }

  /** Set how many threads parse method bodies ahead of validation during a load, zero to disable.
    *
    * Bodies are parsed lazily on first verify, which places that parsing inside the sequential
    * validation pass even though it has no ordering constraint. Parsing them ahead of the
    * validation cursor removes it from the critical path at the cost of holding the parsed
    * statements, which are otherwise weakly reachable, until they are used.
    *
    * Only 0, 2 or 4 are accepted. Parsing does not scale beyond a few threads, its per block cost
    * grows with concurrency, so higher counts spend CPU without shortening the load. Any other
    * value is ignored and the current setting is kept.
    */
  def setBlockPrefetchThreads(threads: Int): Int = synchronized {
    val previous = blockPrefetchThreads
    if (ServerOps.validBlockPrefetchThreads.contains(threads))
      blockPrefetchThreads = threads
    else
      LoggerOps.info(
        s"Ignoring block prefetch thread count '$threads', expecting one of " +
          ServerOps.validBlockPrefetchThreads.mkString(", ")
      )
    previous
  }

  /** Apply an optional per-open override and return the effective value as one atomic operation.
    * An absent override retains the current process-wide setting, which starts at zero.
    */
  private[nawforce] def resolveBlockPrefetchThreads(threads: Option[Int]): Int = synchronized {
    threads.foreach(setBlockPrefetchThreads)
    blockPrefetchThreads
  }

  def getIndexerConfiguration: IndexerConfiguration = {
    indexerConfiguration
  }

  def setIndexerConfiguration(config: IndexerConfiguration): IndexerConfiguration = {
    val old = indexerConfiguration
    indexerConfiguration = config
    old
  }
}
