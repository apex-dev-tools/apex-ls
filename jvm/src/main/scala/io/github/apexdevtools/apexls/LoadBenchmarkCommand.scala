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

import com.nawforce.apexlink.api.{BuildInfo, Org, ServerOps}
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.pkgforce.diagnostics.{LoggerOps, UNUSED_CATEGORY}
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.security.MessageDigest
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Measure a single cold workspace load with a fully explicit configuration.
  *
  * Everything that varies between the CLI and the editor load paths - parser, cache, unused
  * analysis and logging - has to be given on the command line, so no measurement can silently
  * inherit a default. Process level concerns (a fresh JVM, heap sizing, repetition, interleaving
  * and JFR capture) belong to the outer driver in tools/load-benchmark.
  */
private[apexls] object LoadBenchmarkCommand extends BatchCommand {
  override type Result = LoadBenchmarkResult

  override val name: String = "benchmark-load"

  /** The load is the measurement, so it is run here rather than by the shared workspace loader. */
  override val requiresWorkspace: Boolean = false

  override def validate(args: Seq[String]): Either[BatchError, Unit] = {
    LoadBenchmarkArguments.parse(args).map(_ => ())
  }

  override def execute(
    context: BatchContext,
    args: Seq[String]
  ): Either[BatchError, LoadBenchmarkResult] = {
    for {
      arguments <- LoadBenchmarkArguments.parse(args)
      _         <- validateCacheSelection(context.options)
      workspace <- validateWorkspace(context.options.workspace)
      result    <- measure(workspace, context.options, arguments)
    } yield result
  }

  override def writeResult(result: LoadBenchmarkResult): ujson.Value = {
    LoadBenchmarkReport.write(result)
  }

  /** A measurement that used the developer's default cache directory would be neither cold nor
    * safe, so one of the two cache options has to be given.
    */
  private def validateCacheSelection(options: BatchOptions): Either[BatchError, Unit] = {
    if (options.cacheEnabled && options.cacheDirectory.isEmpty) {
      Left(
        BatchError(
          "INVALID_ARGUMENT",
          "benchmark-load requires an explicit cache setting, pass '--no-cache' for a cold load " +
            "or '--cache-dir' with an isolated directory"
        )
      )
    } else if (!options.cacheEnabled && options.cacheDirectory.nonEmpty) {
      Left(BatchError("INVALID_ARGUMENT", "Options '--no-cache' and '--cache-dir' are exclusive"))
    } else {
      Right(())
    }
  }

  private def validateWorkspace(directory: String): Either[BatchError, Path] = {
    val workspace = Path(directory)
    if (!workspace.exists || !workspace.isDirectory) {
      Left(BatchError("INVALID_SCOPE", s"Workspace '$directory' is not a directory"))
    } else if (!workspace.join("sfdx-project.json").isFile) {
      Left(
        BatchError("INVALID_SCOPE", s"Workspace '$directory' does not contain sfdx-project.json")
      )
    } else {
      Right(workspace)
    }
  }

  private def measure(
    workspace: Path,
    options: BatchOptions,
    arguments: LoadBenchmarkArguments
  ): Either[BatchError, LoadBenchmarkResult] = {
    arguments.parallelism.foreach(applyParallelism)

    val collector         = new LoadPhaseCollector
    val previousCollector = LoggerOps.setTimingCollector(Some(collector))
    try {
      val start          = System.nanoTime()
      val org            = Org.newOrg(workspace, openOptions(options, arguments))
      val totalLoadNanos = System.nanoTime() - start
      if (org.getProjectConfig().isEmpty) {
        Left(BatchError("WORKSPACE_LOAD_FAILED", s"Unable to load workspace '$workspace'"))
      } else {
        Right(
          LoadBenchmarkResult(
            label = arguments.label,
            workspaceIdentity = identityOf(workspace),
            workspacePath = Option.when(arguments.includePaths)(workspace.toString),
            totalLoadNanos = totalLoadNanos,
            cacheFlushNanos = flushCache(org, options),
            phases = collector.phases,
            configuration = configurationOf(options, arguments),
            parallelism = parallelismOf(arguments, collector),
            size = sizeOf(org),
            issues = issuesOf(org),
            environment = environmentOf(arguments)
          )
        )
      }
    } finally {
      LoggerOps.setTimingCollector(previousCollector)
    }
  }

  private def openOptions(options: BatchOptions, arguments: LoadBenchmarkArguments): OpenOptions = {
    OpenOptions
      .default()
      .withParser(arguments.parser)
      .withLoggingLevel(arguments.logging)
      .withCache(options.cacheEnabled)
      .withCacheDirectory(options.cacheDirectory.getOrElse(""))
      .withAutoFlush(enabled = false)
      .withUnused(arguments.unused)
      .withUnusedOnError(arguments.unusedOnError)
  }

  /** Cache writing is timed separately, it is not part of the wait for first feedback. */
  private def flushCache(org: Org, options: BatchOptions): Option[Long] = {
    Option.when(options.cacheEnabled) {
      val start = System.nanoTime()
      org.flush()
      System.nanoTime() - start
    }
  }

  /** Bound the pool the parallel parse runs on. Only effective when the properties are still
    * unset and the global execution context has not been started, which is why the outer driver
    * passes them as JVM arguments instead. observedParseThreads reports what actually happened.
    */
  private def applyParallelism(level: Int): Unit = {
    LoadBenchmarkArguments.ContextProperties.foreach(property => {
      if (System.getProperty(property) == null) System.setProperty(property, level.toString)
    })
  }

  private def configurationOf(
    options: BatchOptions,
    arguments: LoadBenchmarkArguments
  ): LoadBenchmarkConfiguration = {
    LoadBenchmarkConfiguration(
      parser = ServerOps.getCurrentParser.toString,
      parserOption = arguments.parser,
      cacheEnabled = options.cacheEnabled,
      cacheDirectory = options.cacheDirectory.filter(_ => arguments.includePaths),
      cacheDirectoryProvided = options.cacheDirectory.nonEmpty,
      unused = arguments.unused,
      unusedOnError = arguments.unusedOnError,
      logging = arguments.logging,
      autoFlush = ServerOps.isAutoFlushEnabled
    )
  }

  private def parallelismOf(
    arguments: LoadBenchmarkArguments,
    collector: LoadPhaseCollector
  ): LoadBenchmarkParallelism = {
    LoadBenchmarkParallelism(
      requested = arguments.parallelism,
      availableProcessors = Runtime.getRuntime.availableProcessors(),
      contextNumThreads = contextProperty("numThreads"),
      contextMinThreads = contextProperty("minThreads"),
      contextMaxThreads = contextProperty("maxThreads"),
      observedParseThreads = collector.threadsFor(LoadPhaseCollector.ClassParseFile)
    )
  }

  /** The size profile needs the workspace indexes, which are not on the Org API. Org.newOrg
    * always returns an OrgImpl, and this is a tool rather than published surface.
    */
  private def sizeOf(org: Org): LoadBenchmarkSize = {
    val orgImpl   = org.asInstanceOf[OPM.OrgImpl]
    val indexes   = orgImpl.workspace.deployOrderedIndexes.toSeq
    val byNature  = Natures.map(nature => nature._1 -> controllingPaths(indexes, nature._2))
    val counts    = byNature.map(nature => nature._1 -> nature._2.size)
    val countOf   = counts.toMap
    val apexPaths = byNature.filter(nature => ApexSourceNatures.contains(nature._1)).flatMap(_._2)

    LoadBenchmarkSize(
      packageCount = orgImpl.packages.length,
      moduleCount = indexes.size,
      apexTypeCount = org.getTypeIdentifiers(apexOnly = true).length,
      apexClassFileCount = countOf.getOrElse("apex", 0),
      triggerFileCount = countOf.getOrElse("trigger", 0),
      metadataFileCount = indexes.map(metadataFileCount).sum,
      apexSourceBytes = apexPaths.map(fileSize).sum,
      byNature = counts.filter(_._2 > 0)
    )
  }

  private def controllingPaths(
    indexes: Seq[DocumentIndex],
    nature: MetadataNature
  ): Seq[PathLike] = {
    indexes.flatMap(_.getControllingDocuments(nature).map(_.path))
  }

  private def metadataFileCount(index: DocumentIndex): Int = {
    Natures.flatMap(nature => index.get(nature._2).values.flatten).distinct.size
  }

  private def fileSize(path: PathLike): Long = {
    Try(Files.size(java.nio.file.Paths.get(path.toString))).getOrElse(0L)
  }

  private def issuesOf(org: Org): LoadBenchmarkIssues = {
    val issues = org.issues.issuesForFiles(null, true, 0)
    val errors = issues.count(_.isError)
    val unused =
      issues.count(issue => !issue.isError && issue.rule().name() == UNUSED_CATEGORY.name)
    LoadBenchmarkIssues(errors, issues.length - errors - unused, unused)
  }

  private def environmentOf(arguments: LoadBenchmarkArguments): LoadBenchmarkEnvironment = {
    LoadBenchmarkEnvironment(
      javaVersion = property("java.version"),
      javaVendor = property("java.vendor"),
      jvmName = property("java.vm.name"),
      jvmVersion = property("java.vm.version"),
      osName = property("os.name"),
      osArch = property("os.arch"),
      osVersion = property("os.version"),
      maxHeapBytes = Runtime.getRuntime.maxMemory(),
      garbageCollectors =
        ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getName).toSeq.sorted,
      jvmArguments = jvmArguments(arguments.includePaths),
      implementationBuild = BuildInfo.implementationBuild
    )
  }

  /** Memory, GC and parallelism arguments, the ones that change what a run measures. Values that
    * look like file paths are redacted so that a JFR destination cannot leak a workspace location.
    */
  private def jvmArguments(includePaths: Boolean): Seq[String] = {
    ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toSeq
      .filter(argument => RetainedArguments.exists(argument.startsWith))
      .map(argument => if (includePaths) argument else redactPaths(argument))
      .sorted
  }

  private def redactPaths(argument: String): String = {
    val separator = argument.indexOf('=')
    val value     = if (separator < 0) "" else argument.substring(separator + 1)
    if (separator < 0 || !value.exists(character => character == '/' || character == '\\')) {
      argument
    } else {
      s"${argument.substring(0, separator)}=<redacted>"
    }
  }

  private def property(name: String): String = Option(System.getProperty(name)).getOrElse("")

  private def contextProperty(name: String): Option[String] = {
    Option(System.getProperty(s"scala.concurrent.context.$name"))
  }

  /** A stable identifier for a workspace that does not disclose where it is. */
  private def identityOf(workspace: Path): String = {
    val canonical = Try(workspace.native.toRealPath().toString).getOrElse(workspace.toString)
    val digest    = MessageDigest.getInstance("SHA-256")
    val bytes     = digest.digest(canonical.getBytes(StandardCharsets.UTF_8)).take(8)
    s"sha256:${bytes.map(byte => f"${byte & 0xff}%02x").mkString}"
  }

  private val ApexSourceNatures: Set[String] = Set("apex", "trigger")

  private val Natures: Seq[(String, MetadataNature)] = Seq(
    "apex"          -> ApexNature,
    "trigger"       -> TriggerNature,
    "label"         -> LabelNature,
    "component"     -> ComponentNature,
    "page"          -> PageNature,
    "flow"          -> FlowNature,
    "sObject"       -> SObjectNature,
    "field"         -> FieldNature,
    "fieldSet"      -> FieldSetNature,
    "sharingReason" -> SharingReasonNature
  )

  private val RetainedArguments: Seq[String] =
    Seq("-Xm", "-Xss", "-XX:", "-Dscala.concurrent.context.")
}
