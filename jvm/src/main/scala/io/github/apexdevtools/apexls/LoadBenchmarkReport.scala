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

import com.nawforce.apexlink.cst.ValidationStatsSnapshot

/** Effective load configuration, every value is set explicitly by the benchmark command. */
private[apexls] final case class LoadBenchmarkConfiguration(
  parser: String,
  parserOption: String,
  cacheEnabled: Boolean,
  cacheDirectory: Option[String],
  cacheDirectoryProvided: Boolean,
  unused: Boolean,
  unusedOnError: Boolean,
  logging: String,
  autoFlush: Boolean,
  blockPrefetchThreads: Int
)

/** Requested and observed parallelism for the measured load. */
private[apexls] final case class LoadBenchmarkParallelism(
  requested: Option[Int],
  availableProcessors: Int,
  contextNumThreads: Option[String],
  contextMinThreads: Option[String],
  contextMaxThreads: Option[String],
  observedParseThreads: Int
)

/** Workspace size profile, enough to interpret a timing from a workspace nobody else can see. */
private[apexls] final case class LoadBenchmarkSize(
  packageCount: Int,
  moduleCount: Int,
  apexTypeCount: Int,
  apexClassFileCount: Int,
  triggerFileCount: Int,
  metadataFileCount: Int,
  apexSourceBytes: Long,
  byNature: Seq[(String, Int)]
)

/** Issue counts, a cheap check that two runs really did analyse the same thing. */
private[apexls] final case class LoadBenchmarkIssues(errors: Int, warnings: Int, unused: Int)

/** Machine and JVM details, results are only comparable within one of these. */
private[apexls] final case class LoadBenchmarkEnvironment(
  javaVersion: String,
  javaVendor: String,
  jvmName: String,
  jvmVersion: String,
  osName: String,
  osArch: String,
  osVersion: String,
  maxHeapBytes: Long,
  garbageCollectors: Seq[String],
  jvmArguments: Seq[String],
  implementationBuild: String
)

/** A single measured workspace load. */
private[apexls] final case class LoadBenchmarkResult(
  label: Option[String],
  workspaceIdentity: String,
  workspacePath: Option[String],
  totalLoadNanos: Long,
  cacheFlushNanos: Option[Long],
  phases: Seq[LoadPhase],
  configuration: LoadBenchmarkConfiguration,
  parallelism: LoadBenchmarkParallelism,
  size: LoadBenchmarkSize,
  issues: LoadBenchmarkIssues,
  validation: ValidationStatsSnapshot,
  environment: LoadBenchmarkEnvironment
)

private[apexls] object LoadBenchmarkReport {
  final val SchemaVersion: Int = 2

  def write(result: LoadBenchmarkResult): ujson.Value = {
    ujson.Obj(
      "schemaVersion" -> SchemaVersion,
      "label"         -> optionalString(result.label),
      "workspace" -> ujson.Obj(
        "identity" -> result.workspaceIdentity,
        "path"     -> optionalString(result.workspacePath)
      ),
      "configuration" -> writeConfiguration(result.configuration),
      "parallelism"   -> writeParallelism(result.parallelism),
      "timings" -> ujson.Obj(
        "totalLoadMs"  -> milliseconds(result.totalLoadNanos),
        "cacheFlushMs" -> result.cacheFlushNanos.map(milliseconds).getOrElse(ujson.Null),
        "phases"       -> ujson.Arr(result.phases.map(writePhase): _*)
      ),
      "size"        -> writeSize(result.size),
      "issues"      -> writeIssues(result.issues),
      "validation"  -> writeValidation(result.validation),
      "environment" -> writeEnvironment(result.environment)
    )
  }

  /** Type resolution counts. These describe apex-ls' own behaviour, not the workspace, so they are
    * safe to report for a private codebase.
    */
  private def writeValidation(validation: ValidationStatsSnapshot): ujson.Value = {
    ujson.Obj(
      "typeContexts"     -> count(validation.typeContexts),
      "typeCacheHits"    -> count(validation.typeCacheHits),
      "typeCacheMisses"  -> count(validation.typeCacheMisses),
      "typeCacheLookups" -> count(validation.typeCacheLookups),
      "typeCacheHitRate" -> validation.typeCacheHitRate.map(ujson.Num).getOrElse(ujson.Null)
    )
  }

  private def writeConfiguration(configuration: LoadBenchmarkConfiguration): ujson.Value = {
    ujson.Obj(
      "parser"       -> configuration.parser,
      "parserOption" -> configuration.parserOption,
      "cache" -> ujson.Obj(
        "enabled"           -> configuration.cacheEnabled,
        "directoryProvided" -> configuration.cacheDirectoryProvided,
        "directory"         -> optionalString(configuration.cacheDirectory)
      ),
      "unused"               -> configuration.unused,
      "unusedOnError"        -> configuration.unusedOnError,
      "logging"              -> configuration.logging,
      "autoFlush"            -> configuration.autoFlush,
      "blockPrefetchThreads" -> configuration.blockPrefetchThreads
    )
  }

  private def writeParallelism(parallelism: LoadBenchmarkParallelism): ujson.Value = {
    ujson.Obj(
      "requested" -> parallelism.requested
        .map(value => ujson.Num(value.toDouble))
        .getOrElse(ujson.Null),
      "availableProcessors"  -> parallelism.availableProcessors,
      "contextNumThreads"    -> optionalString(parallelism.contextNumThreads),
      "contextMinThreads"    -> optionalString(parallelism.contextMinThreads),
      "contextMaxThreads"    -> optionalString(parallelism.contextMaxThreads),
      "observedParseThreads" -> parallelism.observedParseThreads
    )
  }

  private def writePhase(phase: LoadPhase): ujson.Value = {
    ujson.Obj(
      "phase"   -> phase.phase,
      "count"   -> ujson.Num(phase.count.toDouble),
      "totalMs" -> milliseconds(phase.totalNanos),
      "maxMs"   -> milliseconds(phase.maxNanos),
      "threads" -> phase.threads
    )
  }

  private def writeSize(size: LoadBenchmarkSize): ujson.Value = {
    ujson.Obj(
      "packageCount"       -> size.packageCount,
      "moduleCount"        -> size.moduleCount,
      "apexTypeCount"      -> size.apexTypeCount,
      "apexClassFileCount" -> size.apexClassFileCount,
      "triggerFileCount"   -> size.triggerFileCount,
      "metadataFileCount"  -> size.metadataFileCount,
      "apexSourceBytes"    -> ujson.Num(size.apexSourceBytes.toDouble),
      "byNature"           -> byNature(size)
    )
  }

  private def byNature(size: LoadBenchmarkSize): ujson.Value = {
    val counts = ujson.Obj()
    size.byNature.foreach(entry => counts(entry._1) = ujson.Num(entry._2.toDouble))
    counts
  }

  private def writeIssues(issues: LoadBenchmarkIssues): ujson.Value = {
    ujson.Obj("errors" -> issues.errors, "warnings" -> issues.warnings, "unused" -> issues.unused)
  }

  private def writeEnvironment(environment: LoadBenchmarkEnvironment): ujson.Value = {
    ujson.Obj(
      "javaVersion"         -> environment.javaVersion,
      "javaVendor"          -> environment.javaVendor,
      "jvmName"             -> environment.jvmName,
      "jvmVersion"          -> environment.jvmVersion,
      "osName"              -> environment.osName,
      "osArch"              -> environment.osArch,
      "osVersion"           -> environment.osVersion,
      "maxHeapBytes"        -> ujson.Num(environment.maxHeapBytes.toDouble),
      "garbageCollectors"   -> ujson.Arr(environment.garbageCollectors.map(ujson.Str): _*),
      "jvmArguments"        -> ujson.Arr(environment.jvmArguments.map(ujson.Str): _*),
      "implementationBuild" -> environment.implementationBuild
    )
  }

  private def count(value: Long): ujson.Value = ujson.Num(value.toDouble)

  /** Nanoseconds as milliseconds to microsecond precision. */
  private def milliseconds(nanos: Long): ujson.Value = {
    ujson.Num(math.round(nanos / 1000.0) / 1000.0)
  }

  private def optionalString(value: Option[String]): ujson.Value = {
    value.map(ujson.Str).getOrElse(ujson.Null)
  }
}
