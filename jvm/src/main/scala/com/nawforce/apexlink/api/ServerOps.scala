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

/** Setting for external analysis, such as PMD. The mode determines if/when analysis is performed
  * while the params provide directives on how to perform the analysis. Params are grouped by
  * analysis provider name, e.g. PMD, within each group is a list of params each with a list
  * of values (which may be empty). The meaning of the params is specific to each provider.
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
  private var autoFlush                      = true
  private var externalAnalysis               = ExternalAnalysisConfiguration(RefreshAnalysis, Map())
  private var currentParser: AvailableParser = ANTLRParser
  private var indexerConfiguration           = IndexerConfiguration(0, 0)
  private var allowPrivateOverride           = false

  def isAutoFlushEnabled: Boolean = {
    autoFlush
  }

  def setAutoFlush(enable: Boolean): Boolean = {
    val current = autoFlush
    autoFlush = enable
    current
  }

  def getExternalAnalysis: ExternalAnalysisConfiguration = {
    externalAnalysis
  }

  def setExternalAnalysis(config: ExternalAnalysisConfiguration): ExternalAnalysisConfiguration = {
    val current = externalAnalysis
    externalAnalysis = config
    current
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
    currentParser = newParser
    previousParser
  }

  def getIndexerConfiguration: IndexerConfiguration = {
    indexerConfiguration
  }

  def setIndexerConfiguration(config: IndexerConfiguration): IndexerConfiguration = {
    val old = indexerConfiguration
    indexerConfiguration = config
    old
  }

  def isPrivateOverrideAllowed: Boolean = {
    allowPrivateOverride
  }

  def setAllowPrivateOverride(enable: Boolean): Boolean = {
    val current = allowPrivateOverride
    allowPrivateOverride = enable
    current
  }

}
