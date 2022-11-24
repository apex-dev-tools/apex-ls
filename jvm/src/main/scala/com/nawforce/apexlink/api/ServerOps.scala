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

/** Collection of Ops functions for changing global behaviours */
object ServerOps {
  private var lazyBlocks: Boolean            = true
  private var autoFlush: Boolean             = true
  private var externalAnalysis: Boolean      = false
  private var currentParser: AvailableParser = ANTLRParser

  def isLazyBlocksEnabled: Boolean = {
    lazyBlocks
  }

  def setLazyBlocks(enable: Boolean): Unit = {
    lazyBlocks = enable
  }

  def isAutoFlushEnabled: Boolean = {
    autoFlush
  }

  def setAutoFlush(enable: Boolean): Boolean = {
    val current = autoFlush
    autoFlush = enable
    current
  }

  def isExternalAnalysisEnabled: Boolean = {
    externalAnalysis
  }

  def setExternalAnalysis(enable: Boolean): Boolean = {
    val current = externalAnalysis
    externalAnalysis = enable
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
}
