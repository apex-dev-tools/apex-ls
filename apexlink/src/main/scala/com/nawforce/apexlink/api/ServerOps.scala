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
  val shortName         = "ANTLR".toLowerCase
  override def toString = "ANTLR"
}
case object OutlineParserSingleThreaded extends AvailableParser {
  val shortName         = "OutlineSingle".toLowerCase
  override def toString = "Outline Parser - Single Threaded"
}
case object OutlineParserMultithreaded extends AvailableParser {
  val shortName         = "OutlineMulti".toLowerCase
  override def toString = "Outline Parser - Multithreaded"
}

/** Collection of Ops functions for changing global behaviours */
object ServerOps {
  private var lazyBlocks: Boolean             = true
  private var duplicateObjectMonitor: Boolean = false
  private var autoFlush: Boolean              = true
  private var currentParser: AvailableParser  = ANTLRParser

  /** Are we using lazy blocks, this is enabled by default */
  def getLazyBlocks: Boolean = {
    lazyBlocks
  }

  /** Update lazy block flag */
  def setLazyBlocks(enable: Boolean): Unit = {
    lazyBlocks = enable
  }

  /** Is duplicate object monitor enabled, this is disabled by default */
  def getDuplicateObjectMonitoring: Boolean = {
    duplicateObjectMonitor
  }

  /** Update duplicate object monitor flag */
  def setDuplicateObjectMonitoring(enable: Boolean): Unit = {
    duplicateObjectMonitor = enable
  }

  /** Is auto flushing, this is enabled by default */
  def getAutoFlush: Boolean = {
    autoFlush
  }

  /** Update auto flushing flag */
  def setAutoFlush(enable: Boolean): Boolean = {
    val current = autoFlush
    autoFlush = enable
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
