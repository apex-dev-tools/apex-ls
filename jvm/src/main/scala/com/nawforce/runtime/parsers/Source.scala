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
package com.nawforce.runtime.parsers

import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation, Positionable}
import com.nawforce.runtime.SourceBlob
import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import io.github.apexdevtools.apexparser.CaseInsensitiveInputStream
import org.antlr.v4.runtime.CharStream

/** A block of source code loaded from a file
  *
  * @param path source file path
  * @param code source file contents or some subpart of
  * @param lineOffset line in path where source code is found
  * @param columnOffset column of lineOffset where source code is found
  * @param outer outer source container that this was extracted from
  * @param startLine used by Outline parser for hack
  * @param startColumn used by Outline parser for hack
  */
case class Source(
  path: PathLike,
  code: SourceData,
  lineOffset: Int,
  columnOffset: Int,
  outer: Option[Source],
  startLine: Option[Int] = None,
  startColumn: Option[Int] = None
) {
  val hash: Int = code.hash

  def extractSource(context: ParserRuleContext): Source = {
    val subdata = code.subdata(context.start.getStartIndex, context.stop.getStopIndex + 1)
    new Source(
      path,
      subdata,
      context.start.getLine - 1,
      context.start.getCharPositionInLine,
      outer = Some(this)
    )
  }

  def asStream: CharStream = {
    code.asStream
  }

  def asInsensitiveStream: CaseInsensitiveInputStream = {
    code.asInsensitiveStream
  }

  def asUTF8: Array[Byte] = {
    code.asUTF8
  }

  /** Find a location for a rule, adapts based on source offsets to give absolute position in file
    */
  def getLocation(context: ParserRuleContext): PathLocation = {
    val stop = CodeParser.toScala(context.stop).getOrElse(context.start)
    PathLocation(
      path,
      adjustLocation(
        Location(
          context.start.getLine,
          context.start.getCharPositionInLine,
          stop.getLine,
          stop.getCharPositionInLine + stop.getText.length
        )
      )
    )
  }

  private def adjustLocation(location: Location): Location = {
    if (lineOffset == 0 && columnOffset == 0) {
      return location
    }

    val startLine     = location.startLine
    var startPosition = location.startPosition
    if (location.startLine == 1)
      startPosition += columnOffset

    val endLine     = location.endLine
    var endPosition = location.endPosition
    if (location.endLine == 1)
      endPosition += columnOffset

    Location(startLine, startPosition, endLine, endPosition)
  }

  def stampLocation(positionable: Positionable, context: ParserRuleContext): Unit = {
    // This is debug for https://github.com/nawforce/apex-link/issues/90
    if (context.stop == null) {
      val startLine = if (context.start == null) "null" else context.start.getLine
      throw new Exception(
        s"Apex parser context missing stop location, context at line $startLine, type ${context.getClass}"
      )
    }

    positionable.setLocation(
      path,
      context.start.getLine + lineOffset,
      if (context.start.getLine == 1)
        context.start.getCharPositionInLine + columnOffset
      else
        context.start.getCharPositionInLine,
      context.stop.getLine + lineOffset,
      if (context.stop.getLine == 1)
        context.stop.getCharPositionInLine + context.stop.getText.length + columnOffset
      else
        context.stop.getCharPositionInLine + context.stop.getText.length
    )
  }
}

object Source {
  def apply(path: PathLike, source: SourceBlob): Source = {
    new Source(path, SourceData(source), lineOffset = 0, columnOffset = 0, None)
  }
}
