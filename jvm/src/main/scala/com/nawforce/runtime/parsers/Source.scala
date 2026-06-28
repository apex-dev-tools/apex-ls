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

  def asInsensitiveStream: CharStream = {
    code.asInsensitiveStream
  }

  def asUTF8: Array[Byte] = {
    code.asUTF8
  }

  /** Find a location for a rule, adapts based on source offsets to give absolute position in file
    */
  def getLocation(context: ParserRuleContext): PathLocation = {
    val stop = Option(context.stop).getOrElse(context.start)
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

    // The column offset only applies to content on the first line of the fragment, where the
    // fragment does not start at column 0. The line offset applies throughout, mapping the
    // fragment-relative line back to its absolute position in the file (consistent with
    // stampLocation, which is used for normal CST node locations).
    var startPosition = location.startPosition
    if (location.startLine == 1)
      startPosition += columnOffset

    var endPosition = location.endPosition
    if (location.endLine == 1)
      endPosition += columnOffset

    Location(
      location.startLine + lineOffset,
      startPosition,
      location.endLine + lineOffset,
      endPosition
    )
  }

  def stampLocation(positionable: Positionable, context: ParserRuleContext): Unit = {
    val stop       = Option(context.stop).getOrElse(context.start)
    val stopLength = Option(stop.getText).fold(0)(_.length)

    positionable.setLocation(
      path,
      context.start.getLine + lineOffset,
      if (context.start.getLine == 1)
        context.start.getCharPositionInLine + columnOffset
      else
        context.start.getCharPositionInLine,
      stop.getLine + lineOffset,
      if (stop.getLine == 1)
        stop.getCharPositionInLine + stopLength + columnOffset
      else
        stop.getCharPositionInLine + stopLength
    )
  }
}

object Source {
  def apply(path: PathLike, source: SourceBlob): Source = {
    new Source(path, SourceData(source), lineOffset = 0, columnOffset = 0, None)
  }
}
