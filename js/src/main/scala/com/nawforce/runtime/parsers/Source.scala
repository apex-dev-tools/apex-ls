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

import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation}
import com.nawforce.runtime.SourceBlob
import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import io.github.apexdevtools.apexparser.CaseInsensitiveInputStream

trait Locatable {
  var locationPath: PathLike
  var startLine: Int
  var startOffset: Int
  var endLine: Int
  var endOffset: Int

  def location: PathLocation = {
    PathLocation(locationPath, Location(startLine, startOffset, endLine, endOffset))
  }
}

/** Encapsulation of a chunk of Apex code, position tells you where it came from in path */
case class Source(
  path: PathLike,
  code: SourceData,
  lineOffset: Int,
  columnOffset: Int,
  outer: Option[Source]
) {
  lazy val hash: Int = code.hash

  def extractSource(context: ParserRuleContext): Source = {
    val stop    = CodeParser.toScala(context.stop).getOrElse(context.start)
    val subdata = code.subdata(context.start.startIndex, stop.stopIndex + 1)
    new Source(
      path,
      subdata,
      context.start.line - 1,
      context.start.charPositionInLine,
      outer = Some(this)
    )
  }

  def asInsensitiveStream: CaseInsensitiveInputStream = {
    code.asInsensitiveStream
  }

  def asString: String = {
    code.asString
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
          context.start.line,
          context.start.charPositionInLine,
          stop.line,
          stop.charPositionInLine + stop.text.length
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
}

object Source {
  def apply(path: PathLike, source: SourceBlob): Source = {
    new Source(path, SourceData(source), lineOffset = 0, columnOffset = 0, None)
  }
}
