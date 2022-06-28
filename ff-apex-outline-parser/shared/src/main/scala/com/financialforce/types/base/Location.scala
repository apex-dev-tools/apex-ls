/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types.base

/** Position in a source file, uses line/lineOffset & byteOffset to save recomputing later. */
case class Position(line: Int, lineOffset: Int, byteOffset: Int) {
  override def toString: String = {
    s"[$line.$lineOffset;$byteOffset"
  }
}

/** Location of a range in a source file, logically a start and end position but unrolled to reduce object overhead.
  * Note: We don't enforce constraints such as end>=start so consumers should take care, see also Location.default.
  */
case class Location(
  startLine: Int,
  startLineOffset: Int,
  startByteOffset: Int,
  endLine: Int,
  endLineOffset: Int,
  endByteOffset: Int
) {
  def startPosition: Position = Position(startLine, startLineOffset, startByteOffset)
  def endPosition: Position   = Position(endLine, endLineOffset, endByteOffset)

  override def toString: String = {
    s"[$startLine.$startLineOffset->$endLine.$endLineOffset;$startByteOffset->$endByteOffset]"
  }
}

object Location {
  /* A default location, maps to start of the file but has zero characters. */
  val default: Location = Location(0, 0, 0, 0, 0, 0)

  def apply(start: Position, end: Position): Location = {
    Location(
      start.line,
      start.lineOffset,
      start.byteOffset,
      end.line,
      end.lineOffset,
      end.byteOffset
    )
  }

  def span(sl: Location, el: Location): Location = {
    Location(
      sl.startLine,
      sl.startLineOffset,
      sl.startByteOffset,
      el.endLine,
      el.endLineOffset,
      el.endByteOffset
    )
  }
}
