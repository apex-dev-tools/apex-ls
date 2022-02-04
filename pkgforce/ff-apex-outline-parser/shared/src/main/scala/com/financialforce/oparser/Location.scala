/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

case class Location(
  startLine: Int,
  startLineOffset: Int,
  startByteOffset: Int,
  endLine: Int,
  endLineOffset: Int,
  endByteOffset: Int
) {
  override def toString: String = {
    s"[$startLine.$startLineOffset->$endLine.$endLineOffset;$startByteOffset->$endByteOffset]"
  }
}

object Location {
  val default: Location = Location(0, 0, 0, 0, 0, 0)

  def fromStart(l: Location): Location = {
    Location(l.startLine, l.startLineOffset, l.startByteOffset, 0, 0, 0)
  }

  def from(sl: Location, el: Location): Location = {
    Location(
      sl.startLine,
      sl.startLineOffset,
      sl.startByteOffset,
      el.endLine,
      el.endLineOffset,
      el.endByteOffset
    )
  }

  def updateEnd(src: Location, l: Location): Location = {
    src.copy(endLine = l.endLine, endLineOffset = l.endLineOffset, endByteOffset = l.endByteOffset)
  }

  def updateEnd(src: Location, endLine: Int, endLineOffset: Int, endByteOffset: Int): Location = {
    src.copy(endLine = endLine, endLineOffset = endLineOffset, endByteOffset = endByteOffset)
  }
}
