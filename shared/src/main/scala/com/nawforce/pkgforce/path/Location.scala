/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

package com.nawforce.pkgforce.path

import io.github.apexdevtools.api.IssueLocation
import upickle.default.{macroRW, ReadWriter => RW}

/** Internal implementation for identifying sub-parts of a file. */
@upickle.implicits.key("Location")
final case class Location(startLine: Int, startPosition: Int, endLine: Int, endPosition: Int)
    extends IssueLocation {

  override def startLineNumber(): Int = startLine

  override def startCharOffset(): Int = startPosition

  override def endLineNumber(): Int = endLine

  override def endCharOffset(): Int = endPosition
}

object Location {
  implicit val rw: RW[Location] = macroRW

  val empty: Location = Location(1)
  val all: Location   = Location(1, 0, Int.MaxValue, 0)

  def apply(line: Int) = new Location(line, 0, line, 0)

  def apply(line: Int, position: Int) = new Location(line, position, line, position)

  def apply(startLine: Int, startPosition: Int, endLine: Int, endPosition: Int): Location = {
    new Location(startLine, startPosition, endLine, endPosition)
  }

  /** A zero-length point at the supplied one-based line and zero-based column. */
  def point(line: Int, column: Int): Location =
    new Location(line, column, line, column)

  /** The complete content of a one-based line as a half-open range. The end column is an explicit
    * sentinel which [[extract]] clamps before the line terminator.
    */
  def wholeLine(line: Int): Location =
    new Location(line, 0, line, Int.MaxValue)

  /** A half-open range from the supplied start coordinate (inclusive) to end coordinate
    * (exclusive). Lines are one-based and columns are zero-based Unicode code-point offsets.
    */
  def span(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Location =
    new Location(startLine, startColumn, endLine, endColumn)

  /** Extract a half-open location from source text. Coordinates beyond a line's content are
    * clamped before its line terminator, so [[wholeLine]] never consumes the following newline.
    * Lines beyond the source are clamped to the end, retaining [[all]] behavior.
    */
  def extract(source: String, location: Location): String =
    source.substring(
      sourceIndex(source, location.startLine, location.startPosition),
      sourceIndex(source, location.endLine, location.endPosition)
    )

  private def sourceIndex(source: String, targetLine: Int, targetColumn: Int): Int = {
    var index = 0
    var line  = 1
    while (line < targetLine && index < source.length) {
      source.charAt(index) match {
        case '\r' =>
          index += 1
          if (index < source.length && source.charAt(index) == '\n') index += 1
          line += 1
        case '\n' =>
          index += 1
          line += 1
        case _ => index += Character.charCount(Character.codePointAt(source, index))
      }
    }

    if (line < targetLine) {
      source.length
    } else {
      val lineStart = index
      while (
        index < source.length && source.charAt(index) != '\r' && source.charAt(index) != '\n'
      ) {
        index += Character.charCount(Character.codePointAt(source, index))
      }
      val lineLength = source.codePointCount(lineStart, index)
      source.offsetByCodePoints(lineStart, Math.max(0, Math.min(targetColumn, lineLength)))
    }
  }
}

sealed case class LocationAnd[T](location: Location, value: T)

/** Location within a specific file. */
@upickle.implicits.key("PathLocation")
final case class PathLocation(path: PathLike, location: Location) {
  override def toString: String = {
    s"$path: ${location.displayPosition}"
  }
}

object PathLocation {
  implicit val rw: RW[Location] = macroRW
}

/** Trait for things we can locate in some file at some position. */
trait Locatable {
  def location: PathLocation
}

/** Variation on locatable for when we don't know, may return null! */
trait UnsafeLocatable extends Locatable {
  def location: PathLocation

  def safeLocation: Option[PathLocation] = {
    val l = location
    if (l != null && l.path != null)
      Some(location)
    else
      None
  }
}

/** Base for things that might be positioned at some location, data is stored unwrapped to avoid
  * object overhead. It's an UnsafeLocatable because we can't be sure the mutable location will ever
  * be set
  */
class Positionable extends UnsafeLocatable {
  private var locationPath: PathLike = _
  private var startLine: Int         = _
  private var startOffset: Int       = _
  private var endLine: Int           = _
  private var endOffset: Int         = _

  def withLocation(location: PathLocation): this.type = {
    val l = location.location
    setLocation(location.path, l.startLine, l.startPosition, l.endLine, l.endPosition)
    this
  }

  def setLocation(
    path: PathLike,
    startLine: Int,
    startOffset: Int,
    endLine: Int,
    endOffset: Int
  ): Unit = {
    this.locationPath = path
    this.startLine = startLine
    this.startOffset = startOffset
    this.endLine = endLine
    this.endOffset = endOffset
  }

  def location: PathLocation = {
    PathLocation(locationPath, Location(startLine, startOffset, endLine, endOffset))
  }
}

/** Extension of Locatable for things that can also provide an additional location for some form of
  * identifier.
  */
trait IdLocatable extends Locatable {
  def idLocation: Location
  def idPathLocation: PathLocation = PathLocation(location.path, idLocation)
}
