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
package com.nawforce.pkgforce.xml

import com.nawforce.pkgforce.path.Location

import scala.collection.mutable

/** A source-accurate element range used to associate semantic XML trees with their lexical source.
  * Columns are zero-based Unicode code-point offsets, matching SourceData and parser locations.
  */
private[nawforce] final case class XMLSourceElement(
  qualifiedName: String,
  localName: String,
  namespace: Option[String],
  location: Location,
  children: Seq[XMLSourceElement]
)

private[nawforce] object XMLSourceRange {
  def index(source: String): Option[XMLSourceElement] = new Scanner(source).scan()

  private final case class Position(line: Int, column: Int)

  private final case class OpenElement(
    qualifiedName: String,
    localName: String,
    namespace: Option[String],
    start: Position,
    namespaces: Map[String, String],
    children: mutable.ArrayBuffer[XMLSourceElement]
  )

  private final class Scanner(source: String) {
    private var offset = 0
    private var line   = 1
    private var column = 0

    private val elements                       = mutable.Stack[OpenElement]()
    private var root: Option[XMLSourceElement] = None
    private var failed                         = false

    def scan(): Option[XMLSourceElement] = {
      while (!failed && offset < source.length) {
        if (startsWith("<!--")) skipUntil("-->", 4)
        else if (startsWith("<?")) skipUntil("?>", 2)
        else if (startsWith("<![CDATA[")) {
          if (elements.isEmpty) failed = true else skipUntil("]]>", 9)
        } else if (startsWith("<!DOCTYPE")) {
          if (elements.nonEmpty || root.nonEmpty) failed = true else skipDoctype()
        } else if (startsWith("</")) closeElement()
        else if (current == '<') openElement()
        else consumeText()
      }

      if (!failed && elements.isEmpty) root else None
    }

    private def current: Char = source.charAt(offset)

    private def position: Position = Position(line, column)

    private def startsWith(value: String): Boolean = source.startsWith(value, offset)

    private def advance(): Unit = {
      val char = current
      if (char == '\r') {
        offset += 1
        if (offset < source.length && source.charAt(offset) == '\n') offset += 1
        line += 1
        column = 0
      } else if (char == '\n') {
        offset += 1
        line += 1
        column = 0
      } else {
        if (
          Character.isHighSurrogate(char) && offset + 1 < source.length && Character.isLowSurrogate(
            source.charAt(offset + 1)
          )
        )
          offset += 2
        else
          offset += 1
        column += 1
      }
    }

    private def advance(count: Int): Unit = {
      var remaining = count
      while (remaining > 0 && offset < source.length) {
        advance()
        remaining -= 1
      }
    }

    private def skipUntil(terminator: String, prefixLength: Int): Unit = {
      advance(prefixLength)
      while (!failed && offset < source.length && !startsWith(terminator)) advance()
      if (offset == source.length) failed = true else advance(terminator.length)
    }

    private def skipDoctype(): Unit = {
      advance("<!DOCTYPE".length)
      var quote: Char = 0
      var subsetDepth = 0
      var complete    = false
      while (!failed && offset < source.length && !complete) {
        val char = current
        if (quote != 0) {
          if (char == quote) quote = 0
          advance()
        } else {
          if (startsWith("<!--")) skipUntil("-->", 4)
          else if (startsWith("<?")) skipUntil("?>", 2)
          else
            char match {
              case '\'' | '"' =>
                quote = char
                advance()
              case '[' =>
                subsetDepth += 1
                advance()
              case ']' =>
                if (subsetDepth == 0) failed = true
                else subsetDepth -= 1
                advance()
              case '>' if subsetDepth == 0 =>
                advance()
                complete = true
              case _ => advance()
            }
        }
      }
      if (!complete) failed = true
    }

    private def consumeText(): Unit = {
      val char = current
      if (elements.isEmpty && !isXmlWhitespace(char)) failed = true else advance()
    }

    private def openElement(): Unit = {
      val start = position
      advance()
      readName() match {
        case None => failed = true
        case Some(qualifiedName) =>
          val declarations = mutable.Map[String, String]()
          var complete     = false
          var selfClosing  = false
          while (!failed && !complete && offset < source.length) {
            skipWhitespace()
            if (offset >= source.length) {
              failed = true
            } else if (startsWith("/>")) {
              advance(2)
              complete = true
              selfClosing = true
            } else if (current == '>') {
              advance()
              complete = true
            } else {
              readAttribute() match {
                case Some(("xmlns", value)) =>
                  decodeReferences(value) match {
                    case Some(decoded) => declarations.update("", decoded)
                    case None          => failed = true
                  }
                case Some((name, value)) if name.startsWith("xmlns:") =>
                  decodeReferences(value) match {
                    case Some(decoded) =>
                      declarations.update(name.substring("xmlns:".length), decoded)
                    case None => failed = true
                  }
                case Some(_) => ()
                case None    => failed = true
              }
            }
          }
          if (!complete) failed = true
          if (!failed) {
            val parentNamespaces = elements.headOption.fold(
              Map("xml" -> "http://www.w3.org/XML/1998/namespace")
            )(_.namespaces)
            val namespaces      = parentNamespaces ++ declarations
            val (prefix, local) = splitName(qualifiedName)
            val namespace = namespaces.get(prefix).orElse(if (prefix.isEmpty) Some("") else None)
            val open = OpenElement(
              qualifiedName,
              local,
              namespace,
              start,
              namespaces,
              mutable.ArrayBuffer.empty
            )
            if (selfClosing) append(build(open, position)) else elements.push(open)
          }
      }
    }

    private def closeElement(): Unit = {
      advance(2)
      val name = readName()
      skipWhitespace()
      if (name.isEmpty || offset >= source.length || current != '>') failed = true
      else {
        advance()
        if (elements.isEmpty || elements.top.qualifiedName != name.get) failed = true
        else append(build(elements.pop(), position))
      }
    }

    private def readAttribute(): Option[(String, String)] = {
      readName().flatMap { name =>
        skipWhitespace()
        if (offset >= source.length || current != '=') None
        else {
          advance()
          skipWhitespace()
          if (offset >= source.length || (current != '\'' && current != '"')) None
          else {
            val quote = current
            advance()
            val start = offset
            while (offset < source.length && current != quote && current != '<') advance()
            if (offset >= source.length || current != quote) None
            else {
              val value = source.substring(start, offset)
              advance()
              Some(name -> value)
            }
          }
        }
      }
    }

    private def readName(): Option[String] = {
      val start = offset
      while (offset < source.length && !isNameDelimiter(current)) advance()
      if (offset == start) None else Some(source.substring(start, offset))
    }

    private def splitName(name: String): (String, String) = {
      val colon = name.indexOf(':')
      if (colon < 0) ("", name) else (name.substring(0, colon), name.substring(colon + 1))
    }

    private def skipWhitespace(): Unit =
      while (offset < source.length && isXmlWhitespace(current)) advance()

    private def build(open: OpenElement, end: Position): XMLSourceElement =
      XMLSourceElement(
        open.qualifiedName,
        open.localName,
        open.namespace,
        Location(open.start.line, open.start.column, end.line, end.column),
        open.children.toSeq
      )

    private def append(element: XMLSourceElement): Unit = {
      elements.headOption match {
        case Some(parent)         => parent.children.append(element)
        case None if root.isEmpty => root = Some(element)
        case None                 => failed = true
      }
    }

    private def isNameDelimiter(char: Char): Boolean =
      isXmlWhitespace(
        char
      ) || char == '/' || char == '>' || char == '=' || char == '<' || char == '\'' || char == '"'

    private def isXmlWhitespace(char: Char): Boolean =
      char == ' ' || char == '\t' || char == '\r' || char == '\n'

    private def decodeReferences(value: String): Option[String] = {
      val result = new StringBuilder()
      var index  = 0
      while (index < value.length) {
        if (value.charAt(index) != '&') {
          result.append(value.charAt(index))
          index += 1
        } else {
          val end = value.indexOf(';', index + 1)
          if (end < 0) return None
          val reference = value.substring(index + 1, end)
          val decoded = reference match {
            case "amp"                               => Some("&")
            case "apos"                              => Some("'")
            case "gt"                                => Some(">")
            case "lt"                                => Some("<")
            case "quot"                              => Some("\"")
            case numeric if numeric.startsWith("#x") => parseCodePoint(numeric.substring(2), 16)
            case numeric if numeric.startsWith("#")  => parseCodePoint(numeric.substring(1), 10)
            case _                                   => None
          }
          decoded match {
            case Some(text) => result.append(text)
            case None       => return None
          }
          index = end + 1
        }
      }
      Some(result.toString())
    }

    private def parseCodePoint(value: String, radix: Int): Option[String] = {
      try {
        val codePoint = Integer.parseInt(value, radix)
        if (!Character.isValidCodePoint(codePoint)) None
        else Some(new String(Character.toChars(codePoint)))
      } catch {
        case _: NumberFormatException => None
      }
    }
  }
}
