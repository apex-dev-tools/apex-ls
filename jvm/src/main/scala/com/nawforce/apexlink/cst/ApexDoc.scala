/*
 Copyright (c) 2026 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.cst

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/** A tolerant, best-effort reader and markdown renderer for ApexDoc comments.
  *
  * Real-world ApexDoc rarely follows the Salesforce format, so nothing here validates it. The reader
  * strips decoration, splits the main description from block tags and passes anything it does not
  * recognise through visibly. It works on the raw comment text so it is independent of how the
  * comment was captured.
  */
object ApexDoc {

  /** A block tag, `name` as written without the `@` and `text` with its continuation lines. */
  final case class Tag(name: String, text: String) {
    def kind: String = canonicalName(name)
  }

  /** The main description, including any `@description` content, and the remaining block tags. */
  final case class Doc(description: String, tags: Seq[Tag])

  val MaxDescriptionLines = 10
  val MaxDescriptionChars = 1000
  val MaxTagChars         = 300
  val MaxLength           = 3000

  private val Ellipsis        = "…"
  private val BannerChars     = "*=-_#+/"
  private val TagLine         = "^\\s*@([A-Za-z][A-Za-z0-9_-]*)(.*)$".r
  private val Dashes          = Set("-", "–", "—")
  private val InlineTextTags  = Set("link", "linkplain", "literal", "hidden")
  private val MarkdownSpecial = "\\`*_[]#"

  private val Aliases = Map("exception" -> "throws", "returns" -> "return", "params" -> "param")

  def canonicalName(name: String): String = {
    val lower = name.toLowerCase
    Aliases.getOrElse(lower, lower)
  }

  /** Comment text with the delimiters, leading asterisk runs and banner lines removed. Absent when
    * nothing but decoration remains.
    */
  def text(raw: String): Option[String] = Some(strip(raw).mkString("\n")).filter(_.nonEmpty)

  /** Markdown for a doc comment, absent if it holds nothing to show. Never fails; content that
    * cannot be rendered is left for the caller to fall back on the signature alone.
    */
  def markdown(raw: String): Option[String] = Try(render(parse(raw))).toOption.flatten

  def parse(raw: String): Doc = {
    val description          = ArrayBuffer[Line]()
    val tags                 = ArrayBuffer[(String, ArrayBuffer[Line])]()
    var current              = description
    var fence: Option[Fence] = None

    strip(raw).foreach(line => {
      line match {
        case TagLine(name, rest) if fence.isEmpty =>
          val first = rest.trim.stripPrefix(":").trim
          if (canonicalName(name) == "description") {
            if (description.exists(_.text.trim.nonEmpty)) description += Line("", fromTag = false)
            current = description
          } else {
            current = ArrayBuffer[Line]()
            tags += ((name, current))
          }
          if (first.nonEmpty) {
            fence = updateFence(fence, first)
            current += Line(first, fromTag = true)
          }
        case _ =>
          fence = updateFence(fence, line)
          current += Line(line, fromTag = false)
      }
    })

    Doc(block(description.toSeq), tags.map(t => Tag(t._1, block(t._2.toSeq))).toSeq)
  }

  private final case class Line(text: String, fromTag: Boolean)

  /* Joins a block's lines, removing the indentation its continuation lines share. Text on a tag line
   * was trimmed where the tag ended so it takes no part in finding the common indent.
   */
  private def block(lines: Seq[Line]): String = {
    val continuation = lines.filterNot(_.fromTag).map(_.text)
    val indent       = commonIndent(continuation)
    trimBlank(lines.map(line => if (line.fromTag) line.text else dedent(line.text, indent)))
      .mkString("\n")
  }

  private def commonIndent(lines: Seq[String]): Int = {
    val indents = lines.filter(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length)
    if (indents.isEmpty) 0 else indents.min
  }

  private def dedent(line: String, indent: Int): String =
    line.drop(math.min(indent, line.takeWhile(_ == ' ').length))

  private[cst] def strip(raw: String): Seq[String] = {
    var body = raw.trim
    if (body.startsWith("/**"))
      body = body.substring(3)
    if (body.endsWith("*/"))
      body = body.substring(0, body.length - 2).reverse.dropWhile(_ == '*').reverse

    /* Only lines led by asterisks keep indentation, the common part of which is removed */
    val lines = body
      .split("\r?\n", -1)
      .toSeq
      .map(line => {
        val trimmed  = line.trim
        val starred  = trimmed.startsWith("*")
        val content  = if (starred) expandIndent(trimmed.dropWhile(_ == '*')) else trimmed
        val cleaned  = stripTrailing(content)
        val isBanner = cleaned.trim.forall(c => BannerChars.indexOf(c) >= 0)
        (if (isBanner) "" else cleaned, starred)
      })
    val indent = commonIndent(lines.filter(_._2).map(_._1))
    trimBlank(lines.map(line => if (line._2) dedent(line._1, indent) else line._1))
  }

  private def expandIndent(line: String): String = {
    val indent = line.takeWhile(_.isWhitespace)
    indent.replace("\t", "    ") + line.substring(indent.length)
  }

  /* Removes trailing whitespace and a trailing box border of asterisks, scanning back from the end */
  private def stripTrailing(line: String): String = {
    var end = line.length
    while (end > 0 && line.charAt(end - 1).isWhitespace) end -= 1
    var stars = end
    while (stars > 0 && line.charAt(stars - 1) == '*') stars -= 1
    if (stars < end && stars > 0 && line.charAt(stars - 1).isWhitespace) {
      end = stars
      while (end > 0 && line.charAt(end - 1).isWhitespace) end -= 1
    }
    line.substring(0, end)
  }

  private[cst] def render(doc: Doc): Option[String] = {
    val (params, rest1)  = doc.tags.partition(t => t.kind == "param" && target(t).nonEmpty)
    val (returns, rest2) = rest1.partition(_.kind == "return")
    val (throws, rest3)  = rest2.partition(t => t.kind == "throws" && target(t).nonEmpty)
    val (sees, others)   = rest3.partition(_.kind == "see")

    val sections = Seq(
      renderDescription(doc.description.split("\n", -1).toSeq),
      list("Parameters", params.map(targetItem)),
      returns.map(t => s"**Returns**${separated(tagText(t.text))}"),
      list("Throws", throws.map(targetItem)),
      list("See", sees.map(t => s"- ${tagText(t.text)}"))
    ) ++ others.map(renderOther)

    assemble(sections.filter(_.exists(_.nonEmpty)))
  }

  private def renderDescription(lines: Seq[String]): Seq[String] = {
    val (kept, truncated) = cap(collapseBlank(lines), MaxDescriptionLines, MaxDescriptionChars)
    val block             = renderBlock(kept)
    if (truncated) block :+ Ellipsis else block
  }

  private def renderOther(tag: Tag): Seq[String] = {
    val lines = tag.text.split("\n", -1).toSeq
    if (tag.kind == "example" && lines.size > 1) {
      val (kept, truncated) = cap(lines, MaxDescriptionLines, MaxDescriptionChars)
      val body =
        if (kept.exists(line => openingFence(line).nonEmpty)) renderBlock(kept)
        else ("```apex" +: kept) :+ "```"
      (s"**@${tag.name}**" +: body) ++ (if (truncated) Seq(Ellipsis) else Nil)
    } else {
      Seq(
        s"**@${tag.name}**${Some(tagText(tag.text)).filter(_.nonEmpty).map(" " + _).getOrElse("")}"
      )
    }
  }

  private def list(title: String, items: Seq[String]): Seq[String] =
    if (items.isEmpty) Nil else s"**$title**" +: items

  private def targetItem(tag: Tag): String = {
    val name = target(tag).replace("`", "")
    val text = tag.text.trim.split("\\s+", 2).lift(1).getOrElse("").trim
    val description = text.split("\\s+", 2) match {
      case Array(dash, remainder) if Dashes.contains(dash) => remainder
      case Array(dash) if Dashes.contains(dash)            => ""
      case _                                               => text
    }
    s"- `$name`${separated(tagText(description))}"
  }

  private def target(tag: Tag): String =
    tag.text.trim.split("\\s+", 2).headOption.getOrElse("").stripSuffix(":")

  private def separated(text: String): String = if (text.isEmpty) "" else s" — $text"

  private def tagText(text: String): String = {
    val collapsed = text.split("\\s+").filter(_.nonEmpty).mkString(" ")
    if (collapsed.length <= MaxTagChars) inline(collapsed)
    else inline(truncate(collapsed, MaxTagChars)) + " " + Ellipsis
  }

  /* Joins sections, keeping whole lines until the length limit, and closes any fence left open */
  private def assemble(sections: Seq[Seq[String]]): Option[String] = {
    val out                  = ArrayBuffer[String]()
    var length               = 0
    var truncated            = false
    var fence: Option[Fence] = None
    sections.foreach(section => {
      val lines = if (out.isEmpty) section else "" +: section
      lines.foreach(line => {
        if (!truncated) {
          if (length + line.length + 1 > MaxLength) {
            truncated = true
          } else {
            out += line
            length += line.length + 1
            fence = updateFence(fence, line)
          }
        }
      })
    })
    fence.foreach(f => out += f.close)
    if (truncated) out += Ellipsis
    Some(out.mkString("\n").trim).filter(_.nonEmpty)
  }

  /* Escapes lines outside fences and closes a fence left open, so nothing can leak into what follows */
  private def renderBlock(lines: Seq[String]): Seq[String] = {
    var fence: Option[Fence] = None
    val out = lines.map(line => {
      val next = updateFence(fence, line)
      val rendered =
        if (next != fence) line.trim
        else if (fence.nonEmpty) line
        else escapeLine(line)
      fence = next
      rendered
    })
    fence.map(f => out :+ f.close).getOrElse(out)
  }

  private def escapeLine(line: String): String = {
    val indent = line.takeWhile(_.isWhitespace)
    val body   = line.drop(indent.length)
    indent + (if (body.startsWith("#")) "\\" else "") + inline(body)
  }

  /* Renders inline tags and neutralises HTML, leaving other markdown in place */
  private[cst] def inline(text: String): String = {
    val out = new StringBuilder
    var i   = 0
    while (i < text.length) {
      val c = text.charAt(i)
      if (c == '\\' && i + 1 < text.length && isAsciiPunctuation(text.charAt(i + 1))) {
        out.append(if (text.charAt(i + 1) == '<') "&lt;" else text.substring(i, i + 2))
        i += 2
      } else if (c == '`') {
        val run = text.indexWhere(_ != '`', i) match { case -1 => text.length - i; case j => j - i }
        val ticks = "`" * run
        val close = findRun(text, ticks, i + run)
        if (close >= 0) {
          out.append(text.substring(i, close + run))
          i = close + run
        } else {
          out.append("\\`" * run)
          i += run
        }
      } else if (text.startsWith("{@", i) && matchingBrace(text, i) > 0) {
        val end  = matchingBrace(text, i)
        val body = text.substring(i + 2, end)
        val name = body.takeWhile(ch => !ch.isWhitespace)
        val arg  = body.drop(name.length).trim
        name.toLowerCase match {
          case "code"                              => out.append(codeSpan(arg))
          case tag if InlineTextTags.contains(tag) => out.append(escapeText(arg))
          case _ => out.append(escapeText(text.substring(i, end + 1)))
        }
        i = end + 1
      } else if (c == '<') {
        out.append("&lt;")
        i += 1
      } else {
        out.append(c)
        i += 1
      }
    }
    out.toString
  }

  /* Position of a backtick run of exactly the same length, as markdown requires to close a span */
  private def isAsciiPunctuation(c: Char): Boolean =
    c < 128 && !c.isLetterOrDigit && !c.isWhitespace && !c.isControl

  private def findRun(text: String, ticks: String, from: Int): Int = {
    var j = text.indexOf(ticks, from)
    while (j >= 0) {
      val end = text.indexWhere(_ != '`', j) match { case -1 => text.length; case k => k }
      if (end - j == ticks.length) return j
      j = text.indexOf(ticks, end)
    }
    -1
  }

  private def matchingBrace(text: String, start: Int): Int = {
    var depth = 0
    var i     = start
    while (i < text.length) {
      text.charAt(i) match {
        case '{' => depth += 1
        case '}' =>
          depth -= 1
          if (depth == 0) return i
        case _ =>
      }
      i += 1
    }
    -1
  }

  private def codeSpan(code: String): String = {
    if (code.isEmpty) ""
    else if (!code.contains('`')) s"`$code`"
    else {
      val ticks = "`" * ("`+".r.findAllIn(code).map(_.length).max + 1)
      s"$ticks $code $ticks"
    }
  }

  private def escapeText(text: String): String =
    text.flatMap {
      case '<'                                  => "&lt;"
      case c if MarkdownSpecial.indexOf(c) >= 0 => s"\\$c"
      case c                                    => c.toString
    }

  private final case class Fence(char: Char, length: Int) {
    def close: String = char.toString * length
  }

  private def openingFence(line: String): Option[Fence] = {
    val trimmed = line.trim
    trimmed.headOption
      .filter(c => c == '`' || c == '~')
      .flatMap(c => {
        val length = trimmed.takeWhile(_ == c).length
        if (length >= 3 && !(c == '`' && trimmed.drop(length).contains('`'))) Some(Fence(c, length))
        else None
      })
  }

  private def updateFence(fence: Option[Fence], line: String): Option[Fence] = {
    fence match {
      case Some(f) =>
        val trimmed = line.trim
        if (trimmed.length >= f.length && trimmed.forall(_ == f.char)) None else fence
      case None => openingFence(line)
    }
  }

  /* Takes whole lines up to the limits, cutting an over-long first line at a word boundary */
  private def cap(lines: Seq[String], maxLines: Int, maxChars: Int): (Seq[String], Boolean) = {
    val kept      = ArrayBuffer[String]()
    var chars     = 0
    var truncated = false
    lines.foreach(line => {
      if (!truncated && kept.size < maxLines && chars + line.length <= maxChars) {
        kept += line
        chars += line.length + 1
      } else if (!truncated && kept.isEmpty) {
        kept += truncate(line, maxChars)
        truncated = true
      } else {
        truncated = true
      }
    })
    (trimBlank(kept.toSeq), truncated)
  }

  /* Cuts at the last word boundary that keeps some text, else hard, never splitting a surrogate pair */
  private def truncate(text: String, max: Int): String = {
    if (text.length <= max) text
    else {
      val space = text.lastIndexWhere(_.isWhitespace, max)
      if (space > 0 && text.substring(0, space).trim.nonEmpty)
        text.substring(0, space).trim
      else {
        val cut = if (Character.isLowSurrogate(text.charAt(max))) max - 1 else max
        text.substring(0, cut).trim
      }
    }
  }

  private def collapseBlank(lines: Seq[String]): Seq[String] = {
    var fence: Option[Fence] = None
    var previousBlank        = false
    lines.filter(line => {
      val blank = fence.isEmpty && line.trim.isEmpty
      val keep  = !(blank && previousBlank)
      previousBlank = blank
      fence = updateFence(fence, line)
      keep
    })
  }

  private def trimBlank(lines: Seq[String]): Seq[String] =
    lines.dropWhile(_.trim.isEmpty).reverse.dropWhile(_.trim.isEmpty).reverse
}
