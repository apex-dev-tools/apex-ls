/*
 * Copyright (c) 2025
 */
package com.nawforce.apexlink.hover

import com.nawforce.pkgforce.path.Location

import java.util.Locale
import scala.collection.mutable.ListBuffer

/** Parse ApexDoc comment blocks and render compliant Markdown segments. */
object ApexDocFormatter {

  def format(source: String, declarationLocation: Location): Option[String] = {
    val comment = extractComment(source, declarationLocation).getOrElse(return None)
    val lines   = prepareLines(comment)
    val parsed  = parse(lines)

    buildMarkdown(parsed)
  }

  private final case class ParsedDoc(
    summary: Seq[String],
    params: Seq[ParamTag],
    returnsTag: Option[ReturnTag],
    throwsTags: Seq[ThrowsTag],
    seeAlso: Seq[String]
  )

  private sealed trait DocTag {
    protected val builder: StringBuilder = new StringBuilder

    def append(text: String): Unit = {
      val sanitized = MarkdownSanitizer.sanitize(text).trim
      if (sanitized.nonEmpty) {
        if (builder.nonEmpty) {
          builder.append(' ')
        }
        builder.append(sanitized)
      }
    }

    def hasContent: Boolean = builder.nonEmpty

    def content: String = MarkdownSanitizer.collapseWhitespace(builder.toString)
  }

  private final case class ParamTag(rawName: String) extends DocTag {
    private val sanitizedName = MarkdownSanitizer.sanitizeAndCollapse(rawName)

    def name: String = sanitizedName
  }

  private final case class ReturnTag() extends DocTag

  private final case class ThrowsTag(rawType: String) extends DocTag {
    private val sanitizedType = MarkdownSanitizer.sanitizeAndCollapse(rawType)

    def exceptionType: String = sanitizedType
  }

  private final case class SeeTag() extends DocTag

  private def extractComment(source: String, location: Location): Option[String] = {
    val startIndex = indexForLocation(source, location.startLine, location.startPosition)
    if (startIndex <= 0) {
      return None
    }

    val prefix      = source.substring(0, Math.min(startIndex, source.length))
    val commentEnd  = prefix.lastIndexOf("*/")
    val commentStart = if (commentEnd >= 0) prefix.lastIndexOf("/**", commentEnd) else -1

    if (commentStart < 0 || commentEnd < 0 || commentStart >= commentEnd) {
      None
    } else {
      val trailing       = prefix.substring(commentEnd + 2)
      val lastLineBreak  = trailing.lastIndexOf('\n')
      val betweenComment = if (lastLineBreak >= 0) trailing.substring(0, lastLineBreak).trim else ""
      if (betweenComment.nonEmpty) {
        None
      } else {
        Some(prefix.substring(commentStart, commentEnd + 2))
      }
    }
  }

  private def prepareLines(comment: String): Seq[String] = {
    val body = comment.substring(3, comment.length - 2)
    body
      .replace("\r", "")
      .split("\n", -1)
      .iterator
      .map(line => leadingPattern.replaceFirstIn(line, ""))
      .toVector
  }

  private def parse(lines: Seq[String]): ParsedDoc = {
    val summaryParagraphs = ListBuffer[String]()
    val params            = ListBuffer[ParamTag]()
    var returnTag: Option[ReturnTag] = None
    val throwsTags        = ListBuffer[ThrowsTag]()
    val seeTags           = ListBuffer[SeeTag]()

    val paragraphBuilder = new StringBuilder
    var currentTag: Option[DocTag] = None

    lines.foreach { rawLine =>
      val line = rawLine.trim
      if (line.startsWith("@")) {
        if (currentTag.isEmpty && paragraphBuilder.nonEmpty) {
          summaryParagraphs += MarkdownSanitizer.collapseWhitespace(paragraphBuilder.toString)
          paragraphBuilder.clear()
        }
        val (tagName, rest) = splitTag(line)
        currentTag = tagName match {
          case "param" =>
            val pieces = rest.split("\\s+", 2)
            if (pieces.nonEmpty && pieces(0).nonEmpty) {
              val tag = ParamTag(pieces(0))
              if (pieces.length > 1) tag.append(pieces(1))
              params += tag
              Some(tag)
            } else {
              None
            }
          case "return" =>
            val tag = ReturnTag()
            if (rest.nonEmpty) tag.append(rest)
            returnTag = Some(tag)
            Some(tag)
          case "throws" =>
            val pieces = rest.split("\\s+", 2)
            if (pieces.nonEmpty && pieces(0).nonEmpty) {
              val tag = ThrowsTag(pieces(0))
              if (pieces.length > 1) tag.append(pieces(1))
              throwsTags += tag
              Some(tag)
            } else {
              None
            }
          case "see" =>
            val tag = SeeTag()
            if (rest.nonEmpty) tag.append(rest)
            seeTags += tag
            Some(tag)
          case _ =>
            None
        }
      } else {
        val sanitized = MarkdownSanitizer.sanitize(line).trim
        if (currentTag.isDefined) {
          currentTag.foreach(_.append(line))
        } else if (sanitized.isEmpty) {
          if (paragraphBuilder.nonEmpty) {
            summaryParagraphs += MarkdownSanitizer.collapseWhitespace(paragraphBuilder.toString)
            paragraphBuilder.clear()
          }
        } else {
          if (paragraphBuilder.nonEmpty) {
            paragraphBuilder.append(' ')
          }
          paragraphBuilder.append(sanitized)
        }
      }
    }

    if (currentTag.isEmpty && paragraphBuilder.nonEmpty) {
      summaryParagraphs += MarkdownSanitizer.collapseWhitespace(paragraphBuilder.toString)
    }

    val validParams = params.filter(_.hasContent)
    val validReturn = returnTag.filter(_.hasContent)
    val validThrows = throwsTags.filter(tag => tag.exceptionType.nonEmpty && tag.hasContent)
    val validSee    = seeTags.flatMap(tag => formatSee(tag.content))

    ParsedDoc(summaryParagraphs.toSeq, validParams.toSeq, validReturn, validThrows.toSeq, validSee.toSeq)
  }

  private def buildMarkdown(doc: ParsedDoc): Option[String] = {
    if (doc.summary.isEmpty && doc.params.isEmpty && doc.returnsTag.isEmpty && doc.throwsTags.isEmpty && doc.seeAlso.isEmpty) {
      None
    } else {
      val builder = new StringBuilder

      def appendSection(title: String)(body: => String): Unit = {
        val rendered = body.trim
        if (rendered.nonEmpty) {
          if (builder.nonEmpty) {
            builder.append('\n')
          }
          builder.append("**").append(title).append("**\n").append(rendered)
        }
      }

      appendSection("Summary") {
        doc.summary.mkString("\n\n")
      }

      appendSection("Parameters") {
        doc.params
          .flatMap { param =>
            if (!param.hasContent || param.name.isEmpty) {
              None
            } else {
              Some(s"- `${param.name}` — ${param.content}")
            }
          }
          .mkString("\n")
      }

      appendSection("Returns") {
        doc.returnsTag.map(_.content).getOrElse("")
      }

      appendSection("Throws") {
        doc.throwsTags
          .flatMap { tag =>
            if (tag.exceptionType.isEmpty || !tag.hasContent) {
              None
            } else {
              Some(s"- ${tag.exceptionType} — ${tag.content}")
            }
          }
          .mkString("\n")
      }

      appendSection("See Also") {
        doc.seeAlso.mkString("\n")
      }

      Some(builder.toString)
    }
  }

  private def splitTag(line: String): (String, String) = {
    val trimmed = line.drop(1)
    val space   = trimmed.indexWhere(_.isWhitespace)
    if (space == -1) {
      (trimmed.toLowerCase(Locale.ROOT), "")
    } else {
      (
        trimmed.substring(0, space).toLowerCase(Locale.ROOT),
        trimmed.substring(space).trim
      )
    }
  }

  private def formatSee(raw: String): Option[String] = {
    val sanitized = MarkdownSanitizer.sanitize(raw).trim
    if (sanitized.isEmpty) {
      None
    } else {
      sanitized match {
        case linkPattern(text, url) =>
          Some(s"[${MarkdownSanitizer.escapeForLinkLabel(text.trim)}](${url.trim})")
        case _ =>
          val parts = sanitized.split("\\s+", 2)
          val first = parts.headOption.getOrElse("")
          if (isUrl(first)) {
            val label = parts.lift(1).map(_.trim).filter(_.nonEmpty).getOrElse(first)
            Some(s"[${MarkdownSanitizer.escapeForLinkLabel(label)}](${first})")
          } else {
            Some(sanitized)
          }
      }
    }
  }

  private def indexForLocation(source: String, line: Int, offset: Int): Int = {
    if (line <= 1) {
      offset
    } else {
      var idx         = 0
      var currentLine = 1
      val length      = source.length
      while (idx < length && currentLine < line) {
        if (source.charAt(idx) == '\n') {
          currentLine += 1
        }
        idx += 1
      }
      Math.min(idx, length) + math.max(offset, 0)
    }
  }

  private val leadingPattern = "^\\s*\\*?\\s?".r
  private val linkPattern    = "^\\[(.+?)\\]\\((.+?)\\)$".r

  private def isUrl(value: String): Boolean = {
    val lower = value.toLowerCase(Locale.ROOT)
    lower.startsWith("http://") || lower.startsWith("https://")
  }
}

