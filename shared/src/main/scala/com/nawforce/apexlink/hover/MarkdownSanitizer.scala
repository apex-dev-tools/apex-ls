/*
 * Copyright (c) 2025
 */
package com.nawforce.apexlink.hover

/** Utility helpers for preparing ApexDoc fragments for safe Markdown rendering. */
object MarkdownSanitizer {

  private val htmlTagPattern     = "(?s)<[^>]*>".r
  private val controlCharPattern = "[\\u0000-\\u001F&&[^\\n\\t]]".r

  /** Remove HTML tags and control characters while leaving Markdown intact. */
  def sanitize(text: String): String = {
    if (text == null) {
      ""
    } else {
      val noTags     = htmlTagPattern.replaceAllIn(text, "")
      val noControls = controlCharPattern.replaceAllIn(noTags, "")
      noControls
    }
  }

  /** Collapse runs of whitespace into a single space after sanitization. */
  def sanitizeAndCollapse(text: String): String = {
    collapseWhitespace(sanitize(text))
  }

  /** Collapse runs of whitespace into a single space. */
  def collapseWhitespace(text: String): String = {
    text.split("\\s+").filter(_.nonEmpty).mkString(" ")
  }

  /** Escape Markdown-sensitive characters used in link labels. */
  def escapeForLinkLabel(text: String): String = {
    text
      .replace("\\", "\\\\")
      .replace("[", "\\[")
      .replace("]", "\\]")
      .replace("`", "\\`")
  }
}

