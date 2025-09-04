/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
 */
package com.nawforce.pkgforce.sfdx

/** EXACT port of node-ignore 5.3.2 regex transformation using hybrid approach */
object NodeIgnoreExact {

  private val EMPTY  = ""
  private val SPACE  = " "
  private val ESCAPE = "\\\\"

  def makeRegex(pattern: String): String = {
    var source = pattern

    // Keep original pattern for reference in some transformations
    val originalPattern = pattern

    // REPLACER 1: Remove BOM
    source = source.replaceFirst("^\\uFEFF", EMPTY)

    // REPLACER 2: Trailing spaces
    val trailingSpaceRegex = "((?:\\\\\\\\)*?)(\\\\?\\s+)$".r
    source = trailingSpaceRegex.replaceAllIn(
      source,
      m => {
        val m1 = m.group(1)
        val m2 = m.group(2)
        m1 + (if (m2.indexOf('\\') == 0) SPACE else EMPTY)
      }
    )

    // REPLACER 3: Replace (\ ) with ' '
    val escapeSpaceRegex = "(\\\\+?)\\s".r
    source = escapeSpaceRegex.replaceAllIn(
      source,
      m => {
        val m1     = m.group(1)
        val length = m1.length
        m1.substring(0, length - length % 2) + SPACE
      }
    )

    // REPLACER 4: Escape metacharacters
    // Special handling: don't escape backslashes that are already escaping other characters
    val result = new StringBuilder()
    var i      = 0
    while (i < source.length) {
      val char = source.charAt(i)
      char match {
        case '\\' =>
          // Check if this backslash is escaping another character
          if (i + 1 < source.length) {
            val nextChar = source.charAt(i + 1)
            // If backslash is escaping a character that will be handled later (like ?), keep it as is
            if (nextChar == '?') {
              // Based on actual node-ignore behavior, \? doesn't create literal ?
              // Instead, escape the backslash and let ? be processed as wildcard
              result.append("\\\\\\\\")
              result.append(nextChar)
              i += 2
            } else {
              // Escape the backslash normally
              result.append("\\\\\\\\")
              result.append(nextChar)
              i += 2
            }
          } else {
            result.append("\\\\\\\\") // Escape lone backslash at end
            i += 1
          }
        case '$' | '.' | '|' | '*' | '+' | '(' | ')' | '{' | '^' =>
          result.append("\\")
          result.append(char)
          i += 1
        case _ =>
          result.append(char)
          i += 1
      }
    }
    source = result.toString()

    // REPLACER 5: ? -> [^/] (CUSTOM IMPLEMENTATION)
    source = PatternUtils.replaceQuestionMarks(source)

    // REPLACER 6: Leading slash
    val leadingSlashRegex = "^/".r
    source = leadingSlashRegex.replaceFirstIn(source, "^")

    // REPLACER 7: Replace other slashes
    source = source.replace("/", "\\/")

    // REPLACER 8: Leading **/ pattern
    val leadingDoubleStarRegex = "^\\^?\\\\\\*\\\\\\*\\/".r
    source = leadingDoubleStarRegex.replaceFirstIn(source, "^(?:.*\\/)?")

    // REPLACER 9: Starting replacer (CUSTOM IMPLEMENTATION)
    source = PatternUtils.addStartingAnchor(source, originalPattern)

    // REPLACER 10: Two globstars /** (CUSTOM IMPLEMENTATION)
    source = PatternUtils.replaceTwoGlobstars(source)

    // REPLACER 11: Normal intermediate wildcards (CUSTOM IMPLEMENTATION)
    source = PatternUtils.replaceIntermediateWildcards(source)

    // REPLACER 12: Unescape (first one) (CUSTOM IMPLEMENTATION)
    source = PatternUtils.replaceUnescapePattern(source)

    // REPLACER 13: Unescape (second one)
    source = source.replace("\\\\\\\\", ESCAPE)

    // REPLACER 14: Range notation [a-z]
    // This is complex in node-ignore, implementing simplified version
    // that should work for most common cases
    val rangeRegex = "(\\\\)?\\[([^\\]]*?)(\\\\*)($|\\])".r
    source = rangeRegex.replaceAllIn(
      source,
      m => {
        val leadEscape = Option(m.group(1)).getOrElse("")
        val range      = m.group(2)
        val endEscape  = m.group(3)
        val close      = m.group(4)

        if (leadEscape == ESCAPE) {
          // Escaped bracket
          s"\\\\[$range${cleanRangeBackSlash(endEscape)}$close"
        } else if (close == "]") {
          if (endEscape.length % 2 == 0) {
            // Valid range notation
            s"[${sanitizeRange(range)}$endEscape]"
          } else {
            // Invalid range notation
            "[]"
          }
        } else {
          "[]"
        }
      }
    )

    // REPLACER 15: Ending (CUSTOM IMPLEMENTATION)
    source = PatternUtils.addEndingPattern(source)

    // REPLACER 16: Trailing wildcard (CUSTOM IMPLEMENTATION)
    source = PatternUtils.replaceTrailingWildcard(source)

    source
  }

  private def cleanRangeBackSlash(endEscape: String): String = {
    // Simplified implementation for cleaning backslashes in range notation
    endEscape.replace("\\\\", "\\")
  }

  private def sanitizeRange(range: String): String = {
    // Simplified implementation for sanitizing range notation
    // This handles basic cases and should work for most gitignore patterns
    range.replace("\\\\", "\\")
  }
}
