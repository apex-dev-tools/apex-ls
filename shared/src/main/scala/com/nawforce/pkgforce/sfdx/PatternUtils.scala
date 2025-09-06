/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.sfdx

/** Utility functions to replace node-ignore regex patterns that use lookahead/lookbehind
  * These patterns are not compatible with Java/Scala regex engines on both JVM and ScalaJS
  */
object PatternUtils {

  private val ESCAPE = "\\\\"

  /** Replace question marks with [^/] but not escaped question marks
    * Replaces node-ignore pattern: (?!\\)\? with [^/]
    */
  def replaceQuestionMarks(source: String): String = {
    val result = new StringBuilder()
    var i      = 0

    while (i < source.length) {
      val char = source.charAt(i)

      if (char == '?') {
        // Check if immediately preceded by single backslash (escaped in regex terms)
        if (i > 0 && source.charAt(i - 1) == '\\') {
          // Already escaped for regex, keep as is (literal ?)
          result.append(char)
        } else {
          // Non-escaped ?, replace with [^/]
          result.append("[^/]")
        }
      } else {
        result.append(char)
      }
      i += 1
    }

    result.toString
  }

  /** Add starting anchor based on slash detection
    * Replaces node-ignore lookahead pattern with conditional logic
    */
  def addStartingAnchor(source: String, originalPattern: String): String = {
    if (source.startsWith("^")) {
      source // Already has anchor
    } else {
      // Check if ORIGINAL pattern has slash not just at the end
      // !/\/(?!$)/.test(this) means: does NOT have slash except possibly at end
      val hasSlashNotJustAtEnd = originalPattern.contains("/") &&
        !originalPattern.matches(".*[^/]/$") &&
        !originalPattern.matches("[^/]*$")

      if (!hasSlashNotJustAtEnd) {
        // For patterns without slashes, node-ignore uses (?:^|\/)
        s"^(?:.*\\/)?$source"
      } else {
        s"^$source"
      }
    }
  }

  /** Replace two globstars double-star patterns
    * Replaces node-ignore globstar pattern with lookahead
    */
  def replaceTwoGlobstars(source: String): String = {
    // Handle the exact patterns we see from NodeIgnoreExact after metacharacter escaping
    var result = source

    // Pattern 1: /** at end -> /.+ (must come before /**/ pattern)
    if (result.contains("\\/\\*\\*") && result.endsWith("\\/\\*\\*")) {
      result = result.replace("\\/\\*\\*", "\\/.+")
    }
    // Pattern 2: /**/ in middle -> (?:\/[^\/]+)*\/
    else if (result.contains("\\/\\*\\*\\/")) {
      result = result.replace("\\/\\*\\*\\/", "(?:\\/[^\\/]+)*\\/")
    }

    // Pattern 3: ^\*\*\/ -> ^(?:.*\/)?
    if (result.startsWith("^\\*\\*\\/")) {
      result = result.replace("^\\*\\*\\/", "^(?:.*\\/)?")
    }

    result
  }

  /** Replace intermediate wildcards with lookahead logic
    * Replaces node-ignore wildcard pattern with lookahead
    */
  def replaceIntermediateWildcards(source: String): String = {
    val result = new StringBuilder()
    var i      = 0

    while (i < source.length) {
      if (i + 1 < source.length && source.charAt(i) == '\\' && source.charAt(i + 1) == '*') {
        // Found \*, check if more content follows (equivalent to lookahead (?=.+))
        var starCount = 0
        var j         = i

        // Count consecutive \* patterns
        while (j + 1 < source.length && source.charAt(j) == '\\' && source.charAt(j + 1) == '*') {
          starCount += 1
          j += 2
        }

        // Check if there's more content after the stars (lookahead equivalent)
        if (j < source.length) {
          // Replace \* with [^/]*
          for (_ <- 0 until starCount) {
            result.append("[^\\/]*")
          }
          i = j
        } else {
          // No more content, don't replace (this will be handled by trailing wildcard)
          result.append(source.charAt(i))
          i += 1
        }
      } else {
        result.append(source.charAt(i))
        i += 1
      }
    }

    result.toString
  }

  /** Replace unescape pattern with lookahead logic
    * Replaces node-ignore backslash escape pattern with lookahead
    */
  def replaceUnescapePattern(source: String): String = {
    val result         = new StringBuilder()
    var i              = 0
    val metacharacters = Set('$', '.', '|', '*', '+', '(', ')', '{', '^')

    while (i < source.length) {
      // Look for \\\\\\  (6 backslashes, which is 3 escaped backslashes)
      if (
        i + 5 < source.length &&
        source.substring(i, i + 6) == "\\\\\\\\\\\\"
      ) {

        // Check if followed by metacharacter (lookahead equivalent)
        if (i + 6 < source.length && metacharacters.contains(source.charAt(i + 6))) {
          result.append(ESCAPE)
          result.append(source.charAt(i + 6))
          i += 7
        } else {
          result.append(source.charAt(i))
          i += 1
        }
      } else {
        result.append(source.charAt(i))
        i += 1
      }
    }

    result.toString
  }

  /** Add ending pattern with conditional logic
    * Replaces node-ignore ending pattern with replacement function logic
    */
  def addEndingPattern(source: String): String = {
    if (source.nonEmpty && source.last != '*') {
      // For file patterns, just add end anchor
      source + "$"
    } else {
      source
    }
  }

  /** Handle trailing wildcard pattern
    * Replaces node-ignore trailing wildcard pattern
    */
  def replaceTrailingWildcard(source: String): String = {
    if (source.endsWith("\\*")) {
      val beforeStar = source.dropRight(2) // Remove \*

      if (beforeStar.endsWith("^")) {
        // ^* -> ^[^/]+(?=$|\/)
        beforeStar + "[^/]+(?=$|\\/)"
      } else if (beforeStar.endsWith("\\/")) {
        // /*-> /[^/]+(?=$|\/)
        beforeStar + "[^/]+(?=$|\\/)"
      } else {
        // * -> [^/]*(?=$|\/)
        beforeStar + "[^/]*(?=$|\\/)"
      }
    } else {
      source
    }
  }
}
