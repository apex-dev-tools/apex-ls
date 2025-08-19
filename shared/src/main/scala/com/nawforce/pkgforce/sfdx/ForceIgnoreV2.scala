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
package com.nawforce.pkgforce.sfdx

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.platform.{Environment, Path}

import scala.collection.compat.immutable.ArraySeq
import scala.util.matching.Regex

/** ForceIgnoreV2 - node-ignore compatible implementation */
class ForceIgnoreV2(rootPath: PathLike, rules: Seq[IgnoreRuleV2]) {

  /** Salesforce CLI default ignore patterns - always applied */
  private val DEFAULT_PATTERNS =
    Seq("**/*.dup", "**/.*", "**/package2-descriptor.json", "**/package2-manifest.json")

  private val allRules = DEFAULT_PATTERNS.map(IgnoreRuleV2.fromPattern) ++ rules

  // Cache for parent directory test results to avoid repeated calculations
  private val parentDirCache = scala.collection.mutable.Map[String, Boolean]()
  
  // Cache root path string and length for performance
  private val rootStr = rootPath.toString
  private val rootStrLength = rootStr.length
  
  // Cache platform-specific constants to avoid repeated lookups
  private val pathSeparator = Path.separator
  private val isWindows = Environment.isWindows

  def includeDirectory(path: PathLike): Boolean = {
    include(path, isDirectory = true)
  }

  def includeFile(path: PathLike): Boolean = {
    include(path, isDirectory = false)
  }

  private def include(path: PathLike, isDirectory: Boolean): Boolean = {
    // Calculate relative path from root to target and check if path is under root in one step
    val relativePath = if (path == rootPath) {
      ""
    } else {
      // Since PathLike stores normalized absolute paths, we need to compute the relative path
      val pathStr = path.toString
      if (pathStr.startsWith(rootStr)) {
        val suffix = pathStr.substring(rootStrLength)
        val relative =
          if (suffix.startsWith(pathSeparator)) suffix.substring(pathSeparator.length) else suffix
        // Normalize path separators to forward slashes for gitignore compatibility (Windows only)
        if (isWindows) relative.replace('\\', '/') else relative
      } else {
        // Path is not under root, so include it
        return true
      }
    }

    // Check parent directories first (node-ignore _t method logic)
    // > It is not possible to re-include a file if a parent directory of that file is excluded
    if (relativePath.nonEmpty && relativePath.contains('/')) {
      val parts = relativePath.split('/')
      if (parts.length > 1) { // Only proceed if there are actually parent directories
        var currentPath = ""

        // Check each parent directory (except the last part which is the file/dir itself)
        for (i <- 0 until parts.length - 1) {
          currentPath = if (currentPath.isEmpty) parts(i) else currentPath + "/" + parts(i)
          val parentDirPath = currentPath + "/"

          // Check cache first, then compute and cache result
          val isParentIgnored = parentDirCache.getOrElseUpdate(
            parentDirPath, {
              !testPath(parentDirPath, isDirectory = true)
            }
          )

          if (isParentIgnored) {
            // Parent directory is ignored, so this path is also ignored
            return false
          }
        }
      }
    }

    // Now test the path itself
    testPath(relativePath, isDirectory)
  }

  private def testPath(relativePath: String, isDirectory: Boolean): Boolean = {
    // Apply rules using node-ignore 5.3.2 exact logic from _testOne method
    var ignored   = false
    var unignored = false
    // checkUnignored = false is constant, so we can inline the optimization

    var i = 0
    val rulesLength = allRules.length
    while (i < rulesLength) {
      val rule = allRules(i)
      val negative = rule.negated

      // node-ignore optimization logic from _testOne (with checkUnignored=false inlined):
      val shouldSkip = (unignored == negative && ignored != unignored) ||
        (negative && !ignored && !unignored)

      if (!shouldSkip) {
        val matched = rule.matches(relativePath)
        if (matched) {
          ignored = !negative
          unignored = negative
        }
      }
      i += 1
    }

    !ignored
  }
}

object ForceIgnoreV2 {
  def apply(path: PathLike): IssuesAnd[Option[ForceIgnoreV2]] = {
    if (path.isFile) {
      path.read() match {
        case Left(err) =>
          IssuesAnd(ArraySeq(Issue(path, Diagnostic(ERROR_CATEGORY, Location.empty, err))), None)
        case Right(data) =>
          val rules = IgnoreRuleV2.parseRules(data)
          IssuesAnd(ArraySeq(), Some(new ForceIgnoreV2(path.parent, rules)))
      }
    } else {
      IssuesAnd(None)
    }
  }
}

/** Node-ignore compatible rule implementation */
case class IgnoreRuleV2(pattern: String, negated: Boolean, directoryOnly: Boolean) {

  // Convert gitignore pattern to regex - EXACT port of node-ignore makeRegex
  private lazy val regex: Regex = {
    val regexPattern = NodeIgnoreExact.makeRegex(pattern)
    // node-ignore defaults to case-insensitive, which Salesforce CLI uses
    s"(?i)$regexPattern".r // (?i) flag for case-insensitive matching
  }

  def matches(relativePath: String): Boolean = {
    regex.matches(relativePath)
  }
}

object IgnoreRuleV2 {

  def fromPattern(pattern: String): IgnoreRuleV2 = {
    var p       = pattern
    val negated = p.startsWith("!")
    if (negated) {
      p = p.substring(1)
    }

    // Handle escaped ! and # at the beginning (from node-ignore createRule)
    p = p.replaceFirst("^\\\\!", "!")
    p = p.replaceFirst("^\\\\#", "#")

    val directoryOnly = p.endsWith("/")
    // node-ignore doesn't transform directory patterns, it just marks them
    IgnoreRuleV2(p, negated, directoryOnly)
  }

  def parseRules(content: String): Seq[IgnoreRuleV2] = {
    // Split on \r?\n like node-ignore REGEX_SPLITALL_CRLF
    // Note: node-ignore does NOT trim lines
    val lines = content.split("\\r?\\n")

    lines
      .filter { pattern =>
        // checkPattern logic from node-ignore
        pattern.nonEmpty &&
        !pattern.matches("^\\s+$") &&             // REGEX_TEST_BLANK_LINE
        !pattern.matches("(?:[^\\\\]|^)\\\\$") && // REGEX_INVALID_TRAILING_BACKSLASH
        !pattern.startsWith("#")                  // Comments
      }
      .map(fromPattern)
      .toIndexedSeq
  }
}
