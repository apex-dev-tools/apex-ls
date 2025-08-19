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
import com.nawforce.runtime.platform.Path

import scala.collection.compat.immutable.ArraySeq
import scala.util.matching.Regex

/** ForceIgnoreV2 - node-ignore compatible implementation */
class ForceIgnoreV2(rootPath: PathLike, rules: Seq[IgnoreRuleV2]) {
  
  /** Salesforce CLI default ignore patterns - always applied */
  private val DEFAULT_PATTERNS = Seq(
    "**/*.dup",
    "**/.*", 
    "**/package2-descriptor.json",
    "**/package2-manifest.json"
  )
  
  private val allRules = DEFAULT_PATTERNS.map(IgnoreRuleV2.fromPattern) ++ rules

  def includeDirectory(path: PathLike): Boolean = {
    include(path, isDirectory = true)
  }

  def includeFile(path: PathLike): Boolean = {
    include(path, isDirectory = false)
  }

  private def include(path: PathLike, isDirectory: Boolean): Boolean = {
    if (!rootPath.isParentOf(path) && path != rootPath)
      return true

    // Calculate relative path from root to target
    val relativePath = if (path == rootPath) {
      ""
    } else {
      // Since PathLike stores normalized absolute paths, we need to compute the relative path
      val rootStr = rootPath.toString
      val pathStr = path.toString
      val relative = if (pathStr.startsWith(rootStr)) {
        val suffix = pathStr.substring(rootStr.length)
        if (suffix.startsWith(Path.separator)) suffix.substring(Path.separator.length) else suffix
      } else {
        pathStr  // Fallback if not actually a child
      }
      // Normalize path separators to forward slashes for gitignore compatibility
      relative.replace('\\', '/')
    }
    
    // Check parent directories first (node-ignore _t method logic)
    // > It is not possible to re-include a file if a parent directory of that file is excluded
    if (relativePath.contains("/")) {
      val parts = relativePath.split("/")
      var currentPath = ""
      
      // Check each parent directory (except the last part which is the file/dir itself)
      for (i <- 0 until parts.length - 1) {
        currentPath = if (currentPath.isEmpty) parts(i) else currentPath + "/" + parts(i)
        
        // Test if this parent directory is ignored
        if (!testPath(currentPath + "/", true)) {
          // Parent directory is ignored, so this path is also ignored
          return false
        }
      }
    }
    
    // Now test the path itself
    testPath(relativePath, isDirectory)
  }
  
  private def testPath(relativePath: String, isDirectory: Boolean): Boolean = {
    // Apply rules using node-ignore 5.3.2 exact logic from _testOne method
    var ignored = false
    var unignored = false
    val checkUnignored = false  // We always call with false like ignores() method
    
    allRules.foreach { rule =>
      val negative = rule.negated
      
      // node-ignore optimization logic from _testOne:
      val shouldSkip = (unignored == negative && ignored != unignored) ||
                       (negative && !ignored && !unignored && !checkUnignored)
      
      if (!shouldSkip) {
        val matched = rule.matches(relativePath, isDirectory)
        if (matched) {
          ignored = !negative
          unignored = negative
        }
      }
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
  
  private val EMPTY = ""
  
  // Convert gitignore pattern to regex - EXACT port of node-ignore makeRegex
  private lazy val regex: Regex = {
    val regexPattern = NodeIgnoreExact.makeRegex(pattern, ignoreCase = true)
    // node-ignore defaults to case-insensitive, which Salesforce CLI uses
    s"(?i)$regexPattern".r // (?i) flag for case-insensitive matching
  }
  
  def matches(relativePath: String, isDirectory: Boolean): Boolean = {
    regex.matches(relativePath)
  }
}

object IgnoreRuleV2 {
  
  def fromPattern(pattern: String): IgnoreRuleV2 = {
    var p = pattern
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
        !pattern.matches("^\\s+$") && // REGEX_TEST_BLANK_LINE
        !pattern.matches("(?:[^\\\\]|^)\\\\$") && // REGEX_INVALID_TRAILING_BACKSLASH
        !pattern.startsWith("#") // Comments
      }
      .map(fromPattern)
      .toIndexedSeq
  }
}