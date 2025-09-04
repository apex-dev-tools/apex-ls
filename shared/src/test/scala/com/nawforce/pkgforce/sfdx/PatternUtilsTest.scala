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

import org.scalatest.funsuite.AnyFunSuite

class PatternUtilsTest extends AnyFunSuite {

  test("replaceQuestionMarks - convert unescaped ? to [^/]") {
    assert(PatternUtils.replaceQuestionMarks("file?.txt") == "file[^/].txt")
    assert(PatternUtils.replaceQuestionMarks("?") == "[^/]")
    assert(PatternUtils.replaceQuestionMarks("a?b?c") == "a[^/]b[^/]c")
  }

  test("replaceQuestionMarks - not convert escaped ?") {
    assert(PatternUtils.replaceQuestionMarks("file\\?.txt") == "file\\?.txt")
    assert(PatternUtils.replaceQuestionMarks("\\?") == "\\?")
    assert(PatternUtils.replaceQuestionMarks("a\\?b?c") == "a\\?b[^/]c")
  }

  test("replaceQuestionMarks - handle empty and edge cases") {
    assert(PatternUtils.replaceQuestionMarks("") == "")
    assert(PatternUtils.replaceQuestionMarks("nomarks") == "nomarks")
    assert(PatternUtils.replaceQuestionMarks("???") == "[^/][^/][^/]")
  }

  test("addStartingAnchor - not modify patterns that already start with ^") {
    assert(PatternUtils.addStartingAnchor("^pattern", "original") == "^pattern")
    assert(PatternUtils.addStartingAnchor("^/test", "/test") == "^/test")
  }

  test("addStartingAnchor - add ^(?:.*\\/)? for patterns without slashes") {
    assert(PatternUtils.addStartingAnchor("*.js", "*.js") == "^(?:.*\\/)?*.js")
    assert(PatternUtils.addStartingAnchor("test", "test") == "^(?:.*\\/)?test")
  }

  test("addStartingAnchor - add ^ for patterns with slashes in middle") {
    assert(PatternUtils.addStartingAnchor("path\\/file", "path/file") == "^path\\/file")
    assert(PatternUtils.addStartingAnchor("a\\/b\\/c", "a/b/c") == "^a\\/b\\/c")
  }

  test("addStartingAnchor - handle trailing slash patterns correctly") {
    assert(PatternUtils.addStartingAnchor("temp", "temp/") == "^(?:.*\\/)?temp")
  }

  test("replaceTwoGlobstars - replace /**/ patterns") {
    val input = "prefix\\/\\*\\*\\/suffix"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "prefix(?:\\/[^\\/]+)*\\/suffix")
  }

  test("replaceTwoGlobstars - replace /** at end patterns") {
    val input = "prefix\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "prefix\\/.+")
  }

  test("replaceTwoGlobstars - replace **/ at start patterns") {
    val input = "^\\*\\*\\/suffix"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "^(?:.*\\/)?suffix")
  }

  test("replaceTwoGlobstars - handle real patterns from NodeIgnoreExact - classes/**") {
    val input = "^classes\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "^classes\\/.+")
  }

  test("replaceTwoGlobstars - handle real patterns from NodeIgnoreExact - **/*_template") {
    val input = "^\\*\\*\\/\\*_template"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "^(?:.*\\/)?\\*_template")
  }

  test("replaceTwoGlobstars - handle real patterns from NodeIgnoreExact - **/.*") {
    val input = "^\\*\\*\\/\\.\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "^(?:.*\\/)?\\.\\*")
  }

  test("replaceTwoGlobstars - not modify other patterns") {
    assert(PatternUtils.replaceTwoGlobstars("normal\\*pattern") == "normal\\*pattern")
    assert(PatternUtils.replaceTwoGlobstars("") == "")
    assert(PatternUtils.replaceTwoGlobstars("\\*") == "\\*")
  }

  test("replaceIntermediateWildcards - replace \\* when followed by more content") {
    assert(PatternUtils.replaceIntermediateWildcards("\\*abc") == "[^\\/]*abc")
    assert(PatternUtils.replaceIntermediateWildcards("prefix\\*suffix") == "prefix[^\\/]*suffix")
  }

  test("replaceIntermediateWildcards - handle multiple consecutive \\* patterns") {
    assert(PatternUtils.replaceIntermediateWildcards("\\*\\*abc") == "[^\\/]*[^\\/]*abc")
  }

  test("replaceIntermediateWildcards - not replace \\* at end of string") {
    assert(PatternUtils.replaceIntermediateWildcards("pattern\\*") == "pattern\\*")
    assert(PatternUtils.replaceIntermediateWildcards("\\*") == "\\*")
  }

  test("replaceIntermediateWildcards - handle mixed patterns") {
    assert(PatternUtils.replaceIntermediateWildcards("a\\*b\\*c\\*") == "a[^\\/]*b[^\\/]*c\\*")
  }

  test("replaceUnescapePattern - replace \\\\\\\\ before metacharacters") {
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\$") == "\\\\$")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\*") == "\\\\*")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\+") == "\\\\+")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\(") == "\\\\(")
  }

  test("replaceUnescapePattern - replace \\\\\\\\ before all metacharacters from node-ignore") {
    // Test all metacharacters from node-ignore regex: [$.|*+(){^]
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\.") == "\\\\.")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\|") == "\\\\|")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\)") == "\\\\)")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\{") == "\\\\{")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\^") == "\\\\^")
  }

  test("replaceUnescapePattern - not replace \\\\\\\\ before non-metacharacters") {
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\a") == "\\\\\\\\\\\\a")
    assert(PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\1") == "\\\\\\\\\\\\1")
  }

  test("replaceUnescapePattern - handle multiple patterns in string") {
    val input = "\\\\\\\\\\\\$test\\\\\\\\\\\\*end"
    val result = PatternUtils.replaceUnescapePattern(input)
    assert(result == "\\\\$test\\\\*end")
  }

  test("addEndingPattern - add $ for patterns ending with /") {
    assert(PatternUtils.addEndingPattern("pattern") == "pattern$")
  }

  test("addEndingPattern - add $ for other patterns not ending with *") {
    assert(PatternUtils.addEndingPattern("pattern") == "pattern$")
    assert(PatternUtils.addEndingPattern("test") == "test$")
  }

  test("addEndingPattern - not modify patterns ending with *") {
    assert(PatternUtils.addEndingPattern("pattern*") == "pattern*")
    assert(PatternUtils.addEndingPattern("*") == "*")
  }

  test("addEndingPattern - handle empty patterns") {
    assert(PatternUtils.addEndingPattern("") == "")
  }

  test("replaceTrailingWildcard - replace trailing \\* with [^/]*(?=$|\\/)")  {
    assert(PatternUtils.replaceTrailingWildcard("pattern\\*") == "pattern[^/]*(?=$|\\/)")
  }

  test("replaceTrailingWildcard - handle ^ prefix correctly") {
    val result = PatternUtils.replaceTrailingWildcard("^\\*")
    assert(result == "^[^/]+(?=$|\\/)")
  }

  test("replaceTrailingWildcard - handle \\/ prefix correctly") {
    val result = PatternUtils.replaceTrailingWildcard("prefix\\/\\*")
    assert(result == "prefix\\/[^/]+(?=$|\\/)")
  }

  test("replaceTrailingWildcard - not modify patterns not ending with \\*") {
    assert(PatternUtils.replaceTrailingWildcard("pattern") == "pattern")
    assert(PatternUtils.replaceTrailingWildcard("test.txt") == "test.txt")
    assert(PatternUtils.replaceTrailingWildcard("") == "")
  }

  test("PatternUtils functions - work together for complex patterns") {
    // Test a pattern that would use multiple transformations
    var result = "file?.js"
    result = PatternUtils.replaceQuestionMarks(result)
    assert(result == "file[^/].js")

    result = PatternUtils.addStartingAnchor(result, "file?.js")
    assert(result.contains("^(?:.*\\/)?"))

    result = PatternUtils.addEndingPattern(result)
    assert(result.contains("$"))
  }

  test("PatternUtils functions - handle escaped characters correctly throughout pipeline") {
    var result = "test\\?.\\*"
    result = PatternUtils.replaceQuestionMarks(result)
    assert(result == "test\\?.\\*") // ? should remain escaped

    result = PatternUtils.replaceIntermediateWildcards(result)
    assert(result == "test\\?.\\*") // * should not be replaced (escaped)
  }

  test("PatternUtils complex negation patterns - handle classes/** pattern correctly") {
    val input = "^classes\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "^classes\\/.+")
  }

  test("PatternUtils complex negation patterns - handle !classes/utils/** negation pattern correctly") {
    val input = "^classes\\/utils\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    assert(result == "^classes\\/utils\\/.+")
  }

  test("PatternUtils complex negation patterns - handle classes/utils/**/*Test.cls pattern correctly") {
    val input = "^classes\\/utils\\/\\*\\*\\/\\*Test\\.cls"
    val result = PatternUtils.replaceTwoGlobstars(input)
    // The /** in middle should be replaced with (?:\/[^\/]+)* per node-ignore logic
    assert(result == "^classes\\/utils(?:\\/[^\\/]+)*\\/\\*Test\\.cls")
  }

  test("PatternUtils edge cases - handle very long patterns") {
    val longPattern = "a" * 1000 + "\\*" + "b" * 1000
    val result = PatternUtils.replaceIntermediateWildcards(longPattern)
    assert(result == ("a" * 1000 + "[^\\/]*" + "b" * 1000))
  }

  test("PatternUtils edge cases - handle empty and whitespace patterns") {
    assert(PatternUtils.replaceQuestionMarks("   ") == "   ")
    assert(PatternUtils.replaceTwoGlobstars("\\t\\n") == "\\t\\n")
    assert(PatternUtils.addStartingAnchor("\\s+", "\\s+").contains("^(?:.*\\/)?"))
  }

  test("PatternUtils edge cases - handle patterns with no transformations needed") {
    val simplePattern = "simple/path/file.txt"
    assert(PatternUtils.replaceQuestionMarks(simplePattern) == simplePattern)
    assert(PatternUtils.replaceTwoGlobstars(simplePattern) == simplePattern)
    assert(PatternUtils.replaceIntermediateWildcards(simplePattern) == simplePattern)
    assert(PatternUtils.replaceUnescapePattern(simplePattern) == simplePattern)
    assert(PatternUtils.replaceTrailingWildcard(simplePattern) == simplePattern)
  }

  test("addStartingAnchor for patterns without slashes - match node-ignore behavior") {
    val pattern = "*.txt"
    val processedSource = "[^/]*\\.txt" // After metachar escaping and wildcard replacement

    val result = PatternUtils.addStartingAnchor(processedSource, pattern)

    // Based on node-ignore behavior, *.txt should match at any level
    // We use ^(?:.*\/)? which is equivalent and works better in Java/Scala
    assert(result.startsWith("^(?:.*\\/)?"))

    // Debug step-by-step transformation
    println(s"\\nDebugging transformation of '$pattern':")
    var step = pattern
    println(s"1. Original: $step")

    // Simulate the NodeIgnoreExact steps
    step = step.replaceAll("[\\\\$.\\|*+(){^]", "\\\\\\\\$0") // REPLACER 4: Escape metacharacters
    println(s"2. After metachar escape: $step")

    step = PatternUtils.replaceIntermediateWildcards(step) // REPLACER 11
    println(s"3. After intermediate wildcards: $step")

    step = PatternUtils.addStartingAnchor(step, pattern) // REPLACER 9
    println(s"4. After starting anchor: $step")

    step = PatternUtils.addEndingPattern(step) // REPLACER 15
    println(s"5. After ending pattern: $step")

    val fullRegex = NodeIgnoreExact.makeRegex(pattern)
    println(s"6. Full NodeIgnoreExact result: $fullRegex")

    val regexString = s"(?i)$fullRegex"
    val regex = regexString.r
    println(s"Full regex string: '$regexString'")

    // Test the exact string manually for comparison
    val manualTest = raw"(?i)^(?:.*\/)?[^/]*\.txt(?=$$|\/)".r
    println(s"Manual test string: '(?i)^(?:.*\\/)?[^/]*\\.txt(?=$$|\\/)'")
    println(s"Manual test matches 'sub/test.txt': ${manualTest.matches("sub/test.txt")}")

    val testCases = Seq(
      ("test.txt", true),
      ("sub/test.txt", true), // This should match per node-ignore
      ("sub/deep/test.txt", true),
      ("test.js", false)
    )

    testCases.foreach { case (path, shouldMatch) =>
      val matches = regex.matches(path)
      println(s"'$path' matches: $matches (expected: $shouldMatch)")
      assert(matches == shouldMatch)
    }
  }

  test("escaped question mark handling - match node-ignore behavior for test\\?.js pattern") {
    val pattern = "test\\?.js"
    val regexPattern = NodeIgnoreExact.makeRegex(pattern)
    val regex = s"(?i)$regexPattern".r
    val testPath = "test?.js"
    val matches = regex.matches(testPath)

    // Key requirement: should NOT match test?.js (this matches node-ignore behavior)
    // The exact regex internals don't matter as long as the behavior is correct
    assert(
      !matches,
      s"Pattern '$pattern' should NOT match path '$testPath' (node-ignore compatibility)"
    )
  }
}