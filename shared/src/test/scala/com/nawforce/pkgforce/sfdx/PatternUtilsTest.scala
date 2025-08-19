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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PatternUtilsTest extends AnyFlatSpec with Matchers {

  "replaceQuestionMarks" should "convert unescaped ? to [^/]" in {
    PatternUtils.replaceQuestionMarks("file?.txt") shouldBe "file[^/].txt"
    PatternUtils.replaceQuestionMarks("?") shouldBe "[^/]"
    PatternUtils.replaceQuestionMarks("a?b?c") shouldBe "a[^/]b[^/]c"
  }

  it should "not convert escaped ?" in {
    PatternUtils.replaceQuestionMarks("file\\?.txt") shouldBe "file\\?.txt"
    PatternUtils.replaceQuestionMarks("\\?") shouldBe "\\?"
    PatternUtils.replaceQuestionMarks("a\\?b?c") shouldBe "a\\?b[^/]c"
  }

  it should "handle empty and edge cases" in {
    PatternUtils.replaceQuestionMarks("") shouldBe ""
    PatternUtils.replaceQuestionMarks("nomarks") shouldBe "nomarks"
    PatternUtils.replaceQuestionMarks("???") shouldBe "[^/][^/][^/]"
  }

  "addStartingAnchor" should "not modify patterns that already start with ^" in {
    PatternUtils.addStartingAnchor("^pattern", "original") shouldBe "^pattern"
    PatternUtils.addStartingAnchor("^/test", "/test") shouldBe "^/test"
  }

  it should "add (?:^|\\/) for patterns without slashes" in {
    PatternUtils.addStartingAnchor("*.js", "*.js") shouldBe "(?:^|\\\\/)*.js"
    PatternUtils.addStartingAnchor("test", "test") should include("(?:^|\\\\/)")
  }

  it should "add ^ for patterns with slashes in middle" in {
    PatternUtils.addStartingAnchor("path\\/file", "path/file") shouldBe "^path\\/file"
    PatternUtils.addStartingAnchor("a\\/b\\/c", "a/b/c") shouldBe "^a\\/b\\/c"
  }

  it should "handle trailing slash patterns correctly" in {
    PatternUtils.addStartingAnchor("temp", "temp/") should include("(?:^|\\\\/)")
  }

  "replaceTwoGlobstars" should "replace /**/ patterns" in {
    val input = "prefix\\/\\*\\*\\/suffix"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "prefix(?:\\/[^\\/]+)*\\/suffix"
  }

  it should "replace /** at end patterns" in {
    val input = "prefix\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "prefix\\/.+"
  }

  it should "replace **/ at start patterns" in {
    val input = "^\\*\\*\\/suffix"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "^(?:.*\\/)?suffix"
  }

  it should "handle real patterns from NodeIgnoreExact - classes/**" in {
    val input = "^classes\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "^classes\\/.+"
  }

  it should "handle real patterns from NodeIgnoreExact - **/*_template" in {
    val input = "^\\*\\*\\/\\*_template"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "^(?:.*\\/)?\\*_template"
  }

  it should "handle real patterns from NodeIgnoreExact - **/.*" in {
    val input = "^\\*\\*\\/\\.\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "^(?:.*\\/)?\\.\\*"
  }

  it should "not modify other patterns" in {
    PatternUtils.replaceTwoGlobstars("normal\\*pattern") shouldBe "normal\\*pattern"
    PatternUtils.replaceTwoGlobstars("") shouldBe ""
    PatternUtils.replaceTwoGlobstars("\\*") shouldBe "\\*"
  }

  "replaceIntermediateWildcards" should "replace \\* when followed by more content" in {
    PatternUtils.replaceIntermediateWildcards("\\*abc") shouldBe "[^\\/]*abc"
    PatternUtils.replaceIntermediateWildcards("prefix\\*suffix") shouldBe "prefix[^\\/]*suffix"
  }

  it should "handle multiple consecutive \\* patterns" in {
    PatternUtils.replaceIntermediateWildcards("\\*\\*abc") shouldBe "[^\\/]*[^\\/]*abc"
  }

  it should "not replace \\* at end of string" in {
    PatternUtils.replaceIntermediateWildcards("pattern\\*") shouldBe "pattern\\*"
    PatternUtils.replaceIntermediateWildcards("\\*") shouldBe "\\*"
  }

  it should "handle mixed patterns" in {
    PatternUtils.replaceIntermediateWildcards("a\\*b\\*c\\*") shouldBe "a[^\\/]*b[^\\/]*c\\*"
  }

  "replaceUnescapePattern" should "replace \\\\\\\\ before metacharacters" in {
    PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\$") shouldBe "\\\\$"
    PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\*") shouldBe "\\\\*"
    PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\+") shouldBe "\\\\+"
    PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\(") shouldBe "\\\\("
  }

  it should "not replace \\\\\\\\ before non-metacharacters" in {
    PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\a") shouldBe "\\\\\\\\\\\\a"
    PatternUtils.replaceUnescapePattern("\\\\\\\\\\\\1") shouldBe "\\\\\\\\\\\\1"
  }

  it should "handle multiple patterns in string" in {
    val input = "\\\\\\\\\\\\$test\\\\\\\\\\\\*end"
    val result = PatternUtils.replaceUnescapePattern(input)
    result shouldBe "\\\\$test\\\\*end"
  }

  "addEndingPattern" should "add $ for patterns ending with /" in {
    PatternUtils.addEndingPattern("pattern", "original/") shouldBe "pattern$"
  }

  it should "add (?=$|\\\\/) for other patterns not ending with *" in {
    PatternUtils.addEndingPattern("pattern", "original") shouldBe "pattern(?=$|\\\\/)"
    PatternUtils.addEndingPattern("test", "test.txt") shouldBe "test(?=$|\\\\/)"
  }

  it should "not modify patterns ending with *" in {
    PatternUtils.addEndingPattern("pattern*", "original") shouldBe "pattern*"
    PatternUtils.addEndingPattern("*", "original") shouldBe "*"
  }

  it should "handle empty patterns" in {
    PatternUtils.addEndingPattern("", "original") shouldBe ""
  }

  "replaceTrailingWildcard" should "replace trailing \\* with [^/]*(?=$|\\/)" in {
    PatternUtils.replaceTrailingWildcard("pattern\\*") shouldBe "pattern[^/]*(?=$|\\/)"
  }

  it should "handle ^ prefix correctly" in {
    val result = PatternUtils.replaceTrailingWildcard("^\\*")
    result shouldBe "^[^/]+(?=$|\\/)"
  }

  it should "handle \\/ prefix correctly" in {
    val result = PatternUtils.replaceTrailingWildcard("prefix\\/\\*")
    result shouldBe "prefix\\/[^/]+(?=$|\\/)"
  }

  it should "not modify patterns not ending with \\*" in {
    PatternUtils.replaceTrailingWildcard("pattern") shouldBe "pattern"
    PatternUtils.replaceTrailingWildcard("test.txt") shouldBe "test.txt"
    PatternUtils.replaceTrailingWildcard("") shouldBe ""
  }

  // Integration tests combining multiple functions
  "PatternUtils functions" should "work together for complex patterns" in {
    // Test a pattern that would use multiple transformations
    var result = "file?.js"
    result = PatternUtils.replaceQuestionMarks(result)
    result shouldBe "file[^/].js"
    
    result = PatternUtils.addStartingAnchor(result, "file?.js")
    result should include("(?:^|\\\\/)")
    
    result = PatternUtils.addEndingPattern(result, "file?.js")
    result should include("(?=$|\\\\/)")
  }

  it should "handle escaped characters correctly throughout pipeline" in {
    var result = "test\\?.\\*"
    result = PatternUtils.replaceQuestionMarks(result)
    result shouldBe "test\\?.\\*" // ? should remain escaped
    
    result = PatternUtils.replaceIntermediateWildcards(result)
    result shouldBe "test\\?.\\*" // * should not be replaced (escaped)
  }
  
  // Tests for complex negation pattern from issue #323
  "PatternUtils complex negation patterns" should "handle classes/** pattern correctly" in {
    val input = "^classes\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "^classes\\/.+"
  }
  
  it should "handle !classes/utils/** negation pattern correctly" in {
    val input = "^classes\\/utils\\/\\*\\*"
    val result = PatternUtils.replaceTwoGlobstars(input)
    result shouldBe "^classes\\/utils\\/.+"
  }
  
  it should "handle classes/utils/**/*Test.cls pattern correctly" in {
    val input = "^classes\\/utils\\/\\*\\*\\/\\*Test\\.cls"
    val result = PatternUtils.replaceTwoGlobstars(input)
    // The /** in middle should be replaced with (?:\/[^\/]+)* per node-ignore logic
    result shouldBe "^classes\\/utils(?:\\/[^\\/]+)*\\/\\*Test\\.cls"
  }
}