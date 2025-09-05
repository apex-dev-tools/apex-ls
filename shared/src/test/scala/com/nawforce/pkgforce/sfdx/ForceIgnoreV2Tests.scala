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

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class ForceIgnoreV2Tests extends AnyFunSuite {

  test("Issue #323: classes/** should ignore files inside classes directory") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "classes/**")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Should NOT ignore the classes directory itself (per actual node-ignore behavior)
        assert(ignore.includeDirectory(root.join("classes")))

        // Should ignore files directly in classes/
        assert(!ignore.includeFile(root.join("classes/MyClass.cls")))
        assert(!ignore.includeFile(root.join("classes/Helper.cls")))

        // Should ignore subdirectories and their contents
        assert(!ignore.includeDirectory(root.join("classes/utils")))
        assert(!ignore.includeFile(root.join("classes/utils/UtilClass.cls")))
        assert(!ignore.includeFile(root.join("classes/nested/deep/DeepClass.cls")))

        // Should NOT ignore other directories
        assert(ignore.includeDirectory(root.join("triggers")))
        assert(ignore.includeFile(root.join("triggers/MyTrigger.trigger")))
    }
  }

  test("Issue #323: **/*_template should match files ending with _template in any directory") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "**/*_template")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Should ignore files ending with _template
        assert(!ignore.includeFile(root.join("component_template")))
        assert(!ignore.includeFile(root.join("classes/MyClass_template")))
        assert(!ignore.includeFile(root.join("utils/Helper_template")))
        assert(!ignore.includeFile(root.join("deep/nested/path/Config_template")))

        // Should NOT ignore files with extensions after _template (per actual node-ignore behavior)
        assert(ignore.includeFile(root.join("Config_template.json")))
        assert(ignore.includeFile(root.join("classes/MyClass_template.cls")))

        // Should NOT ignore files that don't end with _template
        assert(ignore.includeFile(root.join("classes/MyClass.cls")))
        assert(ignore.includeFile(root.join("template_file.txt")))
        assert(ignore.includeFile(root.join("my_template_file.txt"))) // doesn't END with _template
    }
  }

  test("Issue #323: **/classes_template should work for exact matches") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "**/classes_template")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Should ignore exact matches
        assert(!ignore.includeFile(root.join("classes_template")))
        assert(!ignore.includeFile(root.join("utils/classes_template")))
        assert(!ignore.includeFile(root.join("deep/nested/classes_template")))

        // Should NOT ignore with extensions (per actual node-ignore behavior)
        assert(ignore.includeFile(root.join("classes_template.json")))
        assert(ignore.includeFile(root.join("utils/classes_template.xml")))

        // Should NOT ignore partial matches
        assert(ignore.includeFile(root.join("old_classes_template")))
        assert(ignore.includeFile(root.join("my_classes_template")))

        // Should NOT ignore different names
        assert(ignore.includeFile(root.join("classes_other")))
        assert(ignore.includeFile(root.join("template_classes")))
    }
  }

  test("Issue #323: classes/*.cls* should match .cls files directly in classes directory") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "classes/*.cls*")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Should ignore .cls files in classes/
        assert(!ignore.includeFile(root.join("classes/MyClass.cls")))
        assert(!ignore.includeFile(root.join("classes/Helper.cls-meta.xml")))

        // Should NOT ignore .cls files in subdirectories
        assert(ignore.includeFile(root.join("classes/utils/UtilClass.cls")))
        assert(ignore.includeFile(root.join("classes/nested/NestedClass.cls")))

        // Should NOT ignore other file types in classes/
        assert(ignore.includeFile(root.join("classes/MyTrigger.trigger")))
        assert(ignore.includeFile(root.join("classes/readme.txt")))

        // Should NOT ignore .cls files in other directories
        assert(ignore.includeFile(root.join("triggers/TriggerClass.cls")))
        assert(ignore.includeFile(root.join("utils/UtilClass.cls")))
    }
  }

  test("Negation patterns should work correctly") {
    FileSystemHelper.runTempDir(
      Map[String, String](".forceignore" -> "classes/**\n!classes/important.cls")
    ) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // Should ignore classes directory contents by default
      assert(!ignore.includeFile(root.join("classes/MyClass.cls")))
      assert(!ignore.includeFile(root.join("classes/Helper.cls")))

      // Should NOT ignore the specifically un-ignored file
      assert(ignore.includeFile(root.join("classes/important.cls")))

      // Should still ignore other files
      assert(!ignore.includeFile(root.join("classes/other.cls")))
      assert(!ignore.includeFile(root.join("classes/utils/UtilClass.cls")))
    }
  }


  test("Directory-only patterns (ending with /) should match files inside directories") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "temp/")) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // Should NOT ignore the directory itself (per actual node-ignore behavior)
      assert(ignore.includeDirectory(root.join("temp")))
      assert(ignore.includeDirectory(root.join("utils/temp")))

      // Should NOT ignore files named temp
      assert(ignore.includeFile(root.join("temp")))
      assert(ignore.includeFile(root.join("utils/temp")))

      // Should ignore files INSIDE temp directory
      assert(!ignore.includeFile(root.join("temp/file.txt")))
      assert(!ignore.includeFile(root.join("temp/subdir/file.js")))
    }
  }

  test("Complex pattern combinations should work correctly") {
    val patterns = """
      |# Ignore all class files
      |classes/**
      |# But keep important utilities
      |!classes/utils/**
      |# Except ignore test files in utils
      |classes/utils/**/*Test.cls
      |""".stripMargin.trim

    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> patterns)) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // Should ignore regular classes
      assert(!ignore.includeFile(root.join("classes/MyClass.cls")))
      assert(!ignore.includeFile(root.join("classes/other/Helper.cls")))

      // Should ignore utils (parent directory exclusion rule: cannot re-include files if parent is excluded)
      // This matches actual node-ignore behavior - negation doesn't work when parent directory is excluded
      assert(!ignore.includeFile(root.join("classes/utils/UtilClass.cls")))
      assert(!ignore.includeFile(root.join("classes/utils/helper/HelperClass.cls")))

      // Should ignore test files in utils (re-ignored)
      assert(!ignore.includeFile(root.join("classes/utils/MyTest.cls")))
      assert(!ignore.includeFile(root.join("classes/utils/helper/HelperTest.cls")))
    }
  }

  test("Question mark patterns should work correctly") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "file?.txt\ntest??.js")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Single ? should match single non-slash character
        assert(!ignore.includeFile(root.join("filea.txt")))
        assert(!ignore.includeFile(root.join("file1.txt")))
        assert(ignore.includeFile(root.join("file.txt")))   // No char to match
        assert(ignore.includeFile(root.join("file/a.txt"))) // ? doesn't match /

        // Multiple ? should each match one character
        assert(!ignore.includeFile(root.join("test12.js")))
        assert(!ignore.includeFile(root.join("testab.js")))
        assert(ignore.includeFile(root.join("test1.js")))   // Only one char for two ?
        assert(ignore.includeFile(root.join("test123.js"))) // Too many chars
    }
  }

  test("Leading slash patterns should be anchored to root") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "/root-only.txt")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Leading / should only match from root
        assert(!ignore.includeFile(root.join("root-only.txt")))
        assert(ignore.includeFile(root.join("sub/root-only.txt")))
        assert(ignore.includeFile(root.join("deep/nested/root-only.txt")))
    }
  }

  test("Wildcard patterns should match at any level (node-ignore behavior)") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "*.txt")) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // Should match at root level
      assert(!ignore.includeFile(root.join("test.txt")))

      // Should match in subdirectories (node-ignore 5.3.2 behavior)
      assert(!ignore.includeFile(root.join("sub/test.txt")))
      assert(!ignore.includeFile(root.join("sub/deep/test.txt")))

      // Should not match non-.txt files
      assert(ignore.includeFile(root.join("test.js")))
      assert(ignore.includeFile(root.join("sub/test.js")))
    }
  }

  test("Range patterns should work correctly") {
    FileSystemHelper.runTempDir(
      Map[String, String](".forceignore" -> "file[0-9].txt\ntest[abc].js\nrange[^xyz].cls")
    ) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // Numeric range
      assert(!ignore.includeFile(root.join("file0.txt")))
      assert(!ignore.includeFile(root.join("file5.txt")))
      assert(!ignore.includeFile(root.join("file9.txt")))
      assert(ignore.includeFile(root.join("filea.txt")))
      assert(ignore.includeFile(root.join("file10.txt"))) // Only matches single char

      // Character set
      assert(!ignore.includeFile(root.join("testa.js")))
      assert(!ignore.includeFile(root.join("testb.js")))
      assert(!ignore.includeFile(root.join("testc.js")))
      assert(ignore.includeFile(root.join("testd.js")))

      // Negated range (using ^ instead of ! for regex negation)
      assert(!ignore.includeFile(root.join("rangea.cls")))
      assert(!ignore.includeFile(root.join("range1.cls")))
      assert(ignore.includeFile(root.join("rangex.cls")))
      assert(ignore.includeFile(root.join("rangey.cls")))
      assert(ignore.includeFile(root.join("rangez.cls")))
    }
  }

  test("Comment and blank line handling should work correctly") {
    val patterns = """
      |# This is a comment
      |
      |*.txt
      |   # Indented comment
      |
      |*.js
      |
      |# Another comment
      |*.cls
      |""".stripMargin

    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> patterns)) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // Patterns should work despite comments and blank lines
      assert(!ignore.includeFile(root.join("test.txt")))
      assert(!ignore.includeFile(root.join("app.js")))
      assert(!ignore.includeFile(root.join("MyClass.cls")))

      // Comments should not be treated as patterns
      assert(ignore.includeFile(root.join("# This is a comment")))
      assert(ignore.includeFile(root.join("# Another comment")))
    }
  }

  test("Case sensitivity should follow node-ignore behavior") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "MyClass.cls\n*.TXT")) {
      root: PathLike =>
        val ignore = ForceIgnoreV2(root.join(".forceignore")).value
          .getOrElse(throw new NoSuchElementException())

        // Should be case insensitive (node-ignore default)
        assert(!ignore.includeFile(root.join("MyClass.cls")))
        assert(!ignore.includeFile(root.join("myclass.cls")))
        assert(!ignore.includeFile(root.join("MYCLASS.CLS")))

        assert(!ignore.includeFile(root.join("test.txt")))
        assert(!ignore.includeFile(root.join("test.TXT")))
        assert(!ignore.includeFile(root.join("README.Txt")))
    }
  }

  test("Empty .forceignore file should include all files") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "")) { root: PathLike =>
      val ignore =
        ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())

      // With empty .forceignore, no patterns should apply, so all files are included
      assert(ignore.includeFile(root.join("test.dup")))
      assert(ignore.includeFile(root.join(".hidden")))
      assert(ignore.includeFile(root.join("package2-descriptor.json")))
      assert(ignore.includeFile(root.join("package2-manifest.json")))

      // Regular files should be included
      assert(ignore.includeFile(root.join("MyClass.cls")))
      assert(ignore.includeFile(root.join("test.txt")))
      assert(ignore.includeFile(root.join("manifest/package.xml")))
    }
  }

  test("Missing .forceignore file should return None") {
    FileSystemHelper.runTempDir(Map[String, String]()) { root: PathLike =>
      val result = ForceIgnoreV2(root.join(".forceignore"))
      assert(result.value.isEmpty)
    }
  }
}
