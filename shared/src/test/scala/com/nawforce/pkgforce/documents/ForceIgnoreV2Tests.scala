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
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.sfdx.ForceIgnoreV2
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class ForceIgnoreV2Tests extends AnyFunSuite {

  test("Issue #323: classes/** should ignore files inside classes directory") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "classes/**")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "**/*_template")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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
      assert(ignore.includeFile(root.join("my_template_file.txt")))  // doesn't END with _template
    }
  }

  test("Issue #323: **/classes_template should work for exact matches") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "**/classes_template")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "classes/*.cls*")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "classes/**\n!classes/important.cls")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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

  test("Default Salesforce CLI patterns should be applied") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
      // Should ignore .dup files
      assert(!ignore.includeFile(root.join("classes/MyClass.cls.dup")))
      assert(!ignore.includeFile(root.join("utils/helper.dup")))
      
      // Should ignore hidden files
      assert(!ignore.includeFile(root.join("classes/.hidden")))
      assert(!ignore.includeFile(root.join(".gitignore")))
      
      // Should ignore package2 descriptor files
      assert(!ignore.includeFile(root.join("package2-descriptor.json")))
      assert(!ignore.includeFile(root.join("utils/package2-manifest.json")))
      
      // Should NOT ignore regular files
      assert(ignore.includeFile(root.join("classes/MyClass.cls")))
      assert(ignore.includeFile(root.join("manifest/package.xml")))
    }
  }

  test("Directory-only patterns (ending with /) should match files inside directories") {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> "temp/")) { root: PathLike =>
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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
      val ignore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
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
}