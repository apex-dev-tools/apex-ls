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

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/**
 * Comparison test that validates ForceIgnoreV2 against actual node-ignore library
 * This test only runs on ScalaJS platform where we can import node-ignore
 */
class ForceIgnoreV2NodeIgnoreComparisonTest extends AnyFunSuite {

  // Import node-ignore library
  @js.native
  @JSImport("ignore", JSImport.Default)
  def NodeIgnore(): NodeIgnoreInstance = js.native

  @js.native
  trait NodeIgnoreInstance extends js.Object {
    def add(patterns: js.Array[String]): NodeIgnoreInstance = js.native
    def add(pattern: String): NodeIgnoreInstance = js.native
    def ignores(path: String): Boolean = js.native
  }

  def compareWithNodeIgnore(patterns: Seq[String], testPaths: Seq[String]): Unit = {
    FileSystemHelper.runTempDir(Map[String, String](".forceignore" -> patterns.mkString("\n"))) { root: PathLike =>
      val forceIgnore = ForceIgnoreV2(root.join(".forceignore")).value.getOrElse(throw new NoSuchElementException())
      
      // Create node-ignore instance with same patterns
      val nodeIgnore = NodeIgnore()
      nodeIgnore.add(js.Array(patterns: _*))
      
      // Test each path
      testPaths.foreach { path =>
        val nodeIgnoreResult = nodeIgnore.ignores(path)
        val forceIgnoreResult = !forceIgnore.includeFile(root.join(path))
        
        assert(
          nodeIgnoreResult == forceIgnoreResult,
          s"Mismatch for path '$path': node-ignore=$nodeIgnoreResult, ForceIgnoreV2=$forceIgnoreResult"
        )
      }
    }
  }

  test("Issue #323: Complex negation pattern comparison") {
    val patterns = Seq(
      "classes/**",
      "!classes/utils/**", 
      "classes/utils/**/*Test.cls"
    )
    
    val testPaths = Seq(
      "classes/MyClass.cls",           // Should be ignored
      "classes/other/Helper.cls",      // Should be ignored  
      "classes/utils/UtilClass.cls",   // Should NOT be ignored (negated)
      "classes/utils/helper/HelperClass.cls", // Should NOT be ignored (negated)
      "classes/utils/MyTest.cls",      // Should be ignored (re-ignored)
      "classes/utils/helper/HelperTest.cls"   // Should be ignored (re-ignored)
    )
    
    compareWithNodeIgnore(patterns, testPaths)
  }

  test("Basic glob patterns comparison") {
    val patterns = Seq("*.js", "temp/", "**/*.log")
    
    val testPaths = Seq(
      "file.js",              // Should be ignored
      "app.ts",               // Should NOT be ignored
      "temp/file.txt",        // Should be ignored (directory pattern)
      "logs/app.log",         // Should be ignored
      "nested/deep/error.log" // Should be ignored
    )
    
    compareWithNodeIgnore(patterns, testPaths)
  }

  test("Question mark wildcards comparison") {
    val patterns = Seq("file?.txt", "test\\?.js")
    
    val testPaths = Seq(
      "file1.txt",    // Should be ignored
      "fileA.txt",    // Should be ignored  
      "file12.txt",   // Should NOT be ignored (? matches single char)
      "test?.js",     // Should be ignored (escaped ?)
      "test1.js"      // Should NOT be ignored (? is escaped)
    )
    
    compareWithNodeIgnore(patterns, testPaths)
  }

  test("Double globstar patterns comparison") {
    val patterns = Seq("**/classes/**", "src/**/test/**/*.js")
    
    val testPaths = Seq(
      "classes/MyClass.cls",           // Should be ignored
      "utils/classes/Helper.cls",      // Should be ignored
      "deep/nested/classes/Test.cls",  // Should be ignored
      "src/test/unit.js",              // Should be ignored  
      "src/main/test/integration/spec.js", // Should be ignored
      "src/main/app.js"                // Should NOT be ignored
    )
    
    compareWithNodeIgnore(patterns, testPaths)
  }

  test("Leading slash patterns comparison") {
    val patterns = Seq("/root.txt", "/config/**")
    
    val testPaths = Seq(
      "root.txt",           // Should be ignored (anchored to root)
      "sub/root.txt",       // Should NOT be ignored (not at root)
      "config/app.json",    // Should be ignored
      "lib/config/db.json"  // Should NOT be ignored (not at root)
    )
    
    compareWithNodeIgnore(patterns, testPaths)
  }

  test("Range notation patterns comparison") {
    val patterns = Seq("file[0-9].txt", "temp[abc].log")
    
    val testPaths = Seq(
      "file1.txt",    // Should be ignored
      "file9.txt",    // Should be ignored
      "fileA.txt",    // Should NOT be ignored
      "tempa.log",    // Should be ignored
      "tempd.log"     // Should NOT be ignored
    )
    
    compareWithNodeIgnore(patterns, testPaths)
  }
}