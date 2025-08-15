/*
 Copyright (c) 2024 Kevin Jones, All rights reserved.
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

package com.nawforce.runtime

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.workspace.Workspace
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Tests for FileSystemHelper SFDX auto-creation enhancement.
  *
  * Verifies that FileSystemHelper automatically creates sfdx-project.json
  * when not provided, enabling tests to work with raw metadata files.
  */
class FileSystemHelperEnhancementTest extends AnyFunSuite with Matchers {

  test("FileSystemHelper.run() automatically creates sfdx-project.json when not provided") {
    val apexClass =
      """public class TestClass {
        |  public void testMethod() {}
        |}""".stripMargin

    FileSystemHelper.run(Map("force-app/main/default/classes/TestClass.cls" -> apexClass)) {
      root: PathLike =>
        // Verify sfdx-project.json was auto-created
        val sfdxProjectPath = root.join("sfdx-project.json")
        assert(sfdxProjectPath.exists, "sfdx-project.json should be automatically created")

        // Verify the content is valid JSON with expected structure
        val content = sfdxProjectPath.read().getOrElse("")
        assert(content.contains("packageDirectories"), "Should contain packageDirectories")
        assert(content.contains("force-app"), "Should reference force-app directory")
        assert(content.contains("sourceApiVersion"), "Should contain sourceApiVersion")

        // Verify workspace can be created successfully
        val (wsOpt, logger) = Workspace(root)
        assert(logger.isEmpty, "Should not have any errors")
        assert(wsOpt.nonEmpty, "Should successfully create workspace")
    }
  }

  test("FileSystemHelper.run() does not override existing sfdx-project.json") {
    val customSfdxProject = """{
      "packageDirectories": [
        {"path": "custom-app", "default": true}
      ],
      "sfdcLoginUrl": "https://test.salesforce.com",
      "sourceApiVersion": "59.0"
    }"""

    val apexClass = "public class TestClass {}"

    FileSystemHelper.run(
      Map(
        "sfdx-project.json"                             -> customSfdxProject,
        "custom-app/main/default/classes/TestClass.cls" -> apexClass
      )
    ) { root: PathLike =>
      // Verify custom sfdx-project.json was preserved
      val sfdxProjectPath = root.join("sfdx-project.json")
      val content         = sfdxProjectPath.read().getOrElse("")
      assert(content.contains("custom-app"), "Should preserve custom package directory")
      assert(content.contains("test.salesforce.com"), "Should preserve custom login URL")
      assert(content.contains("59.0"), "Should preserve custom API version")

      // Verify workspace can still be created
      val (wsOpt, logger) = Workspace(root)
      assert(logger.isEmpty, "Should not have any errors with custom config")
      assert(wsOpt.nonEmpty, "Should successfully create workspace with custom config")
    }
  }

  test("FileSystemHelper.run() works with empty files map") {
    FileSystemHelper.run(Map.empty[String, String]) { root: PathLike =>
      // Verify sfdx-project.json was auto-created even with empty files
      val sfdxProjectPath = root.join("sfdx-project.json")
      assert(sfdxProjectPath.exists, "sfdx-project.json should be created even with empty files")

      // Verify workspace can be created
      val (wsOpt, logger) = Workspace(root)
      assert(logger.isEmpty, "Should not have any errors with auto-created config")
      assert(wsOpt.nonEmpty, "Should successfully create workspace with auto-created config")
    }
  }
}
