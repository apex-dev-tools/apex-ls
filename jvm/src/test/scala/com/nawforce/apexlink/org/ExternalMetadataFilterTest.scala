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
package com.nawforce.apexlink.org

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class ExternalMetadataFilterTest extends AnyFunSuite {

  test("External metadata filter suppresses warnings but not errors") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> """{
          "packageDirectories": [{"path": "force-app"}],
          "plugins": {
            "externalMetadata": ["vendor"]
          }
        }""",
        "force-app/main/default/classes/Test.cls" -> "public class Test {}",
        "vendor/lib/External.cls"                 -> "public class External {}"
      )
    ) { root: PathLike =>
      // Create external path filter
      val externalPathFilter: PathLike => Boolean = { path =>
        path.toString.contains("vendor")
      }

      val issueManager = new IssueLogger(Some(externalPathFilter))

      // Add a warning and error for both internal and external files
      val internalFile = root.join("force-app/main/default/classes/Test.cls")
      val externalFile = root.join("vendor/lib/External.cls")

      issueManager.add(
        Issue(internalFile, Diagnostic(WARNING_CATEGORY, Location.empty, "Internal warning"))
      )
      issueManager.add(
        Issue(internalFile, Diagnostic(ERROR_CATEGORY, Location.empty, "Internal error"))
      )
      issueManager.add(
        Issue(externalFile, Diagnostic(WARNING_CATEGORY, Location.empty, "External warning"))
      )
      issueManager.add(
        Issue(externalFile, Diagnostic(ERROR_CATEGORY, Location.empty, "External error"))
      )

      // Test with warnings included - should suppress external warnings but keep external errors
      val allIssues = issueManager.issuesForFilesInternal(
        paths = null,
        includeWarnings = true,
        maxIssuesPerFile = 0
      )

      // Should get: internal warning, internal error, external error (external warning suppressed)
      assert(allIssues.size == 3)
      assert(allIssues.exists(_.diagnostic.message == "Internal warning"))
      assert(allIssues.exists(_.diagnostic.message == "Internal error"))
      assert(allIssues.exists(_.diagnostic.message == "External error"))
      assert(!allIssues.exists(_.diagnostic.message == "External warning"))

      // Test without warnings - should get only errors
      val errorsOnly = issueManager.issuesForFilesInternal(
        paths = null,
        includeWarnings = false,
        maxIssuesPerFile = 0
      )

      // Should get: internal error, external error
      assert(errorsOnly.size == 2)
      assert(errorsOnly.exists(_.diagnostic.message == "Internal error"))
      assert(errorsOnly.exists(_.diagnostic.message == "External error"))
    }
  }

  test("External metadata filter works with no external paths configured") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> """{
          "packageDirectories": [{"path": "force-app"}]
        }""",
        "force-app/main/default/classes/Test.cls" -> "public class Test {}"
      )
    ) { root: PathLike =>
      val issueManager = new IssueLogger(None)

      val internalFile = root.join("force-app/main/default/classes/Test.cls")
      issueManager.add(Issue(internalFile, Diagnostic(WARNING_CATEGORY, Location.empty, "Warning")))
      issueManager.add(Issue(internalFile, Diagnostic(ERROR_CATEGORY, Location.empty, "Error")))

      // With no external filter (None) - should get all issues
      val allIssues = issueManager.issuesForFilesInternal(
        paths = null,
        includeWarnings = true,
        maxIssuesPerFile = 0
      )

      assert(allIssues.size == 2)
      assert(allIssues.exists(_.diagnostic.message == "Warning"))
      assert(allIssues.exists(_.diagnostic.message == "Error"))
    }
  }
}
