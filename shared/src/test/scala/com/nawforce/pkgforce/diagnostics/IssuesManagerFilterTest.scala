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
package com.nawforce.pkgforce.diagnostics

import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.platform.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class IssuesManagerFilterTest extends AnyFunSuite with BeforeAndAfter {
  var issuesManager: IssuesManager = _
  val testPath: PathLike           = Path("/project/src/classes/TestClass.cls")
  val externalPath: PathLike       = Path("/project/external/ExternalClass.cls")
  val location: Location           = Location(1, 0, 1, 10)

  before {
    issuesManager = new IssuesManager()
  }

  def createErrorIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(ERROR_CATEGORY, location, message))

  def createWarningIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(WARNING_CATEGORY, location, message))

  def createUnusedIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(UNUSED_CATEGORY, location, message))

  test("issuesForFilesInternal without external filter includes all issues") {
    issuesManager.add(createErrorIssue(testPath, "Error message"))
    issuesManager.add(createWarningIssue(testPath, "Warning message"))
    issuesManager.add(createUnusedIssue(testPath, "Unused message"))

    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0,
      externalPathFilter = None
    )

    assert(issues.length == 3)
    assert(issues.exists(_.diagnostic.message == "Error message"))
    assert(issues.exists(_.diagnostic.message == "Warning message"))
    assert(issues.exists(_.diagnostic.message == "Unused message"))
  }

  test("issuesForFilesInternal with external filter excludes warnings from external paths") {
    issuesManager.add(createErrorIssue(externalPath, "External error"))
    issuesManager.add(createWarningIssue(externalPath, "External warning"))
    issuesManager.add(createUnusedIssue(externalPath, "External unused"))

    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0,
      externalPathFilter = Some(externalFilter)
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "External error")
    assert(issues.head.diagnostic.category == ERROR_CATEGORY)
  }

  test("issuesForFilesInternal with external filter preserves all issues from non-external paths") {
    issuesManager.add(createErrorIssue(testPath, "Regular error"))
    issuesManager.add(createWarningIssue(testPath, "Regular warning"))
    issuesManager.add(createUnusedIssue(testPath, "Regular unused"))

    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0,
      externalPathFilter = Some(externalFilter)
    )

    assert(issues.length == 3)
    assert(issues.exists(_.diagnostic.message == "Regular error"))
    assert(issues.exists(_.diagnostic.message == "Regular warning"))
    assert(issues.exists(_.diagnostic.message == "Regular unused"))
  }

  test(
    "issuesForFilesInternal with external filter and includeWarnings=false preserves errors from external paths"
  ) {
    issuesManager.add(createErrorIssue(externalPath, "External error"))
    issuesManager.add(createWarningIssue(externalPath, "External warning"))

    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = false,
      maxIssuesPerFile = 0,
      externalPathFilter = Some(externalFilter)
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "External error")
    assert(issues.head.diagnostic.category == ERROR_CATEGORY)
  }

  test("issuesForFilesInternal handles mixed paths with external filter") {
    issuesManager.add(createErrorIssue(testPath, "Regular error"))
    issuesManager.add(createWarningIssue(testPath, "Regular warning"))
    issuesManager.add(createErrorIssue(externalPath, "External error"))
    issuesManager.add(createWarningIssue(externalPath, "External warning"))

    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(testPath, externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0,
      externalPathFilter = Some(externalFilter)
    )

    assert(issues.length == 3)
    assert(issues.exists(_.diagnostic.message == "Regular error"))
    assert(issues.exists(_.diagnostic.message == "Regular warning"))
    assert(issues.exists(_.diagnostic.message == "External error"))
    assert(!issues.exists(_.diagnostic.message == "External warning"))
  }

  test("issuesForFileInternal delegates to issuesForFilesInternal with correct parameters") {
    issuesManager.add(createErrorIssue(testPath, "Test error"))
    issuesManager.add(createWarningIssue(testPath, "Test warning"))

    val issues = issuesManager.issuesForFileInternal(testPath)

    assert(issues.length == 2)
    assert(issues.exists(_.diagnostic.message == "Test error"))
    assert(issues.exists(_.diagnostic.message == "Test warning"))
  }

  test("issuesForFileInternal with external filter applied through issuesForFilesInternal") {
    issuesManager.add(createErrorIssue(externalPath, "External error"))
    issuesManager.add(createWarningIssue(externalPath, "External warning"))

    // Simulate the behavior when external path filter is applied via the API
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0,
      externalPathFilter = Some(externalFilter)
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "External error")
  }

  test("maxIssuesPerFile parameter works with external filtering") {
    (1 to 5).foreach(i => {
      issuesManager.add(createErrorIssue(externalPath, s"External error $i"))
      issuesManager.add(createWarningIssue(externalPath, s"External warning $i"))
    })

    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 3,
      externalPathFilter = Some(externalFilter)
    )

    // Should get 3 errors (warnings filtered out by external filter)
    assert(issues.length == 3)
    assert(issues.forall(_.diagnostic.category == ERROR_CATEGORY))
  }

  test("external filter respects null paths parameter") {
    issuesManager.add(createErrorIssue(testPath, "Regular error"))
    issuesManager.add(createWarningIssue(externalPath, "External warning"))

    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val issues = issuesManager.issuesForFilesInternal(
      paths = null,
      includeWarnings = true,
      maxIssuesPerFile = 0,
      externalPathFilter = Some(externalFilter)
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "Regular error")
  }
}
