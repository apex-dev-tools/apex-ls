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
      maxIssuesPerFile = 0
    )

    assert(issues.length == 3)
    assert(issues.exists(_.diagnostic.message == "Error message"))
    assert(issues.exists(_.diagnostic.message == "Warning message"))
    assert(issues.exists(_.diagnostic.message == "Unused message"))
  }

  test("issuesForFilesInternal with external filter excludes warnings from external paths") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    filteredIssuesManager.add(createErrorIssue(externalPath, "External error"))
    filteredIssuesManager.add(createWarningIssue(externalPath, "External warning"))
    filteredIssuesManager.add(createUnusedIssue(externalPath, "External unused"))

    val issues = filteredIssuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "External error")
    assert(issues.head.diagnostic.category == ERROR_CATEGORY)
  }

  test("issuesForFilesInternal with external filter preserves all issues from non-external paths") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    filteredIssuesManager.add(createErrorIssue(testPath, "Regular error"))
    filteredIssuesManager.add(createWarningIssue(testPath, "Regular warning"))
    filteredIssuesManager.add(createUnusedIssue(testPath, "Regular unused"))

    val issues = filteredIssuesManager.issuesForFilesInternal(
      paths = Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )

    assert(issues.length == 3)
    assert(issues.exists(_.diagnostic.message == "Regular error"))
    assert(issues.exists(_.diagnostic.message == "Regular warning"))
    assert(issues.exists(_.diagnostic.message == "Regular unused"))
  }

  test(
    "issuesForFilesInternal with external filter and includeWarnings=false preserves errors from external paths"
  ) {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    filteredIssuesManager.add(createErrorIssue(externalPath, "External error"))
    filteredIssuesManager.add(createWarningIssue(externalPath, "External warning"))

    val issues = filteredIssuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = false,
      maxIssuesPerFile = 0
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "External error")
    assert(issues.head.diagnostic.category == ERROR_CATEGORY)
  }

  test("issuesForFilesInternal handles mixed paths with external filter") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    filteredIssuesManager.add(createErrorIssue(testPath, "Regular error"))
    filteredIssuesManager.add(createWarningIssue(testPath, "Regular warning"))
    filteredIssuesManager.add(createErrorIssue(externalPath, "External error"))
    filteredIssuesManager.add(createWarningIssue(externalPath, "External warning"))

    val issues = filteredIssuesManager.issuesForFilesInternal(
      paths = Array(testPath, externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
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
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    filteredIssuesManager.add(createErrorIssue(externalPath, "External error"))
    filteredIssuesManager.add(createWarningIssue(externalPath, "External warning"))

    val issues = filteredIssuesManager.issuesForFileInternal(externalPath)

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "External error")
  }

  test("maxIssuesPerFile parameter works with external filtering") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    (1 to 5).foreach(i => {
      filteredIssuesManager.add(createErrorIssue(externalPath, s"External error $i"))
      filteredIssuesManager.add(createWarningIssue(externalPath, s"External warning $i"))
    })

    val issues = filteredIssuesManager.issuesForFilesInternal(
      paths = Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 3
    )

    // Should get 3 errors (warnings filtered out by external filter)
    assert(issues.length == 3)
    assert(issues.forall(_.diagnostic.category == ERROR_CATEGORY))
  }

  test("external filter respects null paths parameter") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredIssuesManager               = new IssuesManager(Some(externalFilter))

    filteredIssuesManager.add(createErrorIssue(testPath, "Regular error"))
    filteredIssuesManager.add(createWarningIssue(externalPath, "External warning"))

    val issues = filteredIssuesManager.issuesForFilesInternal(
      paths = null,
      includeWarnings = true,
      maxIssuesPerFile = 0
    )

    assert(issues.length == 1)
    assert(issues.head.diagnostic.message == "Regular error")
  }
}
