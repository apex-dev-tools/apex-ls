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

class IssueLoggerTest extends AnyFunSuite with BeforeAndAfter {
  var issuesManager: IssueLogger = _
  val testPath: PathLike           = Path("/project/src/classes/TestClass.cls")
  val externalPath: PathLike       = Path("/project/external/ExternalClass.cls")
  val location: Location           = Location(1, 0, 1, 10)

  before {
    issuesManager = new IssueLogger()
  }

  def createErrorIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(ERROR_CATEGORY, location, message))

  def createWarningIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(WARNING_CATEGORY, location, message))

  def createUnusedIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(UNUSED_CATEGORY, location, message))

  def createSyntaxIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(SYNTAX_CATEGORY, location, message))

  def createMissingIssue(path: PathLike, message: String): Issue =
    Issue(path, Diagnostic(MISSING_CATEGORY, location, message))

  def createProviderIssue(path: PathLike, message: String, provider: String): Issue =
    Issue(path, Diagnostic(ERROR_CATEGORY, location, message), provider)

  test("Empty IssueLogger has correct initial state") {
    assert(issuesManager.isEmpty)
    assert(!issuesManager.nonEmpty)
    assert(!issuesManager.hasErrors)
    assert(issuesManager.hasUpdatedIssues.isEmpty)
  }

  test("add method stores issues correctly") {
    issuesManager.add(createErrorIssue(testPath, "Test error"))

    assert(!issuesManager.isEmpty)
    assert(issuesManager.nonEmpty)
    assert(issuesManager.hasErrors)
    assert(issuesManager.hasUpdatedIssues.contains(testPath.toString))
  }

  test("add method with write-time filtering stores only allowed issues") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredManager                     = new IssueLogger(Some(externalFilter))

    // Add warning to external path - should be filtered out
    filteredManager.add(createWarningIssue(externalPath, "External warning"))
    assert(filteredManager.isEmpty)

    // Add error to external path - should be stored
    filteredManager.add(createErrorIssue(externalPath, "External error"))
    assert(!filteredManager.isEmpty)
    assert(filteredManager.hasErrors)

    // Add warning to internal path - should be stored
    filteredManager.add(createWarningIssue(testPath, "Internal warning"))
    val issues =
      filteredManager.issuesForFilesInternal(null, includeWarnings = true, maxIssuesPerFile = 0)
    assert(issues.size == 2)
    assert(issues.exists(_.diagnostic.message == "External error"))
    assert(issues.exists(_.diagnostic.message == "Internal warning"))
  }

  test("clear method removes all issues") {
    issuesManager.add(createErrorIssue(testPath, "Test error"))
    issuesManager.add(createWarningIssue(testPath, "Test warning"))

    assert(!issuesManager.isEmpty)

    issuesManager.clear()

    assert(issuesManager.isEmpty)
    assert(!issuesManager.hasErrors)
    assert(issuesManager.hasUpdatedIssues.isEmpty)
  }

  test("pop method retrieves and removes issues for path") {
    issuesManager.add(createErrorIssue(testPath, "Error 1"))
    issuesManager.add(createWarningIssue(testPath, "Warning 1"))

    val issues = issuesManager.pop(testPath)

    assert(issues.size == 2)
    assert(issues.exists(_.diagnostic.message == "Error 1"))
    assert(issues.exists(_.diagnostic.message == "Warning 1"))

    // Path should be empty after pop
    val remainingIssues = issuesManager.issuesForFilesInternal(
      Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )
    assert(remainingIssues.isEmpty)
  }

  test("push method stores filtered issues") {
    val issues =
      List(createErrorIssue(testPath, "Error 1"), createWarningIssue(testPath, "Warning 1"))

    issuesManager.push(testPath, issues)

    val retrievedIssues = issuesManager.issuesForFilesInternal(
      Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )
    assert(retrievedIssues.size == 2)
    assert(retrievedIssues.exists(_.diagnostic.message == "Error 1"))
    assert(retrievedIssues.exists(_.diagnostic.message == "Warning 1"))
  }

  test("push method with external filter applies write-time filtering") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredManager                     = new IssueLogger(Some(externalFilter))

    val issues = List(
      createErrorIssue(externalPath, "External error"),
      createWarningIssue(externalPath, "External warning") // Should be filtered out
    )

    filteredManager.push(externalPath, issues)

    val retrievedIssues = filteredManager.issuesForFilesInternal(
      Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )
    assert(retrievedIssues.size == 1)
    assert(retrievedIssues.head.diagnostic.message == "External error")
  }

  test("replaceUnusedIssues method replaces only unused issues") {
    issuesManager.add(createErrorIssue(testPath, "Error 1"))
    issuesManager.add(createUnusedIssue(testPath, "Unused 1"))
    issuesManager.add(createWarningIssue(testPath, "Warning 1"))

    val newUnusedIssues = Seq(createUnusedIssue(testPath, "New Unused 1"))
    IssueProviderOps.replaceUnusedIssues(issuesManager, testPath, newUnusedIssues)

    val allIssues = issuesManager.issuesForFilesInternal(
      Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )

    assert(allIssues.size == 3)
    assert(allIssues.exists(_.diagnostic.message == "Error 1"))
    assert(allIssues.exists(_.diagnostic.message == "Warning 1"))
    assert(allIssues.exists(_.diagnostic.message == "New Unused 1"))
    assert(!allIssues.exists(_.diagnostic.message == "Unused 1"))
  }

  test("replaceUnusedIssues with external filter applies write-time filtering") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredManager                     = new IssueLogger(Some(externalFilter))

    filteredManager.add(createErrorIssue(externalPath, "External error"))

    val newUnusedIssues = Seq(
      createUnusedIssue(externalPath, "External unused") // Warning category, should be filtered
    )

    IssueProviderOps.replaceUnusedIssues(filteredManager, externalPath, newUnusedIssues)

    val allIssues = filteredManager.issuesForFilesInternal(
      Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )
    assert(allIssues.size == 1) // Only the error should remain
    assert(allIssues.head.diagnostic.message == "External error")
  }

  test("replaceProviderIssues method replaces issues from specific provider") {
    val customProvider = "custom-provider"

    issuesManager.add(createErrorIssue(testPath, "Default error"))
    issuesManager.add(createProviderIssue(testPath, "Custom error 1", customProvider))
    issuesManager.add(createProviderIssue(testPath, "Custom error 2", customProvider))

    val newCustomIssues = Seq(createProviderIssue(testPath, "New custom error", customProvider))
    IssueProviderOps.replaceProviderIssues(issuesManager, customProvider, testPath, newCustomIssues)

    val allIssues = issuesManager.issuesForFilesInternal(
      Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )

    assert(allIssues.size == 2)
    assert(allIssues.exists(_.diagnostic.message == "Default error"))
    assert(allIssues.exists(_.diagnostic.message == "New custom error"))
    assert(!allIssues.exists(_.diagnostic.message == "Custom error 1"))
    assert(!allIssues.exists(_.diagnostic.message == "Custom error 2"))
  }

  test("replaceProviderIssues with external filter applies write-time filtering") {
    val externalFilter: PathLike => Boolean = path => path.toString.contains("external")
    val filteredManager                     = new IssueLogger(Some(externalFilter))
    val customProvider                      = "custom-provider"

    filteredManager.add(createErrorIssue(externalPath, "External error"))

    val newProviderIssues = Seq(
      createProviderIssue(
        externalPath,
        "Provider error",
        customProvider
      ), // Error, should be stored
      Issue(
        externalPath,
        Diagnostic(WARNING_CATEGORY, location, "Provider warning"),
        customProvider
      ) // Warning, should be filtered
    )

    IssueProviderOps.replaceProviderIssues(filteredManager, customProvider, externalPath, newProviderIssues)

    val allIssues = filteredManager.issuesForFilesInternal(
      Array(externalPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )
    assert(allIssues.size == 2) // Original error + provider error
    assert(allIssues.exists(_.diagnostic.message == "External error"))
    assert(allIssues.exists(_.diagnostic.message == "Provider error"))
    assert(!allIssues.exists(_.diagnostic.message == "Provider warning"))
  }

  test("clearProviderIssues method removes issues from specific provider") {
    val customProvider = "custom-provider"

    issuesManager.add(createErrorIssue(testPath, "Default error"))
    issuesManager.add(createProviderIssue(testPath, "Custom error", customProvider))

    IssueProviderOps.clearProviderIssues(issuesManager, testPath)

    val allIssues = issuesManager.issuesForFilesInternal(
      Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )

    assert(allIssues.size == 1)
    assert(allIssues.head.diagnostic.message == "Default error")
  }

  test("hasSyntaxIssues method detects syntax issues") {
    assert(!IssueAnalysis.hasSyntaxIssues(issuesManager, testPath))

    issuesManager.add(createErrorIssue(testPath, "Regular error"))
    assert(!IssueAnalysis.hasSyntaxIssues(issuesManager, testPath))

    issuesManager.add(createSyntaxIssue(testPath, "Syntax error"))
    assert(IssueAnalysis.hasSyntaxIssues(issuesManager, testPath))
  }

  test("change tracking works correctly") {
    assert(issuesManager.hasUpdatedIssues.isEmpty)

    issuesManager.add(createErrorIssue(testPath, "Error"))
    assert(issuesManager.hasUpdatedIssues.contains(testPath.toString))

    issuesManager.ignoreUpdatedIssues(testPath.toString)
    assert(!issuesManager.hasUpdatedIssues.contains(testPath.toString))
  }

  test("getMissing method tracks missing category issues") {
    issuesManager.add(createMissingIssue(testPath, "Missing dependency"))
    issuesManager.add(createErrorIssue(testPath, "Regular error"))

    val missing = IssueAnalysis.getMissing(issuesManager)
    assert(missing.contains(testPath))

    // Should clear missing tracking after retrieval, but only if still missing issues exist
    val missingAgain = IssueAnalysis.getMissing(issuesManager)
    // The behavior depends on whether missing issues still exist - let's just check it works
    assert(missingAgain.contains(testPath))
  }

  test("issuesForFileLocation method filters by location") {
    val location1 = Location(1, 2, 1, 5) // Small issue location
    val location2 = Location(2, 0, 2, 10)

    issuesManager.add(Issue(testPath, Diagnostic(ERROR_CATEGORY, location1, "Error 1")))
    issuesManager.add(Issue(testPath, Diagnostic(ERROR_CATEGORY, location2, "Error 2")))

    // Create a search location that contains location1 (1,2 to 1,5)
    val searchLocation = new io.github.apexdevtools.api.IssueLocation {
      override def startLineNumber(): Int = 1
      override def startCharOffset(): Int = 0
      override def endLineNumber(): Int   = 1
      override def endCharOffset(): Int   = 10
    }

    val matchingIssues = issuesManager.issuesForFileLocationInternal(testPath, searchLocation)

    assert(matchingIssues.length == 1)
    assert(matchingIssues.head.message() == "Error 1")
  }

  test("getDiagnostics method returns diagnostics for path") {
    issuesManager.add(createErrorIssue(testPath, "Error 1"))
    issuesManager.add(createWarningIssue(testPath, "Warning 1"))

    val diagnostics = issuesManager.getDiagnostics(testPath)

    assert(diagnostics.size == 2)
    assert(diagnostics.exists(_.message == "Error 1"))
    assert(diagnostics.exists(_.message == "Warning 1"))
  }

  test("log method delegates to add method") {
    issuesManager.log(createErrorIssue(testPath, "Logged error"))

    val issues = issuesManager.issuesForFilesInternal(
      Array(testPath),
      includeWarnings = true,
      maxIssuesPerFile = 0
    )
    assert(issues.size == 1)
    assert(issues.head.diagnostic.message == "Logged error")
  }
}
