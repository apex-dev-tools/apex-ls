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
package com.nawforce.pkgforce.modifiers

import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite

class ModifierLoggerTest extends AnyFunSuite {

  test("Empty logger has correct initial state") {
    val logger = new ModifierLogger()

    assert(logger.isEmpty)
    assert(logger.issues.isEmpty)
  }

  test("Logger with error issues is not empty") {
    val logger  = new ModifierLogger()
    val context = new LogEntryContext(Location(1, 0, 1, 5), Path("Test.cls"))

    logger.logError(context, "Test error message")

    assert(!logger.isEmpty)
    assert(logger.issues.length == 1)
    assert(logger.issues.head.isError)
    assert(logger.issues.head.diagnostic.message == "Test error message")
    assert(logger.issues.head.path == Path("Test.cls"))
  }

  test("Logger with warning issues is not empty") {
    val logger  = new ModifierLogger()
    val context = new LogEntryContext(Location(2, 0, 2, 8), Path("Test.cls"))

    logger.logWarning(context, "Test warning message")

    assert(!logger.isEmpty)
    assert(logger.issues.length == 1)
    assert(!logger.issues.head.isError)
    assert(logger.issues.head.diagnostic.message == "Test warning message")
    assert(logger.issues.head.path == Path("Test.cls"))
  }

  test("Logger accumulates multiple issues") {
    val logger   = new ModifierLogger()
    val context1 = new LogEntryContext(Location(1, 0, 1, 5), Path("Test.cls"))
    val context2 = new LogEntryContext(Location(2, 0, 2, 8), Path("Test.cls"))
    val context3 = new LogEntryContext(Location(3, 0, 3, 10), Path("Other.cls"))

    logger.logError(context1, "First error")
    logger.logWarning(context2, "First warning")
    logger.logError(context3, "Second error")

    assert(!logger.isEmpty)
    assert(logger.issues.length == 3)

    val issues = logger.issues
    assert(issues(0).diagnostic.message == "First error")
    assert(issues(0).isError)
    assert(issues(1).diagnostic.message == "First warning")
    assert(!issues(1).isError)
    assert(issues(2).diagnostic.message == "Second error")
    assert(issues(2).isError)
    assert(issues(2).path == Path("Other.cls"))
  }

  test("Logger preserves location information correctly") {
    val logger   = new ModifierLogger()
    val location = Location(5, 10, 5, 20)
    val context  = new LogEntryContext(location, Path("LocationTest.cls"))

    logger.logError(context, "Location test")

    val issue = logger.issues.head
    assert(issue.diagnostic.location == location)
    assert(issue.diagnostic.location.startLine == 5)
    assert(issue.diagnostic.location.startPosition == 10)
    assert(issue.diagnostic.location.endLine == 5)
    assert(issue.diagnostic.location.endPosition == 20)
  }

  test("Issues method returns empty array for empty logger") {
    val logger = new ModifierLogger()

    val issues = logger.issues
    assert(issues.isEmpty)
    assert(issues.length == 0)
  }

  test("Issues method returns issues in order they were logged") {
    val logger  = new ModifierLogger()
    val context = new LogEntryContext(Location(1, 0, 1, 5), Path("Test.cls"))

    logger.logError(context, "First")
    logger.logWarning(context, "Second")
    logger.logError(context, "Third")

    val issues = logger.issues
    assert(issues(0).diagnostic.message == "First")
    assert(issues(1).diagnostic.message == "Second")
    assert(issues(2).diagnostic.message == "Third")
  }
}
