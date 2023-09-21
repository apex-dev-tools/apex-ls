/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class RunAsTest extends AnyFunSuite with TestHelper {

  test("runAs with User") {
    happyTypeDeclaration("public class Dummy {{ User u; System.runAs(u) {} }}")
  }

  test("runAs with Version") {
    happyTypeDeclaration("public class Dummy {{ Version v; System.runAs(v) {} }}")
  }

  test("runAs with String") {
    typeDeclaration("public class Dummy {{ System.runAs('') {} }}")
    assert(
      dummyIssues == "Error: line 1 at 35-37: System.runAs expression should return one of 'Schema.User' or 'System.Version' instances, not a 'System.String' instance\n"
    )
  }

  test("runAs with SObject") {
    typeDeclaration("public class Dummy {{ Account a; System.runAs(a) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 46-47: System.runAs expression should return one of 'Schema.User' or 'System.Version' instances, not a 'Schema.Account' instance\n"
    )
  }

  test("runAs with multiple args") {
    typeDeclaration("public class Dummy {{ User u; System.runAs(u, u) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 30-51: System.runAs must be provided a User or Version argument, not 2 arguments\n"
    )
  }
}
