/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class WhileTest extends AnyFunSuite with TestHelper {

  test("Bad conditional") {
    typeDeclaration("public class Dummy {{ while (foo) {} }}")
    assert(
      dummyIssues == "Missing: line 1 at 29-32: No variable or type found for 'foo' on 'Dummy'\n"
    )
  }

  test("Non boolean conditional") {
    typeDeclaration("public class Dummy {{ while ('') {} }}")
    assert(
      dummyIssues == "Error: line 1 at 29-31: While expression should return a 'System.Boolean' instance, not a 'System.String' instance\n"
    )
  }

  test("Null boolean conditional") {
    typeDeclaration("public class Dummy {{ while (null) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 29-33: While expression should return a 'System.Boolean' instance, not a 'null' instance\n"
    )
  }

  test("Static boolean conditional") {
    typeDeclaration("public class Dummy {{ while (Boolean) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 29-36: While expression should return a 'System.Boolean' instance, not a 'System.Boolean' type\n"
    )
  }

  test("Single statement") {
    happyTypeDeclaration("public class Dummy {{ while (true) System.debug(''); }}")
  }

  test("Single block") {
    happyTypeDeclaration("public class Dummy {{ while (true) {System.debug('');} }}")
  }
}
