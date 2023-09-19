/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class DoWhileTest extends AnyFunSuite with TestHelper {

  test("Bad conditional") {
    typeDeclaration("public class Dummy {{ do {} while (foo); }}")
    assert(
      dummyIssues == "Missing: line 1 at 35-38: No variable or type found for 'foo' on 'Dummy'\n"
    )
  }

  test("Non boolean conditional") {
    typeDeclaration("public class Dummy {{ do {} while (''); }}")
    assert(
      dummyIssues == "Error: line 1 at 35-37: While expression should return a Boolean value, not a 'System.String'\n"
    )
  }

  test("Null boolean conditional") {
    typeDeclaration("public class Dummy {{ do {} while (null); }}")
    assert(
      dummyIssues == "Error: line 1 at 35-39: While expression should return a Boolean value, not a 'null'\n"
    )
  }

  test("Single statement") {
    // TODO: This should fail a block is required, apex-parser is over general
    happyTypeDeclaration("public class Dummy {{ do System.debug(''); while (true); }}")
  }

  test("Single block") {
    happyTypeDeclaration("public class Dummy {{ do {System.debug('');} while (true);  }}")
  }

}
