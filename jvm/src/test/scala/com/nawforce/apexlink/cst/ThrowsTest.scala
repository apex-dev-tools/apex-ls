/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ThrowsTest extends AnyFunSuite with TestHelper {

  test("Throw platform exception") {
    happyTypeDeclaration("public class Dummy {{ throw new AssertException(); }}")
  }

  test("Throw platform exception class errors") {
    typeDeclaration("public class Dummy {{ throw AssertException; }}")
    assert(
      dummyIssues == "Error: line 1 at 28-43: Only Exception objects may be thrown, not 'System.AssertException'\n"
    )
  }

  test("Throw custom exception") {
    typeDeclarations(
      Map(
        "MyException.cls" -> "public class MyException extends Exception {}",
        "Dummy.cls"       -> "public class Dummy {{ throw new MyException(); }}"
      )
    )
    assert(!hasIssues)
  }

  test("Throw custom exception class errors") {
    typeDeclarations(
      Map(
        "MyException.cls" -> "public class MyException extends Exception {}",
        "Dummy.cls"       -> "public class Dummy {{ throw MyException; }}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 28-39: Only Exception objects may be thrown, not 'MyException'\n"
    )
  }

  test("Throw non-exception class errors") {
    typeDeclarations(
      Map(
        "MyClass.cls" -> "public class MyClass {}",
        "Dummy.cls"   -> "public class Dummy {{ throw new MyClass(); }}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 28-41: Only Exception objects may be thrown, not 'MyClass'\n"
    )
  }

  test("Throw primitive errors") {
    typeDeclaration("public class Dummy {{ throw 'Hello'; }}")
    assert(
      dummyIssues == "Error: line 1 at 28-35: Only Exception objects may be thrown, not 'System.String'\n"
    )
  }

}
