/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class CatchTest extends AnyFunSuite with TestHelper {

  test("Catch platform exception") {
    happyTypeDeclaration("public class Dummy {{ try {} catch (AssertException a) {} }}")
  }

  test("Catch custom exception") {
    typeDeclarations(
      Map(
        "MyException.cls" -> "public class MyException extends Exception {}",
        "Dummy.cls"       -> "public class Dummy {{ try {} catch (MyException a) {} }}"
      )
    )
    assert(!hasIssues)
  }

  test("Catch non-exception class errors") {
    typeDeclarations(
      Map(
        "MyClass.cls" -> "public class MyClass {}",
        "Dummy.cls"   -> "public class Dummy {{ try {} catch (MyClass a) {} }}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 36-43: Catch clause should catch an Exception instance, not a 'MyClass' instance\n"
    )
  }

  test("Catch platform type errors") {
    typeDeclaration("public class Dummy {{ try {} catch (String a) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 36-42: Catch clause should catch an Exception instance, not a 'String' instance\n"
    )
  }
}
