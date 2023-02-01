/*
 * Copyright (c) 2023 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class FinalTest extends AnyFunSuite with TestHelper {

  // Apex does not always enforce 'final', we are trying to do better ;-)
  // There are likely additional cases where we should enforce read-only, here we only have known cases to date

  test("Initialised local") {
    typeDeclaration("public class Dummy {{final String a=''; a = 'a';}}")

    assert(
      dummyIssues ==
        "Warning: line 1 at 40-47: Variable 'a' can not be assigned to, it is final\n"
    )
  }

  test("Non-Initialised local") {
    typeDeclaration("public class Dummy {{final String a; a = 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 37-44: Variable 'a' can not be assigned to, it is final\n"
    )
  }

  test("Initialised Instance Field") {
    typeDeclaration("public class Dummy {final String a=''; void func(){a += 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 51-59: Field 'a' can not be assigned to, it is final\n"
    )
  }

  test("Non-Initialised Instance Field") {
    typeDeclaration("public class Dummy {final String a; void func(){a += 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 48-56: Field 'a' can not be assigned to, it is final\n"
    )
  }

  test("Initialised Static Field") {
    typeDeclaration("public class Dummy {final static String a=''; void func(){a += 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 58-66: Field 'a' can not be assigned to, it is final\n"
    )
  }

  test("Non-Initialised Static Field") {
    typeDeclaration("public class Dummy {final static String a; void func(){a += 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 55-63: Field 'a' can not be assigned to, it is final\n"
    )
  }

  test("Instance field assignment allowed in instance init") {
    typeDeclaration("public class Dummy {final String a; {a += 'a';}}")
    assert(dummyIssues.isEmpty)
  }

  test("Static field assignment allowed in static init") {
    typeDeclaration("public class Dummy {final static String a; static {a += 'a';}}")
    assert(dummyIssues.isEmpty)
  }

  test("Static field assignment not allowed in instance init") {
    typeDeclaration("public class Dummy {final static String a; {a += 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 44-52: Field 'a' can not be assigned to, it is final\n"
    )
  }

  test("Instance field assignment allowed in constructor") {
    typeDeclaration("public class Dummy {final String a; Dummy(){a += 'a';}}")
    assert(dummyIssues.isEmpty)
  }

  test("Instance field assignment allowed in subtype constructor") {
    typeDeclaration("""public class Dummy {
        |  virtual class Foo {protected final String a;}
        |  class Bar extends Foo { Bar(){ a = '';} }
        |}
        |""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

  test("Static field assignment not allowed in constructor") {
    typeDeclaration("public class Dummy {final static String a; Dummy(){a += 'a';}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 51-59: Field 'a' can not be assigned to, it is final\n"
    )
  }

  test("Parameter not assignable in instance method") {
    typeDeclaration("public class Dummy {void func(final Integer a) {a &= 1;}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 48-54: Parameter 'a' can not be assigned to, it is final\n"
    )
  }

  test("Parameter not assignable in static method") {
    typeDeclaration("public class Dummy {static void func(final Integer a) {a &= 1;}}")
    assert(
      dummyIssues ==
        "Warning: line 1 at 55-61: Parameter 'a' can not be assigned to, it is final\n"
    )
  }

  test("Enum constant not assignable") {
    typeDeclaration("public class Dummy {enum MyEnum {A} void func(){MyEnum.A = null;} }")
    assert(
      dummyIssues ==
        "Warning: line 1 at 48-63: Field 'A' can not be assigned to, it is final\n"
    )
  }

  test("Switch allows assignment") {
    typeDeclaration(
      "public class Dummy {{SObject a;switch on a {when Account record {record = null;}} }}"
    )
    assert(dummyIssues.isEmpty)
  }

}
