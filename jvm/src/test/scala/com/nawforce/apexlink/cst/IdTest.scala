/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class IdTest extends AnyFunSuite with TestHelper {

  test("Local var illegal name") {
    typeDeclaration("public class Dummy {{ String _i; }}")
    assert(
      dummyIssues == "Error: line 1 at 29-31: '_i' is not legal identifier in Apex, identifiers can not start or end with '_'\n"
    )
  }

  test("Local var reserved name") {
    typeDeclaration("public class Dummy {{ String limit; }}")
    assert(dummyIssues == "Error: line 1 at 29-34: 'limit' is a reserved identifier in Apex\n")
  }

  test("Field illegal name") {
    typeDeclaration("public class Dummy {String _i;}")
    assert(
      dummyIssues == "Error: line 1 at 27-29: '_i' is not legal identifier in Apex, identifiers can not start or end with '_'\n"
    )
  }

  test("Field reserved name") {
    typeDeclaration("public class Dummy {String limit;}")
    assert(dummyIssues == "Error: line 1 at 27-32: 'limit' is a reserved identifier in Apex\n")
  }

  test("For-control var illegal name") {
    typeDeclaration("public class Dummy {{ for(Integer _i; _i<0; _i++) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 34-36: '_i' is not legal identifier in Apex, identifiers can not start or end with '_'\n"
    )
  }

  test("For-control var reserved name") {
    typeDeclaration("public class Dummy {{ for(Integer limit; limit<0; limit++) {} }}")
    assert(dummyIssues == "Error: line 1 at 34-39: 'limit' is a reserved identifier in Apex\n")
  }

  test("Method call illegal name") {
    typeDeclaration("public class Dummy {void _a(){} void f2() {_a();} }")
    assert(
      dummyIssues == "Error: line 1 at 25-27: '_a' is not legal identifier in Apex, identifiers can not start or end with '_'\n"
    )
  }

  test("Method call reserved name") {
    typeDeclaration("public class Dummy {void limit(){} void f2() {limit();} }")
    assert(dummyIssues == "Error: line 1 at 25-30: 'limit' is a reserved identifier in Apex\n")
  }

  test("Method call reserved name, but legal") {
    typeDeclaration("public class Dummy {void integer(){} void f2() {integer();} }")
    assert(
      dummyIssues == "Warning: line 1 at 25-32: 'integer' is currently a legal method name but is a reserved identifier in Apex so should be avoided\n"
    )
  }

  test("Method call illegal param") {
    typeDeclaration("public class Dummy {void f1(Integer _a){} void f2() {f1(1);} }")
    assert(
      dummyIssues == "Error: line 1 at 36-38: '_a' is not legal identifier in Apex, identifiers can not start or end with '_'\n"
    )
  }

  test("Method call reserved param") {
    typeDeclaration("public class Dummy {void f1(Integer limit){} void f2() {f1(1);} }")
    assert(dummyIssues == "Error: line 1 at 36-41: 'limit' is a reserved identifier in Apex\n")
  }

}
