/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ReturnTest extends AnyFunSuite with TestHelper {

  test("Return void") {
    typeDeclaration("public class Dummy { void fn(){} }")
    assert(dummyIssues.isEmpty)
  }

  test("Return primitive") {
    typeDeclaration("public class Dummy { String fn(){ return 'test'; }}")
    assert(dummyIssues.isEmpty)
  }

  test("Return assignable") {
    typeDeclaration("public class Dummy { Long fn(){ return 1; }}")
    assert(dummyIssues.isEmpty)
  }

  test("Return object") {
    typeDeclarations(
      Map(
        "A.cls"     -> "public class A { }",
        "Dummy.cls" -> "public class Dummy { A fn(){ return new A(); } }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Return extended object") {
    typeDeclarations(
      Map(
        "A.cls"     -> "public class A { }",
        "B.cls"     -> "public class B extends A { }",
        "Dummy.cls" -> "public class Dummy { A fn(){ return new B(); } }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Return missing value") {
    typeDeclaration("public class Dummy { String fn(){ return; } }")
    assert(dummyIssues == "Error: line 1 at 34-41: Missing return value of type 'System.String'\n")
  }

  test("Return expecting void") {
    typeDeclaration("public class Dummy { void fn(){ return 0; } }")
    assert(
      dummyIssues == "Error: line 1 at 32-41: Incompatible return type, 'System.Integer' is not assignable to 'void'\n"
    )
  }

  test("Return mismatched value") {
    typeDeclaration("public class Dummy { String fn(){ return false; } }")
    assert(
      dummyIssues == "Error: line 1 at 34-47: Incompatible return type, 'System.Boolean' is not assignable to 'System.String'\n"
    )
  }

  test("Return SOQL") {
    typeDeclaration("public class Dummy { List<Account> fn(){ return [SELECT Id FROM Account]; } }")
    assert(dummyIssues.isEmpty)
  }

  test("Return generic") {
    typeDeclaration("public class Dummy { List<String> fn(){ return new List<String>(); } }")
    assert(dummyIssues.isEmpty)
  }

  test("Return mismatched generic") {
    typeDeclaration("public class Dummy { List<String> fn(){ return new List<Integer>(); } }")
    assert(
      dummyIssues == "Error: line 1 at 40-67: Incompatible return type, 'System.List<System.Integer>' is not assignable to 'System.List<System.String>'\n"
    )
  }

  test("Return SObject generic") {
    typeDeclaration("public class Dummy { List<SObject> fn(){ return new List<Account>(); } }")
    assert(dummyIssues.isEmpty)
  }

  test("Return Map SObject generic") {
    typeDeclaration(
      "public class Dummy { Map<Id, Account> fn(){ return new Map<Id, SObject>(); } }"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Return QueryLocator as Iterable") {
    typeDeclaration(
      "public class Dummy { Iterable<Account> fn(){ return Database.getQueryLocator('SELECT Id FROM Account'); } }"
    )
    assert(dummyIssues.isEmpty)
  }

}
