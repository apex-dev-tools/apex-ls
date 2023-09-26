/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ForTest extends AnyFunSuite with TestHelper {

  test("for-each non-iterable") {
    typeDeclaration("public class Dummy {{ for(Integer a : new Set<Integer>().iterator()) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 26-67: For loop can only iterate over Lists or Sets, not 'System.Iterator<System.Integer>'\n"
    )
  }

  test("for-each assignable") {
    happyTypeDeclaration("public class Dummy {{ for(Long a : new List<Integer>()) {} }}")
  }

  test("for-each non-assignable") {
    typeDeclaration("public class Dummy {{ for(Long a : new List<String>()) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 31-32: Incompatible types in assignment, from 'System.String' to 'System.Long'\n"
    )
  }

  test("for-each assignable to Object") {
    happyTypeDeclaration("public class Dummy {{ for(Object a : new List<Dummy>()) {} }}")
  }

  test("for-each assignable to SObject") {
    happyTypeDeclaration("public class Dummy {{ for(SObject a : new List<Account>()) {} }}")
  }

  test("for-each non-assignable to SObject") {
    typeDeclaration("public class Dummy {{ for(SObject a : new List<Dummy>()) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 34-35: Incompatible types in assignment, from 'Dummy' to 'System.SObject'\n"
    )
  }

  test("for-each SOQL assignable") {
    happyTypeDeclaration("public class Dummy {{ for(Account a : [Select Id From Account]) {} }}")
  }

  test("for-each SOQL assignable to SObject") {
    happyTypeDeclaration("public class Dummy {{ for(SObject a : [Select Id From Account]) {} }}")
  }

  test("for-each SOQL assignable to Object") {
    happyTypeDeclaration("public class Dummy {{ for(Object a : [Select Id From Account]) {} }}")
  }

  test("for-each SOQL non-assignable") {
    typeDeclaration("public class Dummy {{ for(Contact a : [Select Id From Account]) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 34-35: Incompatible types in assignment, from 'Schema.Account' to 'Schema.Contact'\n"
    )
  }

  test("for-each SOQL aggregate") {
    typeDeclaration("public class Dummy {{ for(Account a : [Select Count(Id) From Account]) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 34-35: Incompatible types in assignment, from 'Schema.AggregateResult' to 'Schema.Account'\n"
    )
  }

  test("for-each SOQL count") {
    typeDeclaration("public class Dummy {{ for(Account a : [Select Count() From Account]) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 26-67: For loop can only iterate over Lists or Sets, not 'System.Integer'\n"
    )
  }

  test("for-each Database.query") {
    happyTypeDeclaration("public class Dummy {{ for(Account a : Database.query('')) {} }}")
  }

  test("for-each list SOQL assignable") {
    happyTypeDeclaration(
      "public class Dummy {{ for(List<Account> a : [Select Id From Account]) {} }}"
    )
  }

  test("for-each list SOQL non-assignable") {
    typeDeclaration("public class Dummy {{ for(List<Contact> a : [Select Id From Account]) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 40-41: Incompatible types in assignment, from 'Schema.Account' to 'Schema.Contact'\n"
    )
  }

  test("for-each list SOQL aggregate") {
    typeDeclaration(
      "public class Dummy {{ for(List<Account> a : [Select Count(Id) From Account]) {} }}"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 40-41: Incompatible types in assignment, from 'Schema.AggregateResult' to 'Schema.Account'\n"
    )
  }

  test("for-each list SOQL count") {
    typeDeclaration(
      "public class Dummy {{ for(List<Account> a : [Select Count() From Account]) {} }}"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 26-73: For loop can only iterate over Lists or Sets, not 'System.Integer'\n"
    )
  }

  test("for-each list Database.query") {
    happyTypeDeclaration("public class Dummy {{ for(List<Account> a : Database.query('')) {} }}")
  }

  test("for condition empty") {
    happyTypeDeclaration("public class Dummy {{ for(;;) {} }}")
  }

  test("for condition boolean") {
    happyTypeDeclaration("public class Dummy {{ for(; true;) {} }}")
  }

  test("for condition non-boolean") {
    typeDeclaration("public class Dummy {{ for(; 1;) {} }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 28-29: For condition expression should return a 'System.Boolean' instance, not a 'System.Integer' instance\n"
    )
  }
}
