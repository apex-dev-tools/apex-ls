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
        "Error: line 1 at 26-67: For loop can only have iterable types, not 'System.Iterator<System.Integer>'\n"
    )
  }

  test("for-each iterable") {
    happyTypeDeclaration(
      "public class Dummy {{ Iterable<Integer> it = new List<Integer>(); for(Long a : it) {} }}"
    )
  }

  test("for-each custom iterable") {
    typeDeclarations(
      Map(
        "NumIterable.cls" ->
          """public class NumIterable implements Iterable<Integer> {
            |  public Iterator<Integer> iterator(){
            |    return new List<Integer>().iterator();
            |  }
            |}""".stripMargin,
        "Dummy.cls" -> "public class Dummy {{ NumIterable nums = new NumIterable(); for(Long a : nums) {} }}"
      )
    )
    assert(!hasIssues)
  }

  test("for-each extended custom iterable interface") {
    typeDeclarations(
      Map(
        "NumIterable.cls" ->
          """public class NumIterable implements IterAPI {
            |  public interface IterAPI extends Iterable<Integer> {}
            |
            |  public Iterator<Integer> iterator(){
            |    return new List<Integer>().iterator();
            |  }
            |}""".stripMargin,
        "Dummy.cls" -> "public class Dummy {{ NumIterable nums = new NumIterable(); for(Long a : nums) {} }}"
      )
    )
    assert(!hasIssues)
  }

  test("for-each extended custom iterable class") {
    typeDeclarations(
      Map(
        "NumIterable.cls" ->
          """public virtual class NumIterable implements Iterable<Integer> {
            |  public Iterator<Integer> iterator(){
            |    return new List<Integer>().iterator();
            |  }
            |}""".stripMargin,
        "NumIterable2.cls" -> "public class NumIterable2 extends NumIterable {}",
        "Dummy.cls" -> "public class Dummy {{ NumIterable2 nums = new NumIterable2(); for(Long a : nums) {} }}"
      )
    )
    assert(!hasIssues)
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
        "Error: line 1 at 26-67: For loop can only have iterable types, not 'System.Integer'\n"
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
        "Error: line 1 at 26-73: For loop can only have iterable types, not 'System.Integer'\n"
    )
  }

  test("for-each list Database.query") {
    happyTypeDeclaration("public class Dummy {{ for(List<Account> a : Database.query('')) {} }}")
  }

  test("for-each list getSObjects platform method - GitHub issue #328") {
    typeDeclaration(
      "public class Dummy {{ Account parent = new Account(); for(List<SObject> cursorBlock : parent.getSObjects('Contacts')) {} }}"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 72-83: Incompatible types in assignment, from 'System.SObject' to 'System.List<System.SObject>'\n"
    )
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

  test("for condition no block") {
    happyTypeDeclaration("public class Dummy {{ for(;;); }}")
  }
}
