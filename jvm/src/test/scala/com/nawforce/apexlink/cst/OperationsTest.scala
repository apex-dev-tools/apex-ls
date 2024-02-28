/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class OperationsTest extends AnyFunSuite with TestHelper {

  test("String prefix bug on SObject") {
    happyTypeDeclaration("public class Dummy {{Object a;  String b = '' + + a;}}")
  }

  test("Date addition") {
    happyTypeDeclaration("public class Dummy {{Date a;  Date b = a + 1;}}")
  }

  test("Date subtraction") {
    happyTypeDeclaration("public class Dummy {{Date a;  Date b = a - 12l;}}")
  }

  test("Datetime addition") {
    happyTypeDeclaration("public class Dummy {{Datetime a;  Datetime b = a + 1;}}")
  }

  test("Datetime subtraction") {
    happyTypeDeclaration("public class Dummy {{Datetime a;  Datetime b = a - 12l;}}")
  }

  test("Time addition") {
    happyTypeDeclaration("public class Dummy {{Time a;  Time b = a + 1;}}")
  }

  test("Time subtraction") {
    happyTypeDeclaration("public class Dummy {{Time a;  Time b = a - 12l;}}")
  }

  test("List assignment to Object") {
    happyTypeDeclaration("public class Dummy {{Object a; a = new List<String>{''}; }}")
  }

  test("sObject List to Specific Object List") {
    happyTypeDeclaration("public class Dummy {{List<Account> a; a = new List<sObject>(); }}")
  }

  test("Alternative not equal") {
    happyTypeDeclaration("public class Dummy {{Boolean a; a = 1 <> 2; }}")
  }

  test("Split comparator") {
    happyTypeDeclaration("public class Dummy {{Boolean a; a = 1 < = 2; }}")
  }

  test("Bitwise Integer to Integer") {
    happyTypeDeclaration("public class Dummy {{Integer a; a = a & 2; }}")
  }

  test("Bitwise Long to Integer") {
    happyTypeDeclaration("public class Dummy {{Long a; a = a | 2; }}")
  }

  test("Bitwise Boolean to Boolean") {
    happyTypeDeclaration("public class Dummy {{Boolean a; a = a ^ false; }}")
  }

  test("Bitwise Assigment Integer to Integer") {
    happyTypeDeclaration("public class Dummy {{Integer a; a ^= a; }}")
  }

  test("Bitwise Assigment Integer to Long") {
    happyTypeDeclaration("public class Dummy {{Long a; a &= 2; }}")
  }

  test("Bitwise Assigment Long to Integer") {
    typeDeclaration("public class Dummy {{Integer a; a |= 22l; }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 32-40: Bitwise operation only allowed between Integer, Long & Boolean types, not 'System.Integer' and 'System.Long'\n"
    )
  }

  test("Bitwise Shift in Array Index") {
    happyTypeDeclaration("public class Dummy {{List<String> a; Integer b; String c = a[b << 2];}}")
  }

  test("String concat assignment") {
    happyTypeDeclaration("public class Dummy {{String a; a += 12;}}")
  }

  test("Enum value assignment") {
    happyTypeDeclaration("public class Dummy {enum MyEnum{A} {MyEnum a; a = Dummy.MyEnum.A;}}")
  }

  test("Nested ternary") {
    happyTypeDeclaration(
      "public class Dummy {{ String a; a = (1==0) ? 'a' : (1==-1) ? null : 'b';}}"
    )
  }

  test("Ternary common base") {
    happyTypeDeclaration(
      "public virtual class Dummy {class A extends Dummy {} class B extends Dummy {} { A a; B b; Object c = 2>0 ? a : b;}}"
    )
  }

  test("Ternary common SObject") {
    happyTypeDeclaration(
      "public class Dummy {{List<SObject> a = true ? Database.query('') : [Select Count(Id) from Account];}}"
    )
  }

  test("Ternary SObjectType") {
    happyTypeDeclaration(
      "public virtual class Dummy {{ SObjectType a = 2>0 ? Account.SObjectType : Contact.SObjectType;}}"
    )
  }

  test("Object exact equality") {
    happyTypeDeclaration("public class Dummy {{Object a,b; Boolean c = a===b;}}")
  }

  test("SObject exact equality") {
    happyTypeDeclaration("public class Dummy {{Account a,b; Boolean c = a===b;}}")
  }

  test("Apex Reference exact equality") {
    happyTypeDeclaration("public class Dummy {{Dummy a,b; Boolean c = a===b;}}")
  }

  test("Primitive exact equality") {
    typeDeclaration("public class Dummy {{Integer a,b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 46-51: Exact equality/inequality is not supported between primitive types 'System.Integer' & 'System.Integer'\n"
    )
  }

  test("Primitive & Object exact equality") {
    happyTypeDeclaration("public class Dummy {{Integer a; Object b; Boolean c = a===b;}}")
  }

  test("Primitive & Object exact equality (reversed)") {
    happyTypeDeclaration("public class Dummy {{Object a; Integer b; Boolean c = a===b;}}")
  }

  test("Primitive & SObject exact equality") {
    typeDeclaration("public class Dummy {{Integer a; Account b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 55-60: Comparing incompatible types 'System.Integer' and 'Schema.Account'\n"
    )
  }

  test("Primitive & SObject exact equality (reversed)") {
    typeDeclaration("public class Dummy {{Account a; Integer b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 55-60: Comparing incompatible types 'Schema.Account' and 'System.Integer'\n"
    )
  }

  test("Primitive & Apex Reference exact equality") {
    typeDeclaration("public class Dummy {{Integer a; Dummy b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 53-58: Comparing incompatible types 'System.Integer' and 'Dummy'\n"
    )
  }

  test("Primitive & Apex Reference exact equality (reversed)") {
    typeDeclaration("public class Dummy {{Dummy a; Integer b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 53-58: Comparing incompatible types 'Dummy' and 'System.Integer'\n"
    )
  }

  test("Mixed assignable primitive exact equality") {
    typeDeclaration("public class Dummy {{Integer a; Long b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 52-57: Exact equality/inequality is not supported between primitive types 'System.Integer' & 'System.Long'\n"
    )
  }

  test("Mixed assignable primitive exact equality (reversed)") {
    typeDeclaration("public class Dummy {{Long a; Integer b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 52-57: Exact equality/inequality is not supported between primitive types 'System.Long' & 'System.Integer'\n"
    )
  }

  test("Mixed non-assignable primitive exact equality") {
    typeDeclaration("public class Dummy {{Integer a; String b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 54-59: Comparing incompatible types 'System.Integer' and 'System.String'\n"
    )
  }

  test("Mixed non-assignable primitive exact equality (reversed)") {
    typeDeclaration("public class Dummy {{String a; Integer b; Boolean c = a===b;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 54-59: Comparing incompatible types 'System.String' and 'System.Integer'\n"
    )
  }

  test("Null coalesce") {
    happyTypeDeclaration("public class Dummy {{ String a = null; String b = a ?? 'test';}}")
  }

  test("Null coalesce nested") {
    happyTypeDeclaration(
      "public class Dummy {{ String a = null; String b; String c = 'test'; String d = a ?? b ?? c;}}"
    )
  }

  test("Null coalesce common base") {
    happyTypeDeclaration(
      "public virtual class Dummy {class A extends Dummy {} class B extends Dummy {} { A a; B b = new B(); Object c = a ?? b;}}"
    )
  }

  test("Null coalesce SObject") {
    happyTypeDeclaration(
      "public class Dummy {{ Account def = new Account(name = 'Acme'); Account a = [SELECT Id FROM Account WHERE Id = ''] ?? def; }}"
    )
  }

  test("Null coalesce not assignable") {
    typeDeclaration("public class Dummy {{ String a = null; String b = a ?? false;}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 50-60: Incompatible types in conditional operation, 'System.String' and 'System.Boolean'\n"
    )
  }
}
