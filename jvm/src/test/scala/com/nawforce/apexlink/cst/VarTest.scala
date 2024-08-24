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

class VarTest extends AnyFunSuite with TestHelper {

  test("Duplicate local var") {
    typeDeclaration("public class Dummy { void func() {String a; String a;}}")
    assert(dummyIssues == "Error: line 1 at 51-52: Duplicate variable 'a'\n")
  }

  test("Duplicate local var, same declaration") {
    typeDeclaration("public class Dummy { void func() {String a, a;}}")
    assert(dummyIssues == "Error: line 1 at 44-45: Duplicate variable 'a'\n")
  }

  test("Duplicate local var, nested") {
    typeDeclaration("public class Dummy { void func() {String a; while (true) {String a;}}}")
    assert(dummyIssues == "Error: line 1 at 65-66: Duplicate variable 'a'\n")
  }

  test("Duplicate for vars") {
    typeDeclaration(
      "public class Dummy { void func() {for (Integer i=0; i<0; i++){} for(Integer i=0; i<0; i++){} }}"
    )
    assert(!hasIssues)
  }

  test("Shadow local var") {
    typeDeclaration("public class Dummy { String a; void func() {String a;}}")
    assert(
      dummyIssues
        .startsWith("Warning: line 1 at 44-52: Local variable is hiding class field 'a', see")
    )
  }

  test("Shadow local var, inner class") {
    typeDeclaration("public class Dummy { class Dummy2 {String a; void func() {String a;}}}")
    assert(
      dummyIssues
        .startsWith("Warning: line 1 at 58-66: Local variable is hiding class field 'a', see")
    )
  }

  test("Shadow local var, not extending") {
    typeDeclaration("public class Dummy {String a; class Dummy2 { void func() {String a;}}}")
    assert(!hasIssues)
  }

  test("Shadow local var, extending") {
    typeDeclaration(
      "public virtual class Dummy {String a; class Dummy2 extends Dummy { void func() {String a;}}}"
    )
    assert(
      dummyIssues
        .startsWith("Warning: line 1 at 80-88: Local variable is hiding class field 'a', see ")
    )
  }

  test("Shadow local var static context") {
    typeDeclaration("public class Dummy { static String a; static void func() {String a;}}")
    assert(
      dummyIssues
        .startsWith("Warning: line 1 at 58-66: Local variable is hiding class field 'a', see")
    )
  }

  test("Shadow local var in a static function") {
    typeDeclaration("public class Dummy { String a; static void func() {String a;}}")
    assert(!hasIssues)
  }

  test("Reference local var") {
    typeDeclaration("public class Dummy { void func() {String a; String b = a;}}")
    assert(!hasIssues)
  }

  test("Static type in method argument") {
    typeDeclaration("public class Dummy { void func(Dummy d) {} void func2() {func(dummy);} }")
    assert(
      dummyIssues == "Missing: line 1 at 62-67: No variable or type found for 'dummy' on 'Dummy'\n"
    )
  }

  test("Static type in variable declaration") {
    typeDeclaration("public class Dummy {{ Dummy d = dummy; }}")
    assert(
      dummyIssues == "Error: line 1 at 28-37: Expecting instance for operation, not type 'Dummy'\n"
    )
  }

  test("Static type in variable assignment") {
    typeDeclaration("public class Dummy {{ Dummy d; d = dummy; }}")
    assert(
      dummyIssues == "Error: line 1 at 31-40: Expecting instance for operation, not type 'Dummy'\n"
    )
  }

}
