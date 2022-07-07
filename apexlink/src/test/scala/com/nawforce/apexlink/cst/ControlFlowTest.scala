/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ControlFlowTest extends AnyFunSuite with TestHelper {

  test("Simple method no issues") {
    typeDeclaration("public class Dummy { String fn(){ String s = ''; return s; } }")
    assert(dummyIssues.isEmpty)
  }

  test("Throws exception no issues") {
    typeDeclaration("public class Dummy { String fn(){ throw new NullPointerException(''); } }")
    assert(dummyIssues.isEmpty)
  }

  test("Method with block no issues") {
    typeDeclaration("""
      |public class Dummy { 
      | String fn(){
      |   String s = 'a';
      |   while (s == 'a') {
      |     s = 'b';
      |   }
      |   return s;
      | }
      |}""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

  test("Empty method requiring return") {
    typeDeclaration("public class Dummy { Object fn(){} }")
    assert(dummyIssues == "Error: line 1 at 32-34: Method does not return a value\n")
  }

  test("Root unreachable statements") {
    typeDeclaration("public class Dummy { Object fn(){ return 0; Integer i = 1; return i; } }")
    assert(
      dummyIssues == "Error: line 1 at 44-58: Unreachable block or statement\nError: line 1 at 59-68: Unreachable block or statement\n"
    )
  }

  test("Void unreachable statements") {
    typeDeclaration("public class Dummy { void fn(){ return; return; } }")
    assert(dummyIssues == "Error: line 1 at 40-47: Unreachable block or statement\n")
  }

  test("Nested unreachable statements") {
    typeDeclaration("""
      |public class Dummy {
      | String fn() {
      |   String s = 'a';
      |   if (s == 'a') {
      |     s = 'b';
      |     return s;
      |     s = 'c';
      |   } else {
      |     return s;
      |     if (s == 'c') {
      |       String s2 = 'd';
      |     }
      |   }
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 8 at 5-13: Unreachable block or statement\nError: line 11:5 to 13:6: Unreachable block or statement\n"
    )
  }

  // ifs

  test("IF missing branch") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true) {
      |     Object i;
      |   }
      | }
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 4:3 to 6:4: Code path does not return a value\n")
  }

  test("IF with double blocks") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true) {
      |     Object i;
      |   } else {
      |     Object j;
      |   }
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 5 at 5-14: Expected return statement\nError: line 7 at 5-14: Expected return statement\n"
    )
  }

  test("IF with statements") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true)
      |     Object i;
      |   else
      |     Object j;
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 5 at 5-14: Expected return statement\nError: line 7 at 5-14: Expected return statement\n"
    )
  }

  test("IF with mixed statement and block") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true) {
      |     Object i;
      |   } else
      |     Object j;
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 5 at 5-14: Expected return statement\nError: line 7 at 5-14: Expected return statement\n"
    )
  }

  test("IF with inverted mixed statement and block") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true)
      |     Object i;
      |   else {
      |     Object j;
      |   }
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 5 at 5-14: Expected return statement\nError: line 7 at 5-14: Expected return statement\n"
    )
  }

  test("IF chain of else statements") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true)
      |     return 1;
      |   else if (true)
      |     Object j;
      |   else
      |     Object k;
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 7 at 5-14: Expected return statement\nError: line 9 at 5-14: Expected return statement\n"
    )
  }

  test("IF chain of else statements missing branch") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true)
      |     Object i;
      |   else if (true)
      |     Object j;
      | }
      |}""".stripMargin)
    assert(
      dummyIssues == "Error: line 5 at 5-14: Expected return statement\nError: line 6:8 to 7:14: Code path does not return a value\n"
    )
  }

  test("IF nested blocks") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true) {
      |     if (true) {
      |       Object i;
      |     } else {
      |       return 0;
      |     }
      |   } else {
      |     return 2;
      |   }
      | }
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 6 at 7-16: Expected return statement\n")
  }

  test("IF causing unreachable code") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   if (true) {
      |     return 0;
      |   } else {
      |     return 1;
      |   }
      |   return null;
      | }
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 9 at 3-15: Unreachable block or statement\n")
  }

  // switch

  test("SWITCH block missing return") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   Integer i = 1;
      |   switch on i {
      |     when 0 {
      |       return null;
      |     }
      |     when 1 {}
      |     when else {
      |       throw new NullPointerException('');
      |     }
      |   }
      | }
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 9 at 12-14: Code path does not return a value\n")
  }

  test("SWITCH with default return not unreachable") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   Integer i = 1;
      |   switch on i {
      |     when 0 {
      |       return 1;
      |     }
      |     when 1 {
      |       return 2;
      |     }
      |   }
      |   return 3;
      | }
      |}""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

  // try

  test("TRY without catch no issues") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   try {
      |     return 0;
      |   } finally {}
      | }
      |}""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

  test("TRY with catch missing return or throw") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   try {
      |     return 0;
      |   } catch (Exception e) {}
      | }
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 6 at 25-27: Code path does not return a value\n")
  }

  test("TRY with multiple branches") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   try {
      |     return 0;
      |   } catch (QueryException q) {
      |     return 1;
      |   } catch (Exception e) {
      |     return 2;
      |   } finally {
      |     Object i;
      |   }
      | }
      |}""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

  test("TRY with default return not unreachable") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   try {
      |     return 0;
      |   } catch (QueryException e) {
      |     return 1;
      |   }
      |   return 2;
      | }
      |}""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

  // loop

  test("Loops with returns don't give false positive") {
    typeDeclaration("""
      |public class Dummy {
      | Object fn() {
      |   while (true) {
      |     return 0;
      |   }
      | }
      |}""".stripMargin)
    assert(dummyIssues.isEmpty)
  }

}
