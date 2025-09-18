/*
 * Copyright (c) 2024 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class BreakContinueTest extends AnyFunSuite with TestHelper {

  test("break statement outside loop") {
    typeDeclaration("public class Dummy { void fn() { break; } }")
    assert(dummyIssues == "Error: line 1 at 33-39: Break statement must be in loop\n")
  }

  test("continue statement outside loop") {
    typeDeclaration("public class Dummy { void fn() { continue; } }")
    assert(dummyIssues == "Error: line 1 at 33-42: Continue statement must be in loop\n")
  }

  test("break statement in if block outside loop") {
    typeDeclaration("public class Dummy { void fn() { if (true) { break; } } }")
    assert(dummyIssues == "Error: line 1 at 45-51: Break statement must be in loop\n")
  }

  test("continue statement in if block outside loop") {
    typeDeclaration("public class Dummy { void fn() { if (true) { continue; } } }")
    assert(dummyIssues == "Error: line 1 at 45-54: Continue statement must be in loop\n")
  }

  test("break statement in switch outside loop") {
    typeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    Integer i = 1;
      |    switch on i {
      |      when 0 { break; }
      |      when else {}
      |    }
      |  } 
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 6 at 15-21: Break statement must be in loop\n")
  }

  test("continue statement in switch outside loop") {
    typeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    Integer i = 1;
      |    switch on i {
      |      when 0 { continue; }
      |      when else {}
      |    }
      |  } 
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 6 at 15-24: Continue statement must be in loop\n")
  }

  test("break statement in try-catch outside loop") {
    typeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    try {
      |      break;
      |    } catch (Exception e) {}
      |  } 
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 5 at 6-12: Break statement must be in loop\n")
  }

  test("continue statement in try-catch outside loop") {
    typeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    try {
      |      continue;
      |    } catch (Exception e) {}
      |  } 
      |}""".stripMargin)
    assert(dummyIssues == "Error: line 5 at 6-15: Continue statement must be in loop\n")
  }

  test("break statement in while loop should be valid") {
    happyTypeDeclaration("public class Dummy { void fn() { while (true) { break; } } }")
  }

  test("continue statement in while loop should be valid") {
    happyTypeDeclaration("public class Dummy { void fn() { while (true) { continue; } } }")
  }

  test("break statement in while loop without block should be valid") {
    happyTypeDeclaration("public class Dummy { void fn() { while (true) break; } }")
  }

  test("continue statement in while loop without block should be valid") {
    happyTypeDeclaration("public class Dummy { void fn() { while (true) continue; } }")
  }

  test("break statement in for loop should be valid") {
    happyTypeDeclaration(
      "public class Dummy { void fn() { for (Integer i = 0; i < 10; i++) { break; } } }"
    )
  }

  test("continue statement in for loop should be valid") {
    happyTypeDeclaration(
      "public class Dummy { void fn() { for (Integer i = 0; i < 10; i++) { continue; } } }"
    )
  }

  test("break statement in for-each loop should be valid") {
    happyTypeDeclaration(
      "public class Dummy { void fn() { for (Integer i : new List<Integer>()) { break; } } }"
    )
  }

  test("continue statement in for-each loop should be valid") {
    happyTypeDeclaration(
      "public class Dummy { void fn() { for (Integer i : new List<Integer>()) { continue; } } }"
    )
  }

  test("break statement in do-while loop should be valid") {
    happyTypeDeclaration("public class Dummy { void fn() { do { break; } while (true); } }")
  }

  test("continue statement in do-while loop should be valid") {
    happyTypeDeclaration("public class Dummy { void fn() { do { continue; } while (true); } }")
  }

  test("break statement in nested loops should be valid") {
    happyTypeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    while (true) {
      |      for (Integer i = 0; i < 10; i++) {
      |        break;
      |      }
      |    }
      |  } 
      |}""".stripMargin)
  }

  test("continue statement in nested loops should be valid") {
    happyTypeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    while (true) {
      |      for (Integer i = 0; i < 10; i++) {
      |        continue;
      |      }
      |    }
      |  } 
      |}""".stripMargin)
  }

  test("break statement in if block inside loop should be valid") {
    happyTypeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    while (true) {
      |      if (true) {
      |        break;
      |      }
      |    }
      |  } 
      |}""".stripMargin)
  }

  test("continue statement in if block inside loop should be valid") {
    happyTypeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    while (true) {
      |      if (true) {
      |        continue;
      |      }
      |    }
      |  } 
      |}""".stripMargin)
  }

  test("multiple break and continue violations") {
    typeDeclaration("""
      |public class Dummy { 
      |  void fn() { 
      |    break;
      |    continue;
      |  } 
      |}""".stripMargin)
    assert(
      dummyIssues ==
        "Error: line 4 at 4-10: Break statement must be in loop\nError: line 5 at 4-13: Unreachable block or statement\nError: line 5 at 4-13: Continue statement must be in loop\n"
    )
  }
}
