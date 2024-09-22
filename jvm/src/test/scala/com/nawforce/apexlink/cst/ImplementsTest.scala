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

class ImplementsTest extends AnyFunSuite with TestHelper {

  test("No implementation of interface method") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy implements A {}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(
      dummyIssues ==
        "Missing: line 1 at 13-18: Non-abstract class must implement method 'void func()' from interface 'A'\n"
    )
  }

  test("No implementation of interface method on abstract class") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public abstract class Dummy implements A {}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Global implementation of interface method") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "global class Dummy implements A {global void func() {}}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Public implementation of interface method") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy implements A {public void func() {}}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Protected implementation of interface method") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy implements A {protected void func() {}}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 48-52: Protected methods can only be used on virtual or abstract classes\n"
    )
  }

  test("Private implementation of interface method") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy implements A {void func() {}}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(
      dummyIssues ==
        "Missing: line 1 at 38-42: Method 'void func()' from interface 'A' must be public or global\n"
    )
  }

  test("Missing class interface") {
    assert(typeDeclarations(Map("Dummy.cls" -> "global class Dummy implements A {}")).nonEmpty)
    assert(
      dummyIssues ==
        "Missing: line 1 at 13-18: No type declaration found for 'A'\n"
    )
  }

  test("Missing class second interface") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "global class Dummy implements A, B {}",
        "A.cls"     -> "public interface A {}"
      )
    )
    assert(
      dummyIssues ==
        "Missing: line 1 at 13-18: No type declaration found for 'B'\n"
    )
  }

  test("Class implements class") {
    typeDeclarations(
      Map("Dummy.cls" -> "global class Dummy implements A {}", "A.cls" -> "public class A {}")
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 13-18: Type 'A' must be an interface\n"
    )
  }

  test("Class implements enum") {
    typeDeclarations(
      Map("Dummy.cls" -> "global class Dummy implements A {}", "A.cls" -> "public enum A {}")
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 13-18: Type 'A' must be an interface\n"
    )
  }

  test("Interface extends class") {
    typeDeclarations(
      Map("Dummy.cls" -> "global interface Dummy extends A {}", "A.cls" -> "public class A {}")
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 17-22: Type 'A' must be an interface\n"
    )
  }

  test("Interface extends enum") {
    typeDeclarations(
      Map("Dummy.cls" -> "global interface Dummy extends A {}", "A.cls" -> "public enum A {}")
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 17-22: Type 'A' must be an interface\n"
    )
  }

  test("Class implements Database.Batchable<sObject>") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          """
          | global class Dummy implements Database.Batchable<sObject> {
          |   public Iterable<sObject> start(Database.BatchableContext param1) { return new List<SObject>(); }
          |   public void execute(Database.BatchableContext param1, List<SObject> param2) {}
          |   public void finish(Database.BatchableContext param1) {}
          | }
          |""".stripMargin
      )
    )
    assert(dummyIssues == "")
  }
}
