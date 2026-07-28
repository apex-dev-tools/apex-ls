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
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
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

  test("Static implementation of interface method") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy implements A {public static void func() {}}",
        "A.cls"     -> "public interface A {void func();}"
      )
    )
    assert(
      dummyIssues ==
        "Warning: line 1 at 52-56: Implementing 'void func()' from interface 'A' with a static method can be confusing, change to an instance method\n"
    )
  }

  test("Unrelated static method on interface implementation") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy implements A {public void func() {} public static void helper() {}}",
        "A.cls" -> "public interface A {void func();}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Static implementation of Schedulable method") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements Schedulable {public static void execute(SchedulableContext context) {}}"
      )
    )
    assert(
      dummyIssues ==
        "Warning: line 1 at 62-69: Implementing 'void execute(System.SchedulableContext)' from interface 'System.Schedulable' with a static method can be confusing, change to an instance method\n"
    )
  }

  test("Instance implementation of Schedulable method") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements Schedulable {public void execute(SchedulableContext context) {}}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Static implementation of Queueable method") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements Queueable {public static void execute(QueueableContext context) {}}"
      )
    )
    assert(
      dummyIssues ==
        "Warning: line 1 at 60-67: Implementing 'void execute(System.QueueableContext)' from interface 'System.Queueable' with a static method can be confusing, change to an instance method\n"
    )
  }

  test("Instance implementation of Queueable method") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements Queueable {public void execute(QueueableContext context) {}}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Missing implementation of Schedulable method") {
    typeDeclarations(Map("Dummy.cls" -> "public class Dummy implements Schedulable {}"))
    assert(
      dummyIssues ==
        "Missing: line 1 at 13-18: Non-abstract class must implement method 'void execute(System.SchedulableContext)' from interface 'System.Schedulable'\n"
    )
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

  test("Class implements Database.Batchable<sObject> with Set start") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          """
          | global class Dummy implements Database.Batchable<sObject> {
          |   public Set<sObject> start(Database.BatchableContext param1) { return new Set<SObject>(); }
          |   public void execute(Database.BatchableContext param1, List<SObject> param2) {}
          |   public void finish(Database.BatchableContext param1) {}
          | }
          |""".stripMargin
      )
    )
    assert(dummyIssues == "")
  }

  test("Class implements Database.Batchable<Account> with Set start") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          """
          | global class Dummy implements Database.Batchable<Account> {
          |   public Set<Account> start(Database.BatchableContext param1) { return new Set<Account>(); }
          |   public void execute(Database.BatchableContext param1, List<Account> param2) {}
          |   public void finish(Database.BatchableContext param1) {}
          | }
          |""".stripMargin
      )
    )
    assert(dummyIssues == "")
  }

  test("Class implements Database.Batchable<Account> does not allow Set execute") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          """
          | global class Dummy implements Database.Batchable<Account> {
          |   public Set<Account> start(Database.BatchableContext param1) { return new Set<Account>(); }
          |   public void execute(Database.BatchableContext param1, Set<Account> param2) {}
          |   public void finish(Database.BatchableContext param1) {}
          | }
          |""".stripMargin
      )
    )
    assert(
      dummyIssues ==
        "Missing: line 2 at 14-19: Non-abstract class must implement method 'void execute(Database.BatchableContext, System.List<Schema.Account>)' from interface 'Database.Batchable<Schema.Account>'\n"
    )
  }

  test("Interface method overload validation - GitHub issue #329") {
    typeDeclarations(
      Map(
        "TestInterface.cls" -> "public interface TestInterface { Boolean isEnabled(String param); }",
        "Implementation.cls" ->
          """
          |public class Implementation implements TestInterface {
          |  public Boolean isEnabled(String param) { return true; }
          |  private Boolean isEnabled(List<String> params) { return false; }
          |  private Boolean isEnabled(Integer count) { return false; }
          |}
          |""".stripMargin
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test(
    "Interface method with ghosted parameter type implemented with a derived type - GitHub issue #327"
  ) {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |"packageDirectories": [{"path": "force-app"}],
          |"plugins": {"dependencies": [{"namespace": "ext"}]}
          |}""".stripMargin,
        "force-app/DerivedType.cls" -> "public class DerivedType extends ext.Something {}",
        "force-app/IFoo.cls"        -> "public interface IFoo { void foo(ext.Something a); }",
        "force-app/Impl.cls" ->
          """public class Impl implements IFoo {
            |  public void foo(DerivedType b) {}
            |}""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(getMessages(root.join("force-app").join("Impl.cls")).isEmpty)
    }
  }

}
