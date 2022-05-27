package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ConstructorTest extends AnyFunSuite with TestHelper {

  test("Basic constructor") {
    typeDeclaration("public class Dummy {public dummY() {}}")
    assert(dummyIssues.isEmpty)
  }

  test("Constructor with args") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo {}",
        "Bar.cls"   -> "public class Bar { public Bar(String s){} }",
        "Dummy.cls" -> "public class Dummy {Dummy() {this(new Foo(), new Bar('s'));} Dummy(Foo f, Bar b){} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Bad name constructor") {
    typeDeclaration("public class Dummy {public Foo() {}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 27-30: Constructors should have same name as the class, maybe method return type is missing?\n"
    )
  }

  test("Duplicate no args constructor") {
    typeDeclaration("public class Dummy {public Dummy() {} private Dummy() {}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 46-51: Constructor is a duplicate of an earlier constructor at line 1 at 27-32\n"
    )
  }

  test("Duplicate same single args constructor") {
    typeDeclaration("public class Dummy {public Dummy(String a) {} private Dummy(String b) {}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 54-59: Constructor is a duplicate of an earlier constructor at line 1 at 27-32\n"
    )
  }

  test("Duplicate same multi args constructor") {
    typeDeclaration(
      "public class Dummy {public Dummy(String a, Integer b) {} private Dummy(String c, Integer d) {}}"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 65-70: Constructor is a duplicate of an earlier constructor at line 1 at 27-32\n"
    )
  }

  test("Duplicate same args (different typeRef) constructor") {
    typeDeclaration(
      "public class Dummy {public Dummy(String a) {} private Dummy(System.String b) {}}"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 54-59: Constructor is a duplicate of an earlier constructor at line 1 at 27-32\n"
    )
  }

  test("Multiple Duplicate no args constructor") {
    typeDeclaration("public class Dummy {public Dummy() {} private Dummy() {} private Dummy() {}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 46-51: Constructor is a duplicate of an earlier constructor at line 1 at 27-32\n" +
          "Error: line 1 at 65-70: Constructor is a duplicate of an earlier constructor at line 1 at 27-32\n"
    )
  }

  test("Call to invalid new constructor") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo { public Foo(Integer i) {}}",
        "Dummy.cls" -> "public class Dummy { public Dummy(String s){new Foo();} }"
      )
    )
    assert(dummyIssues == "Error: line 1 at 51-53: No constructor defined with 0 arguments\n")
  }

  test("Call to private constructor") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo { Foo(){} private Foo(Integer i) {}}",
        "Dummy.cls" -> "public class Dummy { public Dummy(String s){new Foo(1);} }"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 51-54: Constructor is not visible: Foo.<constructor>(System.Integer)\n"
    )
  }

  test("Call to private TestVisible constructors") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo { Foo(){} @TestVisible private Foo(Integer i) {}}",
        "Dummy.cls" -> "@isTest public class Dummy { public Dummy(String s){new Foo(1);} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Call to invalid super constructor") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "virtual class Foo {public Foo(Integer l) {}}",
        "Dummy.cls" -> "public class Dummy extends Foo{ public Dummy(String s){super('s');} }"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 55-65: Constructor not defined: Foo.<constructor>(System.String)\n"
    )
  }

  test("Call to invalid this constructor") {
    typeDeclarations(
      Map("Dummy.cls" -> "public class Dummy { public Dummy(Integer i){this('s');} }")
    )
    assert(
      dummyIssues == "Error: line 1 at 45-54: Constructor not defined: Dummy.<constructor>(System.String)\n"
    )
  }

  test("Duplicate platform generics") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy {Dummy(Database.Batchable<String> arg) {} Dummy(Database.Batchable<SObject> arg) {}}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 61-66: Constructor is a duplicate of an earlier constructor at line 1 at 20-25\n"
    )
  }

  test("Ambiguous private calls") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo {private Foo(Id i){} private Foo(String s){} private Foo(Object b){}}",
        "Dummy.cls" -> "public class Dummy { Dummy(){new Foo('abc'); }}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 36-43: Constructor is not visible: Foo.<constructor>(System.String)\n"
    )
  }

  test("Ambiguous private calls with loose assignable") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo {public Foo(Id i){} private Foo(String s){} private Foo(Object b){}}",
        "Dummy.cls" -> "public class Dummy { Dummy(){new Foo('abc'); }}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Custom exceptions") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo {public class TestException extends Exception {}}",
        "Dummy.cls" -> "public class Dummy { Dummy(){ throw new Foo.TestException('Error'); }}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Generic platform type constructors") {
    typeDeclarations(
      Map(
        "Dummy.cls" -> "public class Dummy { Dummy(){ new Set<String>(new List<String>()); new Map<String,String>();}}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("No default super constructor") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "virtual public class Foo {public Foo(String arg) {}}",
        "Dummy.cls" -> "public class Dummy extends Foo {}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 13-18: No default constructor available in super type: Foo\n"
    )
  }

  test("Private default super constructor") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "virtual public class Foo { private Foo() {} public Foo(String arg) {}}",
        "Dummy.cls" -> "public class Dummy extends Foo {}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 13-18: Constructor is not visible: Foo.<constructor>()\n"
    )
  }

  test("Protected constructor call") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo { protected Foo(String s) {}}",
        "Dummy.cls" -> "public class Dummy { public Dummy(String s){new Foo(s);} }"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 51-54: Constructor is not visible: Foo.<constructor>(System.String)\n"
    )
  }

  test("Protected super constructor call") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "virtual public class Foo { protected Foo(String s) {}}",
        "Dummy.cls" -> "public class Dummy extends Foo { public Dummy(String s){super(s);} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Protected constructor call from inner") {
    typeDeclarations(
      Map(
        "Foo.cls" -> "public class Foo { protected Foo(String s) {} class Bar{ public Bar(){ new Foo('s'); }}}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Private TestVisible default super constructor") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "@isTest virtual public class Foo { @TestVisible private Foo() {} public Foo(String arg) {}}",
        "Dummy.cls" -> "@isTest public class Dummy extends Foo {}"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Unconstructable platform type") {
    typeDeclarations(Map("Dummy.cls" -> "public class Dummy {Dummy(){new String();}}"))
    assert(dummyIssues == "Error: line 1 at 38-40: Type cannot be constructed: System.String\n")
  }

  test("Unconstructable type") {
    typeDeclarations(Map("Foo.cls" -> "public enum Foo {TEST}", "Dummy.cls" -> "public class Dummy {{new Foo();} }"))
    assert(dummyIssues == "Error: line 1 at 28-30: Type cannot be constructed: Foo\n")
  }

  test("Interview type") {
    typeDeclarations(Map("Dummy.cls" -> "public class Dummy {Dummy(){new Flow.Interview();}}"))
    assert(dummyIssues == "Error: line 1 at 46-48: Cannot create type for: Flow.Interview\n")
  }

  test("Ambiguous call") {
    typeDeclarations(
      Map(
        "Foo.cls"   -> "public class Foo {public Foo(String s){} public Foo(Integer i){} }",
        "Dummy.cls" -> "public class Dummy {Dummy(){new Foo(null);}}"
      )
    )
    assert(
      dummyIssues == "Error: line 1 at 35-41: Ambiguous constructor call: Foo.<constructor>(null)\n"
    )
  }
}
