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

class MethodShadowTest extends AnyFunSuite with TestHelper {

  def testMethods(classes: Map[String, String], error: String): Unit = {
    typeDeclarations(classes)
    assert(dummyIssues == error)
  }

  test("Override of public non-virtual") {
    testMethods(
      Map(
        "Dummy.cls"      -> "public class Dummy extends SuperClass { public void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { public void func() {}}"
      ),
      "Error: line 1 at 52-56: Method 'func' can not override non-virtual/non-abstract method\n"
    )
  }

  test("Override of public virtual without override") {
    testMethods(
      Map(
        "Dummy.cls"      -> "public class Dummy extends SuperClass { public void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { public virtual void func() {}}"
      ),
      "Error: line 1 at 52-56: Method 'func' must use the 'override' keyword\n"
    )
  }

  test("Override of missing method") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func2() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { private virtual void func() {}}"
      ),
      "Error: line 1 at 61-66: Method 'func2' does not override a virtual or abstract method\n"
    )
  }

  test("Override of private virtual") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { private virtual void func() {}}"
      ),
      "Error: line 1 at 61-65: Method 'func' can not override a private method\n"
    )
  }

  test("Override of private virtual (same file bug)") {
    testMethods(
      Map(
        "Dummy.cls" ->
          """public virtual class Dummy {
              | private virtual void func() {}
              | public class Other extends Dummy {public override void func() {} }
              |}
              |""".stripMargin
      ),
      "Warning: line 2 at 22-26: Private method overrides have inconsistent behaviour, use global, public or protected\n"
    )
  }

  test("Override of protected virtual") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { protected virtual void func() {}}"
      ),
      ""
    )
  }

  test("Override of public virtual") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { public virtual void func() {}}"
      ),
      ""
    )
  }

  test("Duplicate Override of public virtual") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} public override void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { public virtual void func() {}}"
      ),
      "Error: line 1 at 92-96: Method 'func' is a duplicate of an existing method at line 1 at 61-65\n"
    )
  }

  test("Override of public virtual (with protected)") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { protected override void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass { public virtual void func() {}}"
      ),
      "Error: line 1 at 64-68: Method 'func' can not reduce visibility in override\n"
    )
  }

  test("Override of private abstract") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public abstract class SuperClass { private abstract void func();}"
      ),
      "Error: line 1 at 61-65: Method 'func' can not override a private method\n"
    )
  }

  test("Override of protected abstract") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public abstract class SuperClass { protected abstract void func();}"
      ),
      ""
    )
  }

  test("Override of protected abstract (with private)") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { private override void func() {} }",
        "SuperClass.cls" -> "public abstract class SuperClass { protected abstract void func();}"
      ),
      "Error: line 1 at 62-66: Method 'func' can not reduce visibility in override\n"
    )
  }

  test("Override of public abstract") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public abstract class SuperClass { public abstract void func();}"
      ),
      ""
    )
  }

  test("Override of private virtual (test visible)") {
    testMethods(
      Map(
        "Dummy.cls" -> "@IsTest public class Dummy extends SuperClass { public override void func() {} }",
        "SuperClass.cls" -> "public virtual class SuperClass {@TestVisible private virtual void func() {}}"
      ),
      ""
    )
  }

  test("Duplicate static methods") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy { public static void func() {} private static void fuNc() {}}"
      ),
      "Error: line 1 at 70-74: Method 'fuNc' is a duplicate of an existing method\n"
    )
  }

  test("Duplicate static methods (with args)") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy { public static void func(String a) {} private static void fuNc(System.String b) {}}"
      ),
      "Error: line 1 at 78-82: Method 'fuNc' is a duplicate of an existing method\n"
    )
  }

  test("Extending abstract requires interface methods") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void m1(); void m2(); }",
        "AFoo.cls"  -> "public abstract class AFoo implements Foo { public void m1(){} }",
        "Dummy.cls" -> "public class Dummy extends AFoo {  }"
      ),
      "Missing: line 1 at 13-18: Non-abstract class must implement method 'void m2()' from interface 'Foo'\n"
    )
  }

  test("Implementing method has generic SObject as param instead of specific SObject") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(Account ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(SObject ac){} }"
      ),
      ""
    )
  }

  test("Implementing method has specific SObject as param instead of generic SObject") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(SObject ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(Account ac){} }"
      ),
      "Missing: line 1 at 13-18: Non-abstract class must implement method 'void fn(System.SObject)' from interface 'Foo'\n"
    )
  }

  test("Implementing method has generic SObject") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(SObject ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(SObject ac){} }"
      ),
      ""
    )
  }

  test("Implementing method has specific SObject") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(Account ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(Account ac){} }"
      ),
      ""
    )
  }

  test("Implementing method has different specific SObject as param instead of specific SObject") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(Account ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(Opportunity ac){} }"
      ),
      "Missing: line 1 at 13-18: Non-abstract class must implement method 'void fn(Schema.Account)' from interface 'Foo'\n"
    )
  }

  test("Implementing method has Id as param instead of String") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(String ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(Id ac){} }"
      ),
      ""
    )
  }

  test("Implementing method has String as param instead of Id") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(Id ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(String ac){} }"
      ),
      ""
    )
  }

  test("Implementing method has Long as param instead of Integer") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(Integer ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(Long ac){} }"
      ),
      ""
    )
  }

  test("Implementing method has Integer as param instead of Long") {
    testMethods(
      Map(
        "Foo.cls"   -> "public interface Foo { void fn(Long ac); }",
        "Dummy.cls" -> "public class Dummy implements Foo { public void fn(Integer ac){} }"
      ),
      "Missing: line 1 at 13-18: Non-abstract class must implement method 'void fn(System.Long)' from interface 'Foo'\n"
    )
  }

  test("Instance method shadowing a static method") {
    testMethods(
      Map(
        "Dummy.cls" -> "public class Dummy { void getInstance() {}  static void getInstance(){}}"
      ),
      "Error: line 1 at 26-37: method 'getInstance' is a duplicate of an existing static method\n" +
        "Error: line 1 at 56-67: static method 'getInstance' is a duplicate of an existing instance method\n"
    )
  }
}
