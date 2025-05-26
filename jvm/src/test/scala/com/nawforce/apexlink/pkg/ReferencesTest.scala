/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.{TargetLocationString, TargetLocationStringWithLine, TestHelper}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class ReferencesTest extends AnyFunSuite with TestHelper {

  test("Returns empty on non referencable methods") {
    val a =
      withCursor(s"public class A { void fun(){ String s = 'small'.cap${CURSOR}italize();}}")
    FileSystemHelper.run(
      Map("A.cls" -> a._1, "UsedA.cls" -> "public class UsedA {public void fn(){new A().fun();}}")
    ) { root: PathLike =>
      val path = root.join("A.cls")
      val org  = createHappyOrg(root)
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = a._2)
          .isEmpty
      )
    }
  }

  test("Find all references from method signature") {
    val dummy = withCursor(
      s"public class Dummy implements IInterface{ public void f${CURSOR}unc(){} public void callFuncMethod(){func();}}"
    )
    FileSystemHelper.run(
      Map(
        "Dummy.cls"      -> dummy._1,
        "IInterface.cls" -> "public interface IInterface { void func(); }",
        "Bar.cls"        -> "public class Bar {public void method(){ new Dummy().func();}}"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2)
          .map(TargetLocationString(root, _))
          .toSet == Set(
          TargetLocationString(root.join("Dummy.cls").toString, "func()"),
          TargetLocationString(root.join("Bar.cls").toString, "new Dummy().func()")
        )
      )
    }
  }

  test("Find all references from an interface") {
    val dummy = withCursor(s"public interface Dummy { void fun${CURSOR}c(); }")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummy._1,
        "Foo.cls"   -> "public class Foo implements Dummy { public void func(){}}",
        "Bar.cls"   -> "public class Bar {public void method(){new Foo().func();}}"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2)
          .map(TargetLocationString(root, _))
          .toSet == Set(TargetLocationString(root.join("Bar.cls").toString, "new Foo().func()"))
      )
    }
  }

  test("Find all references from inner methods") {
    val dummy =
      withCursor(s"public class Dummy { public class InnerDummy { public void fun${CURSOR}c(){}}}")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummy._1,
        "Foo.cls"   -> "public class Foo { public void func(){new Dummy.InnerDummy().func();}}",
        "Bar.cls"   -> "public class Bar { public void method(){new Dummy.InnerDummy();}}"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2)
          .map(TargetLocationString(root, _))
          .toSet == Set(
          TargetLocationString(root.join("Foo.cls").toString, "new Dummy.InnerDummy().func()")
        )
      )
    }
  }

  test("Shadowed method references from child class") {
    val B = withCursor(
      s"public virtual class B implements Common {  public Common f${CURSOR}n(){return this;}}"
    )
    FileSystemHelper.run(
      Map(
        "Common.cls" -> "public interface Common { Common fn(); }",
        "A.cls"      -> "public class A implements Common { public Common fn(){return this;}}",
        "B.cls"      -> B._1,
        "C.cls"      -> "public virtual class C extends B { }",
        "UsedC.cls"  -> "public class UsedC { void fun(){ Common c = new C(); c.fn();}}",
        "UsedB.cls"  -> "public class UsedB { void fun(){ new B().fn();}}",
        "UsedA.cls"  -> "public class UsedA { void fun(){ new A();} }"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("B.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = B._2)
          .map(TargetLocationString(root, _))
          .toSet == Set(
          TargetLocationString(root.join("UsedC.cls").toString, "c.fn()"),
          TargetLocationString(root.join("UsedB.cls").toString, "new B().fn()")
        )
      )
    }
  }

  test("Shadowed method references from interface") {
    val common = withCursor(s"public interface Common { Common f${CURSOR}n(); }")
    FileSystemHelper.run(
      Map(
        "Common.cls" -> common._1,
        "A.cls"      -> "public class A implements Common { public Common fn(){return this;}}",
        "B.cls" -> "public virtual class B implements Common {  public Common fn(){return this;}}",
        "C.cls" -> "public virtual class C extends B { }",
        "UsedB.cls" -> "public class UsedB { void fun(){ new B().fn();}}",
        "UsedC.cls" -> "public class UsedC { void fun(){ Common c = new C(); c.fn();}}"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Common.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = common._2)
          .map(TargetLocationString(root, _))
          .toSet == Set(
          TargetLocationString(root.join("UsedB.cls").toString, "new B().fn()"),
          TargetLocationString(root.join("UsedC.cls").toString, "c.fn()")
        )
      )
    }
  }

  def testReferences(
    files: Map[String, String],
    cursor: (String, CursorPos),
    expected: Array[TargetLocationStringWithLine]
  ): Assertion = {
    withManualFlush {
      FileSystemHelper.run(files) { root: PathLike =>
        val org = createHappyOrg(root)
        val results = org.unmanaged
          .getReferences(root.join(cursor._1), line = cursor._2.line, offset = cursor._2.offset)
          .map(TargetLocationStringWithLine(root, _))
          .toSet
        assert(results.size == expected.length)
        assert(results == expected.toSet)

        org.flush()
        val org2 = createOrg(root)
        val results2 = org2.unmanaged
          .getReferences(root.join(cursor._1), line = cursor._2.line, offset = cursor._2.offset)
          .map(TargetLocationStringWithLine(root, _))
          .toSet
        assert(results2.size == expected.length)
        assert(results2 == expected.toSet)
      }
    }
  }

  test("Find references from field declaration") {
    val cursor = withCursorMultiLine(s"""
         | public class Foo {
         |   public String m${CURSOR}yField;
         |   void func1(){List<Account> a=[Select Id from Account where Id = :myField];}
         |   void func2(){myField = 'test';}
         | }
         |""".stripMargin)

    testReferences(
      Map(
        "Foo.cls" -> cursor._1,
        "Bar.cls" -> "public class Bar { {Foo f; String a = f.myField;}}"
      ),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "myField", 4),
        TargetLocationStringWithLine("/Foo.cls", "myField", 5),
        TargetLocationStringWithLine("/Bar.cls", "f.myField", 1)
      )
    )
  }

  test("Find references from property declaration") {
    val cursor = withCursorMultiLine(s"""
                                     | public class Foo {
                                     |   public String m${CURSOR}yField {get; set;}
                                     |   void func1(){List<Account> a=[Select Id from Account where Id = :myField];}
                                     |   void func2(){myField = 'test';}
                                     | }
                                     |""".stripMargin)

    testReferences(
      Map(
        "Foo.cls" -> cursor._1,
        "Bar.cls" -> "public class Bar { {Foo f; String a = f.myField;}}"
      ),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "myField", 4),
        TargetLocationStringWithLine("/Foo.cls", "myField", 5),
        TargetLocationStringWithLine("/Bar.cls", "f.myField", 1)
      )
    )
  }

  test("Find references from enum constant declaration") {
    val cursor = withCursorMultiLine(s"""
                                     | public enum Foo {
                                     |   ${CURSOR}Constant1
                                     | }
                                     |""".stripMargin)

    testReferences(
      Map(
        "Foo.cls" -> cursor._1,
        "Bar.cls" -> "public class Bar { {Foo f = Foo.Constant1;}}",
        "Baz.cls" -> "public class Baz { {Foo f; switch on f {when Constant1 {} }}}"
      ),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Bar.cls", "Foo.Constant1", 1),
        TargetLocationStringWithLine("/Baz.cls", "Constant1", 1)
      )
    )
  }

  test("Find type references from type cast") {
    val cursor = withCursorMultiLine(s"""
                                     | public class ${CURSOR}Foo {
                                     |   { Object f = (Foo) null;}
                                     | }
                                     |""".stripMargin)

    testReferences(
      Map(
        "Foo.cls" -> cursor._1,
        "Bar.cls" -> "public class Bar { {Object f = (Map<String, Foo>) null; } }"
      ),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "(Foo) null", 3),
        TargetLocationStringWithLine("/Bar.cls", "(Map<String, Foo>) null", 1)
      )
    )
  }

  test("Find type references from instanceOf") {
    val cursor = withCursorMultiLine(s"""
                                     | public class F${CURSOR}oo {
                                     |   { Object a; Boolean b = a instanceOf List<Foo>;}
                                     | }
                                     |""".stripMargin)

    testReferences(
      Map(
        "Foo.cls" -> cursor._1,
        "Bar.cls" -> "public class Bar { {Object a; Boolean b = a instanceOf Set<Foo>;} }"
      ),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "a instanceOf List<Foo>", 3),
        TargetLocationStringWithLine("/Bar.cls", "a instanceOf Set<Foo>", 1)
      )
    )
  }

  test("Find type references from .class") {
    val cursor = withCursorMultiLine(s"""
                                     | public class Fo${CURSOR}o {
                                     |   {Type t  = Foo.class;}
                                     | }
                                     |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Type t  = Foo.class;} }"),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "Foo.class", 3),
        TargetLocationStringWithLine("/Bar.cls", "Foo.class", 1)
      )
    )
  }

  test("Find type references from local var") {
    val cursor = withCursorMultiLine(s"""
                                        | public class Fo${CURSOR}o {
                                        |   {Foo f;}
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Foo f = null;} }"),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "f", 3),
        TargetLocationStringWithLine("/Bar.cls", "f = null", 1)
      )
    )
  }

  test("Find type references from formal param") {
    val cursor = withCursorMultiLine(s"""
                                        | public class Foo$CURSOR {
                                        |   Foo(Foo f) {}
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { static void func(Foo f) {} }"),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "f", 3),
        TargetLocationStringWithLine("/Bar.cls", "f", 1)
      )
    )
  }

  test("Find type references from extends") {
    val cursor = withCursorMultiLine(s"""
                                        | public virtual class Foo$CURSOR {
                                        |   public class Bar extends Foo {}
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar extends Foo { }"),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "Bar", 3),
        TargetLocationStringWithLine("/Bar.cls", "Bar", 1)
      )
    )
  }

  test("Find type references from implements") {
    val cursor = withCursorMultiLine(s"""
                                        | public class FooOuter {
                                        |   public interface ${CURSOR}Foo {}
                                        |   public class Bar implements Foo {}
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("FooOuter.cls" -> cursor._1, "Bar.cls" -> "public class Bar implements FooOuter.Foo { }"),
      ("FooOuter.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/FooOuter.cls", "Bar", 4),
        TargetLocationStringWithLine("/Bar.cls", "Bar", 1)
      )
    )
  }

  test("Find type references from new") {
    val cursor = withCursorMultiLine(s"""
                                        | public class ${CURSOR}Foo {
                                        |   Foo() {}
                                        |   public Foo(String a) {}
                                        |   {Object f = new Foo();}
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Object f = new Foo('a');} }"),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "Foo", 5),
        TargetLocationStringWithLine("/Bar.cls", "Foo", 1)
      )
    )
  }

  test("Find type references from instance field reference") {
    val cursor = withCursorMultiLine(s"""
                                        | public class ${CURSOR}Foo {
                                        |   public String a;
                                        |   {a = null;}
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Foo f; f.a = null;} }"),
      ("Foo.cls", cursor._2),
      Array(TargetLocationStringWithLine("/Bar.cls", "f", 1))
    )
  }

  test("Find type references from static field reference") {
    val cursor = withCursorMultiLine(s"""
                                        | public class ${CURSOR}Foo {
                                        |   public static String a;
                                        |   {Foo.a = null;}
                                        |   static { a = null; }
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Foo.a = null;} }"),
      ("Foo.cls", cursor._2),
      Array(TargetLocationStringWithLine("/Bar.cls", "Foo.a", 1))
    )
  }

  test("Find type references from constructor reference") {
    val cursor = withCursorMultiLine(s"""
         | public virtual class ${CURSOR}Foo {
         |   public Foo(String a) {};
         |   {Object a = new Foo('test');}
         |   public class Baz extends Foo {
         |    Baz(String a) {super(a);}
         |   }
         | }
         |""".stripMargin)

    testReferences(
      Map(
        "Foo.cls" -> cursor._1,
        "Bar.cls" -> "public class Bar { {Object a = new Foo('test');} }"
      ),
      ("Foo.cls", cursor._2),
      Array(
        TargetLocationStringWithLine("/Foo.cls", "Foo", 4),
        TargetLocationStringWithLine("/Foo.cls", "Baz", 5),
        TargetLocationStringWithLine("/Foo.cls", "super(a)", 6),
        TargetLocationStringWithLine("/Bar.cls", "Foo", 1)
      )
    )
  }

  test("Find type references from instance method reference") {
    val cursor = withCursorMultiLine(s"""
                                          | public class ${CURSOR}Foo {
                                          |   public void a() {};
                                          |   {a();}
                                          | }
                                          |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Foo f; f.a();} }"),
      ("Foo.cls", cursor._2),
      Array(TargetLocationStringWithLine("/Bar.cls", "f", 1))
    )
  }

  test("Find type references from static method reference") {
    val cursor = withCursorMultiLine(s"""
                                        | public class ${CURSOR}Foo {
                                        |   public static void a() {};
                                        |   {Foo.a();}
                                        |   static { a(); }
                                        | }
                                        |""".stripMargin)

    testReferences(
      Map("Foo.cls" -> cursor._1, "Bar.cls" -> "public class Bar { {Foo.a();} }"),
      ("Foo.cls", cursor._2),
      Array(TargetLocationStringWithLine("/Bar.cls", "Foo.a()", 1))
    )
  }

}
