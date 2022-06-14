/*
 Copyright (c) 2017 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.rpc.LocationLink
import com.nawforce.apexlink.{FileSystemHelper, LocationLinkString, TestHelper}
import com.nawforce.pkgforce.path.{Location, PathLike}
import org.scalatest.funsuite.AnyFunSuite

class DefinitionAndImplProviderTest extends AnyFunSuite with TestHelper {

  test("Outer class match") {
    val contentAndCursorPos = withCursor(s"public class D${CURSOR}ummy {}")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(LocationLinkString("Dummy", path.toString, contentAndCursorPos._1, "Dummy"))
      )
    }
  }

  test("Outer class match (direct content)") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org                 = createHappyOrg(root)
      val path                = root.join("Dummy.cls")
      val contentAndCursorPos = withCursor(s"public class D${CURSOR}ummy {}")
      assert(
        org.unmanaged
          .getDefinition(
            path,
            line = 1,
            offset = contentAndCursorPos._2,
            Some(contentAndCursorPos._1)
          )
          .map(LocationLinkString(root, contentAndCursorPos._1, contentAndCursorPos._1, _))
          .contains(LocationLinkString("Dummy", path.toString, contentAndCursorPos._1, "Dummy"))
      )
    }
  }

  test("Outer class match (start)") {
    val contentAndCursorPos = withCursor(s"public class D${CURSOR}ummy {}")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(LocationLinkString("Dummy", path.toString, contentAndCursorPos._1, "Dummy"))
      )
    }
  }

  test("Outer class match (end)") {
    val contentAndCursorPos = withCursor(s"public class Dumm${CURSOR}y {}")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(LocationLinkString("Dummy", path.toString, contentAndCursorPos._1, "Dummy"))
      )
    }
  }

  test("Outer class match (external)") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy {/* F${CURSOR}oo */}")
    FileSystemHelper.run(
      Map("Dummy.cls" -> contentAndCursorPos._1, "Foo.cls" -> "public class Foo {}")
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString("Foo", root.join("Foo.cls").toString, "public class Foo {}", "Foo")
          )
      )
    }
  }

  test("Inner class match") {
    val contentAndCursorPos =
      withCursor(s"public class Match {/* Dummy.I${CURSOR}nner */}")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {class Inner{} }",
        "Match.cls" -> contentAndCursorPos._1
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Match.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString(
              "Dummy.Inner",
              root.join("Dummy.cls").toString,
              "class Inner{}",
              "Inner"
            )
          )
      )
    }
  }

  test("Inner class match (relative external)") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy /* I${CURSOR}nner */ {class Inner{} }")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString("Inner", root.join("Dummy.cls").toString, "class Inner{}", "Inner")
          )
      )
    }
  }

  test("Inner class match (relative within)") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy {class Inner{/* I${CURSOR}nner */} }")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString(
              "Inner",
              root.join("Dummy.cls").toString,
              "class Inner{/* Inner */}",
              "Inner"
            )
          )
      )
    }
  }

  test("Inner class match (relative inner)") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy {class Inner{/* O${CURSOR}ther */} class Other {}}")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString("Other", root.join("Dummy.cls").toString, "class Other {}", "Other")
          )
      )
    }
  }

  test("Inner class match (external)") {
    val contentAndCursorPos = withCursor(s"public class Dummy {/* Foo.${CURSOR}Other */}")
    FileSystemHelper.run(
      Map("Dummy.cls" -> contentAndCursorPos._1, "Foo.cls" -> "public class Foo {class Other {}}")
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString(
              "Foo.Other",
              root.join("Foo.cls").toString,
              "class Other {}",
              "Other"
            )
          )
      )
    }
  }

  test("Outer class with syntax error") {
    val contentAndCursorPos = withCursor(s"public class D${CURSOR}ummy {")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      createOrg(root)
      val path = root.join("Dummy.cls")
      withOrg { org =>
        assert(
          org.unmanaged
            .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
            .map(LocationLinkString(root, contentAndCursorPos._1, _))
            .contains(LocationLinkString("Dummy", path.toString, contentAndCursorPos._1, "Dummy"))
        )
      }
    }
  }

  test("Inner class with syntax error") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy {/*I${CURSOR}nner*/ class Inner{ /* }}")
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      createOrg(root)
      withOrg(org => {
        assert(
          org.unmanaged
            .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
            .map(LocationLinkString(root, contentAndCursorPos._1, _))
            .contains(
              LocationLinkString(
                "Inner",
                root.join("Dummy.cls").toString,
                "class Inner{ /*",
                "Inner"
              )
            )
        )
      })
    }
  }

  test("Static method") {
    val contentAndCursorPos = withCursor(s"public class Dummy {{Foo.m${CURSOR}ethod();}}")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> contentAndCursorPos._1,
        "Foo.cls"   -> "public class Foo { static void method() {} }"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString(
              "method",
              root.join("Foo.cls").toString,
              "void method() {}",
              "method"
            )
          )
      )
    }
  }

  test("Static field") {
    val contentAndCursorPos = withCursor(s"public class Dummy {{String a = Foo.F${CURSOR}OO;}}")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> contentAndCursorPos._1,
        "Foo.cls"   -> "public class Foo { static String FOO = 'foo'; }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      withOrg { org =>
        assert(
          org.unmanaged
            .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
            .map(LocationLinkString(root, contentAndCursorPos._1, _))
            .contains(
              LocationLinkString(
                "Foo.FOO",
                root.join("Foo.cls").toString,
                "String FOO = 'foo';",
                "FOO"
              )
            )
        )
      }
    }
  }

  test("Overloaded static method") {
    val contentAndCursorPos = withCursor(s"public class Dummy { {Foo.m${CURSOR}ethod('');} }")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> contentAndCursorPos._1,
        "Foo.cls"   -> "public class Foo { static void method(Integer p) {} static void method(String p) {}}"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements Array(
            LocationLinkString(
              "method",
              root.join("Foo.cls").toString,
              "void method(String p) {}",
              "method"
            )
          )
      )
    }
  }

  test("Unrecognised static reference") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy {/* Foo.r${CURSOR}andom */}")
    FileSystemHelper.run(
      Map("Dummy.cls" -> contentAndCursorPos._1, "Foo.cls" -> "public class Foo { }")
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .isEmpty
      )
    }
  }

  test("Trigger External Class") {
    val contentAndCursorPos =
      withCursor(s"trigger Dummy on Account(before insert) {/* Fo${CURSOR}o */}")
    FileSystemHelper.run(
      Map("Dummy.trigger" -> contentAndCursorPos._1, "Foo.cls" -> "public class Foo {}")
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.trigger")
      assert(
        org.unmanaged
          .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(
            LocationLinkString("Foo", root.join("Foo.cls").toString, "public class Foo {}", "Foo")
          )
      )
    }
  }

  test("Trigger External Class Expression") {
    val contentAndCursorPos =
      withCursor(s"trigger Dummy on Account(before insert) {Foo f = new F${CURSOR}oo();}")
    FileSystemHelper.run(
      Map("Dummy.trigger" -> contentAndCursorPos._1, "Foo.cls" -> "public class Foo {}")
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.trigger")

      assert(
        org.unmanaged
          .getDefinition(path, line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          .contains(LocationLinkString("new Foo()", root.join("Foo.cls").toString, "Foo", "Foo"))
      )
    }
  }

  test("Trigger with content and static field") {
    val contentAndCursorPos =
      withCursor(s"trigger Dummy on Account(before insert) {Foo.met${CURSOR}hod(1)}")
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {static void method(Integer p) {}}")) {
      root: PathLike =>
        val org  = createHappyOrg(root)
        val path = root.join("Dummy.trigger")
        assert(
          org.unmanaged
            .getDefinition(
              path,
              line = 1,
              offset = contentAndCursorPos._2,
              Some(contentAndCursorPos._1)
            )
            .map(LocationLinkString(root, contentAndCursorPos._1, _))
            .contains(
              LocationLinkString(
                "method",
                root.join("Foo.cls").toString,
                "void method(Integer p) {}",
                "method"
              )
            )
        )
    }
  }

  test("Synthetic constructor navigation") {
    val contentAndCursorPos = withCursor(s"public class Dummy {{new F${CURSOR}oo();}}")
    FileSystemHelper.run(
      Map("Dummy.cls" -> contentAndCursorPos._1, "Foo.cls" -> "public class Foo {}")
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements
            Array(LocationLinkString("new Foo()", root.join("Foo.cls").toString, "Foo", "Foo"))
      )
    }
  }

  test("Defined constructor navigation") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy {{Foo f = new F${CURSOR}oo(1);}}")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> contentAndCursorPos._1,
        "Foo.cls"   -> "public class Foo { public Foo(Integer i){}}"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements
            Array(
              LocationLinkString(
                "new Foo(1)",
                root.join("Foo.cls").toString,
                "Foo(Integer i){}",
                "Foo"
              )
            )
      )
    }
  }

  test("Super Constructor") {
    val contentAndCursorPos =
      withCursor(s"public class Dummy extends Foo { public Dummy(){s${CURSOR}uper('s');} }")
    FileSystemHelper.run(
      Map(
        "Foo.cls"   -> "virtual public class Foo { public Foo(String s){}}",
        "Dummy.cls" -> contentAndCursorPos._1
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      println(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements
            Array(LocationLinkString("super('s')", "/Foo.cls", "Foo(String s){}", "Foo"))
      )
    }
  }

  test("This constructor") {
    val contentAndCursorPos =
      withCursor(
        s"public class Dummy { public Dummy(){th${CURSOR}is('s');} public Dummy(String s){} }"
      )
    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org = createHappyOrg(root)
      println(
        org.unmanaged
          .getDefinition(root.join("Dummy.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements
            Array(LocationLinkString("this('s')", "/Dummy.cls", "Dummy(String s){}", "Dummy"))
      )
    }
  }

  test("super call to inner class with synthetic ctor") {
    val contentAndCursorPos =
      withCursor(s"public class Foo extends Dummy.InnerClass {public Foo(){s${CURSOR}uper();}}")
    FileSystemHelper.run(
      Map(
        "Foo.cls"   -> contentAndCursorPos._1,
        "Dummy.cls" -> "public class Dummy { public virtual class InnerClass {}}"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getDefinition(root.join("Foo.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements
            Array(LocationLinkString("super()", "/Dummy.cls", "InnerClass", "InnerClass"))
      )
    }
  }

  test("Method implementations") {
    val contentAndCursorPos = withCursor(s"public interface Foo { void goTo${CURSOR}Method(); }")
    val source = Map(
      "Foo.cls"           -> contentAndCursorPos._1,
      "Dummy.cls"         -> "public class Dummy implements Foo {public void goToMethod(){}}",
      "FooController.cls" -> "public class FooController implements Foo{ public void goToMethod(){}}"
    )
    FileSystemHelper.run(source) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getImplementation(root.join("Foo.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _)) sameElements
          Array(
            LocationLinkString("goToMethod", "/Dummy.cls", "void goToMethod(){}", "goToMethod"),
            LocationLinkString(
              "goToMethod",
              "/FooController.cls",
              "void goToMethod(){}",
              "goToMethod"
            )
          )
      )
    }
  }

  test("Method Implementation in abstract classes") {
    val contentAndCursorPos =
      withCursor(s"public interface Foo { void goToMethod(); void con${CURSOR}crete(); }")
    val dummy    = "public abstract class Dummy implements Foo { public virtual void concrete(){}}"
    val dummyTwo = "public virtual class DummyTwo extends Dummy { public void goToMethod(){}}"
    val bar      = "public class Bar extends DummyTwo { public override void concrete(){} }"
    FileSystemHelper.run(
      Map(
        "Foo.cls"      -> contentAndCursorPos._1,
        "Dummy.cls"    -> dummy,
        "DummyTwo.cls" -> dummyTwo,
        "Bar.cls"      -> bar
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getImplementation(root.join("Foo.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _))
          sameElements Array(
            LocationLinkString("concrete", "/Bar.cls", "void concrete(){}", "concrete"),
            LocationLinkString("concrete", "/Dummy.cls", "void concrete(){}", "concrete")
          )
      )
    }
  }

  test("Identifier Implementation") {
    val contentAndCursorPos =
      withCursor(s"public interface F${CURSOR}oo { void goToMethod(); void concrete(); }")
    val dummy    = "public abstract class Dummy implements Foo { public void concrete(){}}"
    val dummyTwo = "public virtual class DummyTwo extends Dummy { public void goToMethod(){}}"
    val bar      = "public class Bar extends DummyTwo { }"
    FileSystemHelper.run(
      Map(
        "Foo.cls"      -> contentAndCursorPos._1,
        "Dummy.cls"    -> dummy,
        "DummyTwo.cls" -> dummyTwo,
        "Bar.cls"      -> bar
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getImplementation(root.join("Foo.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _)) sameElements
          Array(
            LocationLinkString("Foo", "/Bar.cls", bar, "Bar"),
            LocationLinkString("Foo", "/DummyTwo.cls", dummyTwo, "DummyTwo"),
            LocationLinkString("Foo", "/Dummy.cls", dummy, "Dummy")
          )
      )
    }
  }

  test("Identifier Implementation with non super type dependents") {
    val contentAndCursorPos =
      withCursor(s"public interface F${CURSOR}oo { void goToMethod(); void concrete(); }")
    val dummy    = "public abstract class Dummy implements Foo { public void concrete(){}}"
    val dummyTwo = "public virtual class DummyTwo extends Dummy { public void goToMethod(){}}"
    val bar      = "public class Bar { public Bar(){ DummyTwo t = new DummyTwo();}}"
    FileSystemHelper.run(
      Map(
        "Foo.cls"      -> contentAndCursorPos._1,
        "Dummy.cls"    -> dummy,
        "DummyTwo.cls" -> dummyTwo,
        "Bar.cls"      -> bar
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getImplementation(root.join("Foo.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _)) sameElements
          Array(
            LocationLinkString("Foo", "/DummyTwo.cls", dummyTwo, "DummyTwo"),
            LocationLinkString("Foo", "/Dummy.cls", dummy, "Dummy")
          )
      )
    }
  }

  test("Identifier Implementation with inner classes") {
    val contentAndCursorPos =
      withCursor(s"public interface F${CURSOR}oo { void goToMethod();}")
    val dummy =
      "public abstract class Dummy { class InnerClass implements Foo { public void goToMethod(){}}}"
    FileSystemHelper.run(Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy)) {
      root: PathLike =>
        val org = createHappyOrg(root)
        assert(
          org.unmanaged
            .getImplementation(
              root.join("Foo.cls"),
              line = 1,
              offset = contentAndCursorPos._2,
              None
            )
            .map(LocationLinkString(root, contentAndCursorPos._1, _)) sameElements
            Array(
              LocationLinkString(
                "Foo",
                "/Dummy.cls",
                "class InnerClass implements Foo { public void goToMethod(){}}",
                "InnerClass"
              )
            )
        )
    }
  }

  test("From Inner go to implements") {
    val contentAndCursorPos =
      withCursor(s"public class Foo { public interface B${CURSOR}ar{} }")
    val dummy =
      "public abstract class Dummy { class InnerClass implements Foo.Bar {}}"
    FileSystemHelper.run(Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy)) {
      root: PathLike =>
        val org = createHappyOrg(root)
        assert(
          org.unmanaged
            .getImplementation(
              root.join("Foo.cls"),
              line = 1,
              offset = contentAndCursorPos._2,
              None
            )
            .map(LocationLinkString(root, contentAndCursorPos._1, _))
            sameElements
              Array(
                LocationLinkString(
                  "Bar",
                  "/Dummy.cls",
                  "class InnerClass implements Foo.Bar {}",
                  "InnerClass"
                )
              )
        )
    }
  }
}
