/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.{LocationLinkString, TestHelper}
import com.nawforce.pkgforce.PathInterpolator.PathInterpolator
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class ImplementationProviderTest extends AnyFunSuite with TestHelper {

  test("Method implementations") {
    val contentAndCursorPos = withCursor(s"public interface Foo { void goTo${CURSOR}Method(); }")
    val source = Map(
      "Foo.cls"   -> contentAndCursorPos._1,
      "Dummy.cls" -> "public class Dummy implements Foo {public void goToMethod(){}}",
      "FooController.cls" -> "public class FooController implements Foo{ public void goToMethod(){}}"
    )
    FileSystemHelper.run(source) { root: PathLike =>
      val org = createHappyOrg(root)
      assert(
        org.unmanaged
          .getImplementation(root.join("Foo.cls"), line = 1, offset = contentAndCursorPos._2, None)
          .map(LocationLinkString(root, contentAndCursorPos._1, _)) sameElements
          Array(
            LocationLinkString("goToMethod", path"/Dummy.cls", "void goToMethod(){}", "goToMethod"),
            LocationLinkString(
              "goToMethod",
              path"/FooController.cls",
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
            LocationLinkString("concrete", path"/Dummy.cls", "void concrete(){}", "concrete"),
            LocationLinkString("concrete", path"/Bar.cls", "void concrete(){}", "concrete")
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
            LocationLinkString("Foo", path"/Bar.cls", bar, "Bar"),
            LocationLinkString("Foo", path"/DummyTwo.cls", dummyTwo, "DummyTwo"),
            LocationLinkString("Foo", path"/Dummy.cls", dummy, "Dummy")
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
            LocationLinkString("Foo", path"/DummyTwo.cls", dummyTwo, "DummyTwo"),
            LocationLinkString("Foo", path"/Dummy.cls", dummy, "Dummy")
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
                path"/Dummy.cls",
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
                  path"/Dummy.cls",
                  "class InnerClass implements Foo.Bar {}",
                  "InnerClass"
                )
              )
        )
    }
  }
}
