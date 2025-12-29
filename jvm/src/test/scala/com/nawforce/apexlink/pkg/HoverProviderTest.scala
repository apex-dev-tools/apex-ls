/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.TestHelper
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class HoverProviderTest extends AnyFunSuite with TestHelper {
  test("Hover for method") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Hover.cls")
      val content =
        """public class Dummy { public void someMethod() {methodB(1, 2);}
          |public String methodB(Integer a, Integer b){}
          |}""".stripMargin.replaceAll("\r\n", "\n")
      val offset    = content.split('\n').head.length - 10
      val hoverItem = org.unmanaged.getHover(path, line = 1, offset, Some(content))
      assert(
        hoverItem.content.get == "public System.String methodB(System.Integer a, System.Integer b)"
      )
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 47)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 54)
    }
  }

  test("Hover for inner class") {
    val contentAndCursorPos =
      withCursor(
        s"public virtual class Foo {public void after(){new Du${CURSOR}mmy();}  " +
          s"private class Dummy {public Dummy(){} } }"
      )
    FileSystemHelper.run(Map("Foo.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(hoverItem.content.get == "private class Dummy")
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for external class") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){Du${CURSOR}mmy.dummyMethod();} }")
    val dummy    = "public class Dummy implements DummyTwo {public static void dummyMethod(){} }"
    val dummyTwo = "public interface DummyTwo {}"
    FileSystemHelper.run(
      Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy, "DummyTwo.cls" -> dummyTwo)
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(hoverItem.content.get == "public class Dummy implements DummyTwo")
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for class implementing multiple interfaces") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){Du${CURSOR}mmy.dummyMethod();} }")
    val dummy =
      "public class Dummy implements DummyTwo, DummyThree, DummyFour {public static void dummyMethod(){} }"
    val dummyTwo   = "public interface DummyTwo {}"
    val dummyThree = "public interface DummyThree {}"
    val dummyFour  = "public interface DummyFour {}"
    FileSystemHelper.run(
      Map(
        "Foo.cls"        -> contentAndCursorPos._1,
        "Dummy.cls"      -> dummy,
        "DummyTwo.cls"   -> dummyTwo,
        "DummyThree.cls" -> dummyThree,
        "DummyFour.cls"  -> dummyFour
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(
        hoverItem.content.get == "public class Dummy implements DummyTwo, DummyThree, DummyFour"
      )
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for class implementing interfaces and extending classes") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){Du${CURSOR}mmy.dummyMethod();} }")
    val dummy =
      "public class Dummy extends DummyTwo implements DummyThree, DummyFour {public static void dummyMethod(){} }"
    val dummyTwo   = "public abstract class DummyTwo {}"
    val dummyThree = "public interface DummyThree {}"
    val dummyFour  = "public interface DummyFour {}"
    FileSystemHelper.run(
      Map(
        "Foo.cls"        -> contentAndCursorPos._1,
        "Dummy.cls"      -> dummy,
        "DummyTwo.cls"   -> dummyTwo,
        "DummyThree.cls" -> dummyThree,
        "DummyFour.cls"  -> dummyFour
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(
        hoverItem.content.get == "public class Dummy extends DummyTwo implements DummyThree, DummyFour"
      )
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for constructor") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){new Du${CURSOR}mmy(1);} }")
    val dummy = "public class Dummy{public Dummy(Integer a){} }"
    FileSystemHelper.run(Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy)) {
      root: PathLike =>
        val org = createHappyOrg(root)
        val hoverItem =
          org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
        assert(hoverItem.content.get == "public constructor(Integer a)")
        assert(hoverItem.location.get.startLine == 1)
        assert(hoverItem.location.get.startPosition == 46)
        assert(hoverItem.location.get.endLine == 1)
        assert(hoverItem.location.get.endPosition == 58)
    }
  }

  test("Hover for unsupported keyword") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public vo${CURSOR}id after(){Dummy.dummyMethod();} }")
    val dummy    = "public class Dummy implements DummyTwo {public static void dummyMethod(){} }"
    val dummyTwo = "public interface DummyTwo {}"
    FileSystemHelper.run(
      Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy, "DummyTwo.cls" -> dummyTwo)
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(hoverItem.content.isEmpty)
      assert(hoverItem.location.isEmpty)
    }
  }
}
