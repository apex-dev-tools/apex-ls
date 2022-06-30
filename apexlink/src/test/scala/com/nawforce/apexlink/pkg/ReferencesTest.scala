package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.{FileSystemHelper, TargetLocationString, TestHelper}
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class ReferencesTest extends AnyFunSuite with TestHelper {

  test("Find all references from method signature") {
    val dummy = withCursor(
      s"public class Dummy { public void f${CURSOR}unc() {} public void callFuncMethod(){func();}}"
    )
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummy._1,
        "Bar.cls"   -> "public class Bar {public void method(){ new Dummy().func();}}"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2, None)
          .map(TargetLocationString(root, _))
          sameElements
            Array(
              TargetLocationString(root.join("Dummy.cls").toString, "callFuncMethod"),
              TargetLocationString(root.join("Bar.cls").toString, "method")
            )
      )
    }
  }

  test("Find all references from method call") {
    val dummy = withCursor(
      s"public class Dummy { public void method(){Foo.f${CURSOR}unc();}}"
    )
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummy._1,
        "Foo.cls" -> "public class Foo { public static void func(){} }",
        "Bar.cls" -> "public class Bar {public void method(){Foo.func();}}"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2, None)
          .map(TargetLocationString(root, _))
          sameElements
          Array(
            TargetLocationString(root.join("Dummy.cls").toString, "method"),
            TargetLocationString(root.join("Bar.cls").toString, "method")
          )
      )
    }
  }

  test("Find all references from an interface") {
    val dummy = withCursor(
      s"public interface Dummy {  void fun${CURSOR}c(); }"
    )
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummy._1,
        "Foo.cls" -> "public class Foo implements Dummy { public void func(){} }",
        "Bar.cls" -> "public class Bar {public void method(){new Foo().func();}}"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2, None)
          .isEmpty
      )
    }
  }
}
