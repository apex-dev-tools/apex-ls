package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.{FileSystemHelper, TargetLocationString, TestHelper}
import com.nawforce.pkgforce.path.PathLike
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
          sameElements
            Array(
              TargetLocationString(root.join("Dummy.cls").toString, "func()"),
              TargetLocationString(root.join("Bar.cls").toString, "new Dummy().func()")
            )
      )
    }
  }

  test("Find all references from method call") {
    val dummy = withCursor(s"public class Dummy { public void method(){Foo.f${CURSOR}unc();}}")
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummy._1,
        "Foo.cls"   -> "public class Foo { public static void func(){} }",
        "Bar.cls"   -> "public class Bar {public void method(){Foo.func();}}"
      )
    ) { root: PathLike =>
      val org  = createHappyOrg(root)
      val path = root.join("Dummy.cls")
      assert(
        org.unmanaged
          .getReferences(path, line = 1, offset = dummy._2)
          .map(TargetLocationString(root, _))
          sameElements
            Array(
              TargetLocationString(root.join("Dummy.cls").toString, "Foo.func()"),
              TargetLocationString(root.join("Bar.cls").toString, "Foo.func()")
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
          sameElements
            Array(TargetLocationString(root.join("Bar.cls").toString, "new Foo().func()"))
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
          sameElements
            Array(
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
          sameElements
            Array(
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
          sameElements
            Array(
              TargetLocationString(root.join("UsedB.cls").toString, "new B().fn()"),
              TargetLocationString(root.join("UsedC.cls").toString, "c.fn()")
            )
      )
    }
  }

  /* failing on pipe
  test("Indirect usage from cache") {
    val usedB =
      withCursor(
        s"public class UsedB { void fun(){ Common a = new B(); a.fn(); C c = new C();c.getB().f${CURSOR}n();}}"
      )
    FileSystemHelper.run(
      Map(
        "Common.cls" -> "public interface Common { Common fn();} ",
        "A.cls"      -> "public class A implements Common { public Common fn(){return this;}}",
        "B.cls" -> "public virtual class B implements Common {  public Common fn(){return this;}}",
        "C.cls" -> "public virtual class C{ B getB(){return new B();} }",
        "UsedB.cls" -> usedB._1,
        "UsedC.cls" -> "public class UsedC {{new B().fn();}}"
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      org.flush()
      // Reload from cache
      val org2 = createOrg(root)
      val path = root.join("UsedB.cls")
      assert(
        org2.unmanaged
          .getReferences(path, line = 1, offset = usedB._2)
          .map(TargetLocationString(root, _))
          .toSet ==
          Set(
            TargetLocationString(root.join("UsedB.cls").toString, "a.fn()"),
            TargetLocationString(root.join("UsedB.cls").toString, "c.getB().fn()"),
            TargetLocationString(root.join("UsedC.cls").toString, "new B().fn()")
          )
      )
    }
  }
  */

  test("Reference after change") {
    val usedB =
      withCursor(
        s"public class UsedB { void fun(){ Common a = new B(); a.fn(); C c = new C();c.getB().f${CURSOR}n();}}"
      )
    FileSystemHelper.run(
      Map(
        "Common.cls" -> "public interface Common { Common fn();} ",
        "A.cls"      -> "public class A implements Common { public Common fn(){return this;}}",
        "B.cls" -> "public virtual class B implements Common {  public Common fn(){return this;}}",
        "C.cls" -> "public virtual class C{ B getB(){return new B();} }",
        "UsedB.cls" -> usedB._1,
        "UsedC.cls" -> "public class UsedC {{new B().fn();}}"
      )
    ) { root: PathLike =>
      val path = root.join("UsedB.cls")

      val org = createHappyOrg(root)
      org.flush()
      // Reload from cache
      val org2 = createOrg(root)
      org2.flush()

      // make change
      root.createFile("UsedC.cls", "public class UsedC {}")
      val org3 = createOrg(root)
      org3.flush()
      // org with the change
      val org4 = createOrg(root)
      assert(
        org4.unmanaged
          .getReferences(path, line = 1, offset = usedB._2)
          .map(TargetLocationString(root, _))
          sameElements
            Array(
              TargetLocationString(root.join("UsedB.cls").toString, "a.fn()"),
              TargetLocationString(root.join("UsedB.cls").toString, "c.getB().fn()")
            )
      )
    }
  }
}
