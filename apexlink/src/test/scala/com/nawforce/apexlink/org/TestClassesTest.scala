/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.types.apex.SummaryDeclaration
import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class TestClassesTest extends AnyFunSuite with TestHelper {

  private def getTestClassNames(root: PathLike, paths: Array[String]): Set[String] = {
    withOrg(org => {
      org.getTestClassNamesInternal(paths.map(p => root.join(p))).map(_._1)
    })
  }

  private def assertIsSummaryDeclaration(
    pkg: OPM.PackageImpl,
    name: String,
    namespace: Option[Name] = None
  ): Unit = {
    assert(
      pkg.orderedModules.head
        .findModuleType(TypeName(Name(name)).withNamespace(namespace))
        .head
        .isInstanceOf[SummaryDeclaration]
    )
  }

  private def run(files: Map[String, String])(verify: (PathLike, Option[String]) => ()): Unit = {

    // Uncached, no namespace
    FileSystemHelper.run(files) { root: PathLike =>
      createHappyOrg(root)
      verify(root, None)
    }

    // Cached, no namespace
    withManualFlush {
      FileSystemHelper.run(files) { root: PathLike =>
        val org = createHappyOrg(root)
        org.flush()

        val org2 = createHappyOrg(root)
        val pkg2 = org2.unmanaged
        files.foreach(file => assertIsSummaryDeclaration(pkg2, file._1.replace(".cls", "")))

        verify(root, None)
      }
    }

    // Uncached, with namespace
    val withProject = Map(
      "sfdx-project.json" -> """{"namespace": "pkg", "packageDirectories": [{"path": "pkg"}]}"""
    ) ++ files.map(kv => ("pkg/" + kv._1, kv._2))
    FileSystemHelper.run(withProject) { root: PathLike =>
      createHappyOrg(root)
      verify(root.join("pkg"), Some("pkg"))
    }

    // Cached, with namespace
    withManualFlush {
      FileSystemHelper.run(withProject) { root: PathLike =>
        val org = createHappyOrg(root)
        org.flush()

        val org2 = createHappyOrg(root)
        val pkg2 = org2.packagesByNamespace(Some(Name("pkg")))
        files.foreach(
          file => assertIsSummaryDeclaration(pkg2, file._1.replace(".cls", ""), Some(Name("pkg")))
        )

        verify(root.join("pkg"), Some("pkg"))
      }
    }
  }

  private def withNamespace(ns: Option[String], value: String): String =
    ns.map(_ + '.' + value).getOrElse(value)

  test("Empty request") {
    run(Map()) { (root: PathLike, _: Option[String]) =>
      assert(getTestClassNames(root, Array()).isEmpty)
    }
  }

  test("No tests") {
    run(Map("Dummy.cls" -> "public class Dummy {}")) { (root: PathLike, _: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Single test class") {
    run(
      Map(
        "Dummy.cls"     -> "public class Dummy {}",
        "DummyTest.cls" -> "@isTest public class DummyTest { {Dummy a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "DummyTest")))
      assert(getTestClassNames(root, Array("DummyTest.cls")) == Set(withNamespace(ns, "DummyTest")))
    }
  }

  test("Single test class (inner)") {
    run(
      Map(
        "Dummy.cls"     -> "public class Dummy { public class DummyInner {} }",
        "DummyTest.cls" -> "@isTest public class DummyTest { {Dummy.DummyInner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "DummyTest")))
    }
  }

  test("Multiple test classes") {
    run(
      Map(
        "Dummy.cls"      -> "public class Dummy {}",
        "DummyTest.cls"  -> "@isTest public class DummyTest { {Dummy a;}}",
        "DummyTest2.cls" -> "@isTest public class DummyTest2 { {Dummy a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(
        getTestClassNames(root, Array("Dummy.cls")) == Set(
          withNamespace(ns, "DummyTest"),
          withNamespace(ns, "DummyTest2")
        )
      )
      assert(getTestClassNames(root, Array("DummyTest.cls")) == Set(withNamespace(ns, "DummyTest")))
      assert(
        getTestClassNames(root, Array("DummyTest2.cls")) == Set(withNamespace(ns, "DummyTest2"))
      )
    }
  }

  test("Multiple test classes (inner)") {
    run(
      Map(
        "Dummy.cls"      -> "public class Dummy { public class DummyInner {} }",
        "DummyTest.cls"  -> "@isTest public class DummyTest { {Dummy.DummyInner a;}}",
        "DummyTest2.cls" -> "@isTest public class DummyTest2 { {Dummy.DummyInner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(
        getTestClassNames(root, Array("Dummy.cls")) == Set(
          withNamespace(ns, "DummyTest"),
          withNamespace(ns, "DummyTest2")
        )
      )
      assert(getTestClassNames(root, Array("DummyTest.cls")) == Set(withNamespace(ns, "DummyTest")))
      assert(
        getTestClassNames(root, Array("DummyTest2.cls")) == Set(withNamespace(ns, "DummyTest2"))
      )
    }
  }

  test("Indirect test class") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy {}",
        "Foo.cls"     -> "public class Foo { {Dummy a;}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect test class (inner)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy { public class DummyInner {} }",
        "Foo.cls"     -> "public class Foo { {Dummy.DummyInner a;}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect test class (indirect inner)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy { }",
        "Foo.cls"     -> "public class Foo { public class FooInner { {Dummy a;} }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via super class to test class") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo {}",
        "Foo.cls"     -> "public virtual class Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via super class to test class (super class inner)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public virtual class FooInner { }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via inner super class to test class (super class not tested)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public virtual class FooInner { }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, _: Option[String]) =>
      // This is empty because FooTest does not reference Foo.FooInner, only Foo
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Indirect via super class to test class (super class not tested)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo {}",
        "Foo.cls"     -> "public virtual class Foo {public class FooInner {}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { (root: PathLike, _: Option[String]) =>
      // This is empty because FooTest does not reference Foo, only Foo.FooInner
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Double indirect via super classes to test class") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Bar {}",
        "Bar.cls"     -> "public virtual class Bar extends Foo {}",
        "Foo.cls"     -> "public virtual class Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via interface to test class") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Foo {}",
        "Foo.cls"     -> "public interface Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via interface to test class (inner interface)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public interface FooInner {}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via inner interface class to test class (interface class not tested)") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public interface FooInner { }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, _: Option[String]) =>
      // This is empty because FooTest does not reference Foo.FooInner, only Foo
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Double indirect via interfaces to test class") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Bar {}",
        "Bar.cls"     -> "public interface Bar extends Foo {}",
        "Foo.cls"     -> "public interface Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Indirect via super class & interface to test class") {
    run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Bar {}",
        "Bar.cls"     -> "public virtual class Bar implements Foo {}",
        "Foo.cls"     -> "public interface Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "FooTest")))
    }
  }

  test("Shared interface does not spider") {
    run(
      Map(
        "Bar.cls"     -> "public interface Bar {}",
        "BarTest.cls" -> "@isTest public class BarTest {{Bar a;}}",
        "Dummy.cls"   -> "public class Dummy implements Bar {}",
        "Foo.cls"     -> "public class Foo implements Bar {}",
        "FooTest.cls" -> "@isTest public class FooTest {{Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      // FooTest can be skipped as unrelated to impl of Bar
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "BarTest")))

      // Changing interface could impact Foo although it would likely need fixing for deploy
      assert(
        getTestClassNames(root, Array("Bar.cls")) == Set(
          withNamespace(ns, "BarTest"),
          withNamespace(ns, "FooTest")
        )
      )

      // If we did change the interface & impl
      assert(
        getTestClassNames(root, Array("Bar.cls", "Foo.cls")) == Set(
          withNamespace(ns, "BarTest"),
          withNamespace(ns, "FooTest")
        )
      )
    }
  }

  test("Shared inner interface does not spider") {
    run(
      Map(
        "Bar.cls"     -> "public class Bar { public interface BarInner {}}",
        "BarTest.cls" -> "@isTest public class BarTest {{Bar.BarInner a;}}",
        "Dummy.cls"   -> "public class Dummy implements Bar.BarInner {}",
        "Foo.cls"     -> "public class Foo implements Bar.BarInner {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      // FooTest can be skipped as unrelated to impl of Bar
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set(withNamespace(ns, "BarTest")))

      // Changing interface could impact Foo although it would likely need fixing for deploy
      assert(
        getTestClassNames(root, Array("Bar.cls")) == Set(
          withNamespace(ns, "BarTest"),
          withNamespace(ns, "FooTest")
        )
      )

      // If we did change the interface & impl
      assert(
        getTestClassNames(root, Array("Bar.cls", "Foo.cls")) == Set(
          withNamespace(ns, "BarTest"),
          withNamespace(ns, "FooTest")
        )
      )
    }
  }

  test("Shared interface spiders over abstract class") {
    run(
      Map(
        "Bar.cls"     -> "public interface Bar {}",
        "BarTest.cls" -> "@isTest public class BarTest {{Bar a;}}",
        "Dummy.cls"   -> "public class Dummy implements Bar {}",
        "Foo.cls"     -> "public abstract class Foo implements Bar {}",
        "FooTest.cls" -> "@isTest public class FooTest {{Foo a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      // Using abstract is not special here, just verifying an old problem does not return
      assert(
        getTestClassNames(root, Array("Bar.cls")) == Set(
          withNamespace(ns, "BarTest"),
          withNamespace(ns, "FooTest")
        )
      )
    }
  }

  test("Shared base class should not spider") {
    run(
      Map(
        "Bar.cls"     -> "public virtual class Bar {}",
        "Foo.cls"     -> "public class Foo extends Bar {}",
        "Baz.cls"     -> "public class Baz extends Bar {{Bar a;}}",
        "BazTest.cls" -> "@isTest public class BazTest {{Baz a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Foo.cls")).isEmpty)
      assert(getTestClassNames(root, Array("Bar.cls")) == Set(withNamespace(ns, "BazTest")))
    }
  }

  test("Shared base class with direct reference should spider") {
    run(
      Map(
        "Bar.cls"     -> "public virtual class Bar {}",
        "Foo.cls"     -> "public class Foo extends Bar {}",
        "Baz.cls"     -> "public class Baz extends Bar {{Foo a;}}",
        "BazTest.cls" -> "@isTest public class BazTest {{Baz a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Foo.cls")) == Set(withNamespace(ns, "BazTest")))
      assert(getTestClassNames(root, Array("Bar.cls")) == Set(withNamespace(ns, "BazTest")))
    }
  }

  test("Shared base class with intermediate super class should not spider") {
    run(
      Map(
        "Bar.cls"      -> "public virtual class Bar {}",
        "Foo.cls"      -> "public class Foo extends Bar {}",
        "BazSuper.cls" -> "public virtual class BazSuper extends Bar {{Bar a;}}",
        "Baz.cls"      -> "public class Baz extends BazSuper {{Bar a;}}",
        "BazTest.cls"  -> "@isTest public class BazTest {{Baz a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Foo.cls")).isEmpty)
      assert(getTestClassNames(root, Array("Bar.cls")) == Set(withNamespace(ns, "BazTest")))
    }
  }

  test("Shared base class with intermediate super and direct reference should spider") {
    run(
      Map(
        "Bar.cls"      -> "public virtual class Bar {}",
        "Foo.cls"      -> "public class Foo extends Bar {}",
        "BazSuper.cls" -> "public virtual class BazSuper extends Bar {{Bar a;}}",
        "Baz.cls"      -> "public class Baz extends BazSuper {{Foo a;}}",
        "BazTest.cls"  -> "@isTest public class BazTest {{Baz a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Foo.cls")) == Set(withNamespace(ns, "BazTest")))
      assert(getTestClassNames(root, Array("Bar.cls")) == Set(withNamespace(ns, "BazTest")))
    }
  }

  test("Shared base class via outer should not spider") {
    run(
      Map(
        "Bar.cls"     -> "public virtual class Bar {}",
        "Foo.cls"     -> "public class Foo extends Bar {}",
        "Baz.cls"     -> "public class Baz extends Bar {public class Inner {{Bar a;}} }",
        "BazTest.cls" -> "@isTest public class BazTest {{Baz.Inner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Foo.cls")).isEmpty)
      assert(getTestClassNames(root, Array("Bar.cls")) == Set(withNamespace(ns, "BazTest")))
    }
  }

  test("Shared base class via outer with direct reference should spider") {
    run(
      Map(
        "Bar.cls"     -> "public virtual class Bar {}",
        "Foo.cls"     -> "public class Foo extends Bar {}",
        "Baz.cls"     -> "public class Baz extends Bar {public class Inner {{Foo a;}} }",
        "BazTest.cls" -> "@isTest public class BazTest {{Baz.Inner a;}}"
      )
    ) { (root: PathLike, ns: Option[String]) =>
      assert(getTestClassNames(root, Array("Foo.cls")) == Set(withNamespace(ns, "BazTest")))
      assert(getTestClassNames(root, Array("Bar.cls")) == Set(withNamespace(ns, "BazTest")))
    }
  }

}
