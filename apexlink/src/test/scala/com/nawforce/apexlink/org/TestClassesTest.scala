/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class TestClassesTest extends AnyFunSuite with TestHelper {

  def getTestClassNames(root: PathLike, paths: Array[String]): Set[String] = {
    withOrg(org => {
      org.getTestClassNamesInternal(paths.map(p => root.join(p)))
    })
  }

  test("Empty request") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array()).isEmpty)
    }
  }

  test("No tests") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Single test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"     -> "public class Dummy {}",
        "DummyTest.cls" -> "@isTest public class DummyTest { {Dummy a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("DummyTest"))
      assert(getTestClassNames(root, Array("DummyTest.cls")) == Set("DummyTest"))
    }
  }

  test("Single test class (inner)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"     -> "public class Dummy { public class DummyInner {} }",
        "DummyTest.cls" -> "@isTest public class DummyTest { {Dummy.DummyInner a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("DummyTest"))
    }
  }

  test("Multiple test classes") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"      -> "public class Dummy {}",
        "DummyTest.cls"  -> "@isTest public class DummyTest { {Dummy a;}}",
        "DummyTest2.cls" -> "@isTest public class DummyTest2 { {Dummy a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("DummyTest", "DummyTest2"))
      assert(getTestClassNames(root, Array("DummyTest.cls")) == Set("DummyTest"))
      assert(getTestClassNames(root, Array("DummyTest2.cls")) == Set("DummyTest2"))
    }
  }

  test("Multiple test classes (inner)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"      -> "public class Dummy { public class DummyInner {} }",
        "DummyTest.cls"  -> "@isTest public class DummyTest { {Dummy.DummyInner a;}}",
        "DummyTest2.cls" -> "@isTest public class DummyTest2 { {Dummy.DummyInner a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("DummyTest", "DummyTest2"))
      assert(getTestClassNames(root, Array("DummyTest.cls")) == Set("DummyTest"))
      assert(getTestClassNames(root, Array("DummyTest2.cls")) == Set("DummyTest2"))
    }
  }

  test("Indirect test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy {}",
        "Foo.cls"     -> "public class Foo { {Dummy a;}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect test class (inner)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy { public class DummyInner {} }",
        "Foo.cls"     -> "public class Foo { {Dummy.DummyInner a;}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect test class (indirect inner)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy { }",
        "Foo.cls"     -> "public class Foo { public class FooInner { {Dummy a;} }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via super class to test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo {}",
        "Foo.cls"     -> "public virtual class Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via super class to test class (super class inner)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public virtual class FooInner { }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via inner super class to test class (super class not tested)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public virtual class FooInner { }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      // This is empty because FooTest does not reference Foo.FooInner, only Foo
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Indirect via super class to test class (super class not tested)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Foo {}",
        "Foo.cls"     -> "public virtual class Foo {public class FooInner {}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      // This is empty because FooTest does not reference Foo, only Foo.FooInner
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Double indirect via super classes to test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Bar {}",
        "Bar.cls"     -> "public virtual class Bar extends Foo {}",
        "Foo.cls"     -> "public virtual class Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via interface to test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Foo {}",
        "Foo.cls"     -> "public interface Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via interface to test class (inner interface)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public interface FooInner {}}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo.FooInner a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via inner interface class to test class (interface class not tested)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Foo.FooInner {}",
        "Foo.cls"     -> "public class Foo {public interface FooInner { }}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      // This is empty because FooTest does not reference Foo.FooInner, only Foo
      assert(getTestClassNames(root, Array("Dummy.cls")).isEmpty)
    }
  }

  test("Double indirect via interfaces to test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy implements Bar {}",
        "Bar.cls"     -> "public interface Bar extends Foo {}",
        "Foo.cls"     -> "public interface Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Indirect via super class & interface to test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"   -> "public class Dummy extends Bar {}",
        "Bar.cls"     -> "public virtual class Bar implements Foo {}",
        "Foo.cls"     -> "public interface Foo {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("FooTest"))
    }
  }

  test("Shared interface does not spider") {
    FileSystemHelper.run(
      Map(
        "Bar.cls"     -> "public interface Bar {}",
        "BarTest.cls" -> "@isTest public class BarTest {{Bar a;}}",
        "Dummy.cls"   -> "public class Dummy implements Bar {}",
        "Foo.cls"     -> "public class Foo implements Bar {}",
        "FooTest.cls" -> "@isTest public class FooTest {{Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      // FooTest can be skipped as unrelated to impl of Bar
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("BarTest"))

      // Changing interface could impact Foo although it would likely need fixing for deploy
      assert(getTestClassNames(root, Array("Bar.cls")) == Set("BarTest", "FooTest"))

      // If we did change the interface & impl
      assert(getTestClassNames(root, Array("Bar.cls", "Foo.cls")) == Set("BarTest", "FooTest"))
    }
  }

  test("Shared inner interface does not spider") {
    FileSystemHelper.run(
      Map(
        "Bar.cls"     -> "public class Bar { public interface BarInner {}}",
        "BarTest.cls" -> "@isTest public class BarTest {{Bar.BarInner a;}}",
        "Dummy.cls"   -> "public class Dummy implements Bar.BarInner {}",
        "Foo.cls"     -> "public class Foo implements Bar.BarInner {}",
        "FooTest.cls" -> "@isTest public class FooTest { {Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      // FooTest can be skipped as unrelated to impl of Bar
      assert(getTestClassNames(root, Array("Dummy.cls")) == Set("BarTest"))

      // Changing interface could impact Foo although it would likely need fixing for deploy
      assert(getTestClassNames(root, Array("Bar.cls")) == Set("BarTest", "FooTest"))

      // If we did change the interface & impl
      assert(getTestClassNames(root, Array("Bar.cls", "Foo.cls")) == Set("BarTest", "FooTest"))
    }
  }

  test("Shared interface spiders over abstract class") {
    FileSystemHelper.run(
      Map(
        "Bar.cls"     -> "public interface Bar {}",
        "BarTest.cls" -> "@isTest public class BarTest {{Bar a;}}",
        "Dummy.cls"   -> "public class Dummy implements Bar {}",
        "Foo.cls"     -> "public abstract class Foo implements Bar {}",
        "FooTest.cls" -> "@isTest public class FooTest {{Foo a;}}"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      // Using abstract is not special here, just verifying an old problem does not return
      assert(getTestClassNames(root, Array("Bar.cls")) == Set("BarTest", "FooTest"))
    }
  }

}
