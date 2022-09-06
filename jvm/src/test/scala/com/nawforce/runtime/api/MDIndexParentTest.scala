/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexParentTest extends AnyFunSuite {

  test("Extends outer class") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "public class Foo extends Bar {}", "Bar.cls" -> "public class Bar {}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val barType = fooType.getParent

      assert(barType != null)
      assert(barType.isResolved)
      assert(barType.getApexName == "Bar")
      assert(barType.getApexNamespace.isEmpty)
      assert(barType.getEnclosingType == null)
    }
  }

  test("Extends inner class") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo extends Bar {public class Bar {}}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val barType = fooType.getParent

        assert(barType != null)
        assert(barType.isResolved)
        assert(barType.getApexName == "Foo.Bar")
        assert(barType.getApexNamespace.isEmpty)
        assert(barType.getEnclosingType != null)
    }
  }

  test("Extends outer self class") {
    FileSystemHelper.run(Map("Bar.cls" -> "public class Bar {public class Foo extends Bar {}}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Bar.Foo")
        val barType = fooType.getParent

        assert(barType != null)
        assert(barType.isResolved)
        assert(barType.getApexName == "Bar")
        assert(barType.getApexNamespace.isEmpty)
        assert(barType.getEnclosingType == null)
    }
  }

  test("Extends peer inner class") {
    FileSystemHelper.run(
      Map("Bar.cls" -> "public class Bar {public class Foo extends Baz {} public class Baz {}}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Bar.Foo")
      val barType = fooType.getParent

      assert(barType != null)
      assert(barType.isResolved)
      assert(barType.getApexName == "Bar.Baz")
      assert(barType.getApexNamespace.isEmpty)
      assert(barType.getEnclosingType != null)
    }
  }

  test("Extends outer class (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo extends Bar {}",
        "sources/Bar.cls"   -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val barType = fooType.getParent

      assert(barType != null)
      assert(barType.isResolved)
      assert(barType.getApexName == "ns.Bar")
      assert(barType.getApexNamespace == "ns")
      assert(barType.getEnclosingType == null)
    }
  }

  test("Extends inner class (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo extends Bar {public class Bar {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val barType = fooType.getParent

      assert(barType != null)
      assert(barType.isResolved)
      assert(barType.getApexName == "ns.Foo.Bar")
      assert(barType.getApexNamespace == "ns")
      assert(barType.getEnclosingType != null)
    }
  }

  test("Extends outer self class (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Bar.cls"   -> "public class Bar {public class Foo extends Bar {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Bar.Foo")
      val barType = fooType.getParent

      assert(barType != null)
      assert(barType.isResolved)
      assert(barType.getApexName == "ns.Bar")
      assert(barType.getApexNamespace == "ns")
      assert(barType.getEnclosingType == null)
    }
  }

  test("Extends peer inner class (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Bar.cls"   -> "public class Bar {public class Foo extends Baz {} public class Baz {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Bar.Foo")
      val barType = fooType.getParent

      assert(barType != null)
      assert(barType.isResolved)
      assert(barType.getApexName == "ns.Bar.Baz")
      assert(barType.getApexNamespace == "ns")
      assert(barType.getEnclosingType != null)
    }
  }

}
