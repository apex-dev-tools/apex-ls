/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexInterfaceTest extends AnyFunSuite {

  test("Implements outer") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "public class Foo implements Bar {}", "Bar.cls" -> "public interface Bar {}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 1)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "Bar")
      assert(interfaces.get(0).getApexNamespace.isEmpty)
      assert(interfaces.get(0).getEnclosingType == null)
    }
  }

  test("Implements inner") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "public class Foo implements Bar {public interface Bar {}}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 1)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "Foo.Bar")
      assert(interfaces.get(0).getApexNamespace.isEmpty)
      assert(interfaces.get(0).getEnclosingType != null)
    }
  }

  test("Implements peer inner") {
    FileSystemHelper.run(
      Map(
        "Bar.cls" -> "public class Bar {public class Foo implements Baz {} public interface Baz {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("Bar.Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 1)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "Bar.Baz")
      assert(interfaces.get(0).getApexNamespace.isEmpty)
      assert(interfaces.get(0).getEnclosingType != null)
    }
  }

  test("Implements multiple") {
    FileSystemHelper.run(
      Map(
        "Foo.cls" -> "public class Foo implements Bar, Baz {}",
        "Bar.cls" -> "public interface Bar {}",
        "Baz.cls" -> "public interface Baz {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 2)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "Bar")
      assert(interfaces.get(0).getApexNamespace.isEmpty)
      assert(interfaces.get(0).getEnclosingType == null)
      assert(interfaces.get(1).isResolved)
      assert(interfaces.get(1).getApexName == "Baz")
      assert(interfaces.get(1).getApexNamespace.isEmpty)
      assert(interfaces.get(1).getEnclosingType == null)
    }
  }

  test("Implements outer (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo implements Bar {}",
        "sources/Bar.cls"   -> "public interface Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("ns.Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 1)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "ns.Bar")
      assert(interfaces.get(0).getApexNamespace == "ns")
      assert(interfaces.get(0).getEnclosingType == null)
    }
  }

  test("Implements inner (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo implements Bar {public interface Bar {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("ns.Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 1)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "ns.Foo.Bar")
      assert(interfaces.get(0).getApexNamespace == "ns")
      assert(interfaces.get(0).getEnclosingType != null)
    }
  }

  test("Implements peer inner (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Bar.cls"   -> "public class Bar {public class Foo implements Baz {} public interface Baz {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("ns.Bar.Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 1)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "ns.Bar.Baz")
      assert(interfaces.get(0).getApexNamespace == "ns")
      assert(interfaces.get(0).getEnclosingType != null)
    }
  }

  test("Implements multiple (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo implements Bar, Baz {}",
        "sources/Bar.cls"   -> "public interface Bar {}",
        "sources/Baz.cls"   -> "public interface Baz {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType    = index.findExactTypeId("ns.Foo")
      val interfaces = fooType.getInterfaces

      assert(interfaces.size() == 2)
      assert(interfaces.get(0).isResolved)
      assert(interfaces.get(0).getApexName == "ns.Bar")
      assert(interfaces.get(0).getApexNamespace == "ns")
      assert(interfaces.get(0).getEnclosingType == null)
      assert(interfaces.get(1).isResolved)
      assert(interfaces.get(1).getApexName == "ns.Baz")
      assert(interfaces.get(1).getApexNamespace == "ns")
      assert(interfaces.get(1).getEnclosingType == null)
    }
  }

}
