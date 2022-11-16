/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import io.github.apexdevtools.apexls.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexFieldTest extends AnyFunSuite {

  test("Basic field") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {String a;}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getType.getApexName == "System.String")
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Self reference field") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {private static Foo a = null;}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val fields  = fooType.getFields

        assert(fields.size() == 1)
        assert(fields.get(0).getMemberType == "FIELD")
        assert(fields.get(0).getModifiers == "private static")
        assert(fields.get(0).getType.isResolved)
        assert(fields.get(0).getType.getApexName == "Foo")
        assert(fields.get(0).getFieldName == "a")
        assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Unknown type field") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {Bar a;}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "Bar")
      assert(!fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Local type field") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "public class Foo {Bar a;}", "Bar.cls" -> "public class Bar {}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "Bar")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Basic field (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {String a;}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getType.getApexName == "System.String")
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Self reference field (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {private static Foo a = null;}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "private static")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getType.getApexName == "ns.Foo")
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Unknown type field (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {Bar a;}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "Bar")
      assert(!fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Local type field (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {Bar a;}",
        "sources/Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "ns.Bar")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Namespaced Local type field (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {ns.Bar a;}",
        "sources/Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "FIELD")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "ns.Bar")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Basic property") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {String a {get; set;} }")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val fields  = fooType.getFields

        assert(fields.size() == 1)
        assert(fields.get(0).getMemberType == "PROPERTY")
        assert(fields.get(0).getModifiers == "")
        assert(fields.get(0).getType.isResolved)
        assert(fields.get(0).getType.getApexName == "System.String")
        assert(fields.get(0).getFieldName == "a")
        assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Self reference property") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {private static Foo a {get; set;}}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val fields  = fooType.getFields

        assert(fields.size() == 1)
        assert(fields.get(0).getMemberType == "PROPERTY")
        assert(fields.get(0).getModifiers == "private static")
        assert(fields.get(0).getType.isResolved)
        assert(fields.get(0).getType.getApexName == "Foo")
        assert(fields.get(0).getFieldName == "a")
        assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Unknown type property") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {Bar a {get; set;}}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val fields  = fooType.getFields

        assert(fields.size() == 1)
        assert(fields.get(0).getMemberType == "PROPERTY")
        assert(fields.get(0).getModifiers == "")
        assert(fields.get(0).getType.getApexName == "Bar")
        assert(!fields.get(0).getType.isResolved)
        assert(fields.get(0).getFieldName == "a")
        assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Local type property") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "public class Foo {Bar a {get; set;}}", "Bar.cls" -> "public class Bar {}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "PROPERTY")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "Bar")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "Foo")
    }
  }

  test("Basic property (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {String a {get; set;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "PROPERTY")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getType.getApexName == "System.String")
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Self reference property (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {private static Foo a {get; set;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "PROPERTY")
      assert(fields.get(0).getModifiers == "private static")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getType.getApexName == "ns.Foo")
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Unknown type property (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {Bar a {get; set;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "PROPERTY")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "Bar")
      assert(!fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Local type property (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {Bar a {get; set;}}",
        "sources/Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "PROPERTY")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "ns.Bar")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

  test("Namespaced Local type property (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {ns.Bar a {get; set;}}",
        "sources/Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val fields  = fooType.getFields

      assert(fields.size() == 1)
      assert(fields.get(0).getMemberType == "PROPERTY")
      assert(fields.get(0).getModifiers == "")
      assert(fields.get(0).getType.getApexName == "ns.Bar")
      assert(fields.get(0).getType.isResolved)
      assert(fields.get(0).getFieldName == "a")
      assert(fields.get(0).getOwner.getApexName == "ns.Foo")
    }
  }

}
