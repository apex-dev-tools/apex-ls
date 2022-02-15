/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexTest extends AnyFunSuite {

  test("Empty org") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index != null)
      assert(index.getFilesWithErrors.isEmpty)
    }
  }

  test("File with error") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "",
      "Bar.cls" -> "public class Bar {}"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index != null)
      assert(index.getFilesWithErrors.toArray sameElements Array(root.join("Foo.cls").toString))
    }
  }

  test("Files with error") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "",
      "Bar.cls" -> ""
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index != null)
      assert(index.getFilesWithErrors.toArray sameElements
        Array(root.join("Bar.cls").toString, root.join("Foo.cls").toString))
    }
  }

  test("MDAPI class is loaded") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo") != null)
      assert(index.findExactTypeId("FoO") != null)
      assert(index.findExactTypeId("Baz") == null)
    }
  }

  test("SFDX class is loaded") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}] }",
        "sources/Foo.cls"   -> "public class Foo {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo") != null)
      assert(index.findExactTypeId("FOo") != null)
      assert(index.findExactTypeId("Baz") == null)
    }
  }

  test("SFDX namespaced class is loaded") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("ns.Foo") != null)
      assert(index.findExactTypeId("ns.foo") != null)
      assert(index.findExactTypeId("Foo") == null)
      assert(index.findExactTypeId("ns.Baz") == null)
    }
  }
}
