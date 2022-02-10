/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api

import com.nawforce.pkgforce.TestHelper
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class MDIndexTest extends AnyFunSuite with TestHelper {

  test("Empty org") {
    virtualFS(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index != null)
    }
  }

  test("MDAPI class is loaded") {
    virtualFS(Map("Foo.cls" -> "public class Foo {}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo") != null)
      assert(index.findExactTypeId("FoO") != null)
      assert(index.findExactTypeId("Baz") == null)
    }
  }

  test("SFDX class is loaded") {
    virtualFS(
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
    virtualFS(
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
