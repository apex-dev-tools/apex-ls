/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexModifierTest extends AnyFunSuite {

  test("Single modifier") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").getModifiers == "public")
    }
  }

  test("Dual modifiers") {
    FileSystemHelper.run(Map("Foo.cls" -> "public  abstract class Foo {}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").getModifiers == "public abstract")
    }
  }

  test("Single annotation") {
    FileSystemHelper.run(Map("Foo.cls" -> "@isTest class Foo {}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").getModifiers == "@isTest")
    }
  }

  test("Dual annotations") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "@isTest(isParallel = true)  @SuppressWarnings(' PMD') class Foo {}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(
        index
          .findExactTypeId("Foo")
          .getModifiers == "@isTest(isParallel=true) @SuppressWarnings(' PMD')"
      )
    }
  }

  test("Mixed modifiers & annotations") {
    FileSystemHelper.run(Map("Foo.cls" -> "@SuppressWarnings protected abstract class Foo {}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)
        assert(index.findExactTypeId("Foo").getModifiers == "@SuppressWarnings protected abstract")
    }
  }
}
