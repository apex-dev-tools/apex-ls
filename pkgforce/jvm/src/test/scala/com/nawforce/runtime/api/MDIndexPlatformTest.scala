/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexPlatformTest extends AnyFunSuite {

  test("Platform class is loaded (MDAPI)") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.String") != null)
    }
  }

  test("Unknown platform class is not loaded (MDAPI)") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.Foo") == null)
    }
  }

  test("Platform class is loaded (SFDX)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}] }",
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.String") != null)
    }
  }

  test("Unknown platform class is not loaded (SFDX)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}] }",
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.Foo") == null)
    }
  }

  test("Platform generic class is loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.List<System.String>") != null)
    }
  }

  test("Platform generic with bad name is not loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.Listy<System.String>") == null)
    }
  }

  test("Platform generic with wrong type args is not loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.List<System.String,System.String>") == null)
    }
  }

  test("Platform generic with multiple args is loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System.Map<System.String,System.String>") != null)
    }
  }

  test("System platform class without namespace is loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("String") != null)
    }
  }

  test("System platform generic class without namespace is loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("List<String>") != null)
    }
  }

  test("Platform system class is loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("System") != null)
    }
  }

  test("Platform inner class is loaded") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Database.DMLOptions.EmailHeader") != null)
    }
  }

}
