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
      assert(index.findExactTypeId("") == null)
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
      assert(index.findExactTypeId("") == null)
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
      assert(index.findExactTypeId("") == null)
    }
  }

  test("Nested class is loaded") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo { public class Bar {} }")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo") != null)
      assert(index.findExactTypeId("Foo.Bar") != null)
    }
  }

  test("MDAPI fuzzy find") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { }",
      "FooBar.cls" -> "public class FooBar { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.fuzzyFindTypeId("Foo").id.get.toString == "Foo")
      assert(index.fuzzyFindTypeId("FO").id.get.toString == "Foo")
      assert(index.fuzzyFindTypeId("f").id.get.toString == "Foo")
      assert(index.fuzzyFindTypeId("Foob").id.get.toString == "FooBar")
      assert(index.fuzzyFindTypeId("Fox") == null)
      assert(index.fuzzyFindTypeId("O") == null)
      assert(index.fuzzyFindTypeId("X") == null)
      assert(index.fuzzyFindTypeId("") == null)
    }
  }

  test("SFDX namespaced fuzzy find") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {}",
        "sources/FooBar.cls" -> "public class FooBar { public class Bar {} }"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.fuzzyFindTypeId("ns.Foo").id.get.toString == "Foo")
      assert(index.fuzzyFindTypeId("ns.FO").id.get.toString == "Foo")
      assert(index.fuzzyFindTypeId("ns.f").id.get.toString == "Foo")
      assert(index.fuzzyFindTypeId("ns.Foob").id.get.toString == "FooBar")
      assert(index.fuzzyFindTypeId("Foo") == null)
      assert(index.fuzzyFindTypeId("ns.Fox") == null)
      assert(index.fuzzyFindTypeId("") == null)
    }
  }

  test("MDAPI fuzzy find list") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { }",
      "FooBar.cls" -> "public class FooBar { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooFind = index.fuzzyFindTypeIds("Foo")
      assert(fooFind.size() == 2)
      assert(fooFind.get(0).id.get.toString == "Foo")
      assert(fooFind.get(1).id.get.toString == "FooBar")
      assert(index.fuzzyFindTypeIds("FO").size() == 2)
      assert(index.fuzzyFindTypeIds("f").size() == 2)
      assert(index.fuzzyFindTypeIds("Foob").get(0).id.get.toString == "FooBar")
      assert(index.fuzzyFindTypeIds("Fox").isEmpty)
      assert(index.fuzzyFindTypeIds("O").isEmpty)
      assert(index.fuzzyFindTypeIds("X").isEmpty)
      assert(index.fuzzyFindTypeIds("").isEmpty)
    }
  }

  test("SFDX namespaced fuzzy find list") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {}",
        "sources/FooBar.cls" -> "public class FooBar { public class Bar {} }"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooFind = index.fuzzyFindTypeIds("ns.Foo")
      assert(fooFind.size() == 2)
      assert(fooFind.get(0).id.get.toString == "Foo")
      assert(fooFind.get(1).id.get.toString == "FooBar")
      assert(index.fuzzyFindTypeIds("ns.FO").size() == 2)
      assert(index.fuzzyFindTypeIds("ns.f").size() == 2)
      assert(index.fuzzyFindTypeIds("ns.Foob").get(0).id.get.toString == "FooBar")
      assert(index.fuzzyFindTypeIds("Foo").isEmpty)
      assert(index.fuzzyFindTypeIds("ns.O").isEmpty)
      assert(index.fuzzyFindTypeIds("ns.X").isEmpty)
      assert(index.fuzzyFindTypeIds("").isEmpty)
    }
  }

  test("SFDX multiple package directories") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources1\"}, {\"path\": \"sources2\"}] }",
        "sources1/Foo.cls" -> "public class Foo {}",
        "sources1/Bar.cls" -> "public class Bar {}",
        "sources2/Foo.cls" -> "public class Foo {}",
        "sources2/Baz.cls" -> "public class Baz {}",
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      val sources1BarPath = root.join("sources1").join("Bar.cls").toString
      val sources2FooPath = root.join("sources2").join("Foo.cls").toString
      val sources2BazPath = root.join("sources2").join("Baz.cls").toString

      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").path == sources2FooPath)
      assert(index.findExactTypeId("Bar").path == sources1BarPath)
      assert(index.findExactTypeId("Baz").path == sources2BazPath)

      assert(index.fuzzyFindTypeId("F").path == sources2FooPath)
      assert(index.fuzzyFindTypeId("B").path == sources2BazPath)

      val fFind = index.fuzzyFindTypeIds("F")
      assert(fFind.size() == 1)
      assert(fFind.get(0).path == sources2FooPath)

      val bFind = index.fuzzyFindTypeIds("B")
      assert(bFind.size() == 2)
      assert(bFind.get(0).path == sources1BarPath)
      assert(bFind.get(1).path == sources2BazPath)
    }
  }

  test("SFDX multiple packages") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [
            |    { "package": "first", "versionNumber": "1.2.3.4", "path": "sources1"},
            |    { "package": "second", "versionNumber": "4.5.6.7", "path": "sources2",
            |      "dependencies": [
            |        {"package": "first", "versionNumber": "1.2.3.4" }
            |      ]
            |    }
            |  ]
            |}
            |""".stripMargin,
        "sources1/Foo.cls" -> "public class Foo {}",
        "sources1/Bar.cls" -> "public class Bar {}",
        "sources2/Foo.cls" -> "public class Foo {}",
        "sources2/Baz.cls" -> "public class Baz {}",
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      val sources1BarPath = root.join("sources1").join("Bar.cls").toString
      val sources2FooPath = root.join("sources2").join("Foo.cls").toString
      val sources2BazPath = root.join("sources2").join("Baz.cls").toString

      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").path == sources2FooPath)
      assert(index.findExactTypeId("Bar").path == sources1BarPath)
      assert(index.findExactTypeId("Baz").path == sources2BazPath)

      assert(index.fuzzyFindTypeId("F").path == sources2FooPath)
      assert(index.fuzzyFindTypeId("B").path == sources2BazPath)

      val fFind = index.fuzzyFindTypeIds("F")
      assert(fFind.size() == 1)
      assert(fFind.get(0).path == sources2FooPath)

      val bFind = index.fuzzyFindTypeIds("B")
      assert(bFind.size() == 2)
      assert(bFind.get(0).path == sources1BarPath)
      assert(bFind.get(1).path == sources2BazPath)
    }
  }

}
