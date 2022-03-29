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
      assert(index.fuzzyFindTypeId("Foo").getApexName == "Foo")
      assert(index.fuzzyFindTypeId("FO").getApexName == "Foo")
      assert(index.fuzzyFindTypeId("f").getApexName == "Foo")
      assert(index.fuzzyFindTypeId("Foob").getApexName == "FooBar")
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
      assert(index.fuzzyFindTypeId("ns.Foo").getApexName == "ns.Foo")
      assert(index.fuzzyFindTypeId("ns.FO").getApexName == "ns.Foo")
      assert(index.fuzzyFindTypeId("ns.f").getApexName == "ns.Foo")
      assert(index.fuzzyFindTypeId("ns.Foob").getApexName == "ns.FooBar")
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
      assert(fooFind.get(0).getApexName == "Foo")
      assert(fooFind.get(1).getApexName == "FooBar")
      assert(index.fuzzyFindTypeIds("FO").size() == 2)
      assert(index.fuzzyFindTypeIds("f").size() == 2)
      assert(index.fuzzyFindTypeIds("Foob").get(0).getApexName == "FooBar")
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
      assert(fooFind.get(0).getApexName == "ns.Foo")
      assert(fooFind.get(1).getApexName == "ns.FooBar")
      assert(index.fuzzyFindTypeIds("ns.FO").size() == 2)
      assert(index.fuzzyFindTypeIds("ns.f").size() == 2)
      assert(index.fuzzyFindTypeIds("ns.Foob").get(0).getApexName == "ns.FooBar")
      assert(index.fuzzyFindTypeIds("Foo").isEmpty)
      assert(index.fuzzyFindTypeIds("ns.O").isEmpty)
      assert(index.fuzzyFindTypeIds("ns.X").isEmpty)
      assert(index.fuzzyFindTypeIds("").isEmpty)
    }
  }

  test("MDAPI find by namespace") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { }",
      "FooBar.cls" -> "public class FooBar { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val findResults = index.findTypeIdsByNamespace("")
      assert(findResults.size() == 3)
      assert(findResults.get(0).getApexName == "Foo")
      assert(findResults.get(1).getApexName == "FooBar")
      assert(findResults.get(2).getApexName == "FooBar.Bar")

      assert(index.findTypeIdsByNamespace("F").isEmpty)
    }
  }

  test("SFDX find by namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {}",
        "sources/FooBar.cls" -> "public class FooBar { public class Bar {} }"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val findResults = index.findTypeIdsByNamespace("ns")
      assert(findResults.size() == 3)
      assert(findResults.get(0).getApexName == "ns.Foo")
      assert(findResults.get(1).getApexName == "ns.FooBar")
      assert(findResults.get(2).getApexName == "ns.FooBar.Bar")

      val findResults2 = index.findTypeIdsByNamespace("n")
      assert(findResults2.size() == 3)
      assert(findResults2.get(0).getApexName == "ns.Foo")
      assert(findResults2.get(1).getApexName == "ns.FooBar")
      assert(findResults2.get(2).getApexName == "ns.FooBar.Bar")

      assert(index.findTypeIdsByNamespace("F").isEmpty)
      assert(index.findTypeIdsByNamespace("").isEmpty)
    }
  }

  test("SFDX multiple package directories") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources1\"}, {\"path\": \"sources2\"}] }",
        "sources1/Foo.cls" -> "public class Foo {}",
        "sources1/Bar.cls" -> "public class Bar {}",
        "sources2/Foo.cls" -> "public class Foo {}",
        "sources2/Baz.cls" -> "public class Baz { public class Bar {} }",
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      val sources1BarPath = root.join("sources1").join("Bar.cls").toString
      val sources2FooPath = root.join("sources2").join("Foo.cls").toString
      val sources2BazPath = root.join("sources2").join("Baz.cls").toString

      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").getFile.getFilename == sources2FooPath)
      assert(index.findExactTypeId("Bar").getFile.getFilename == sources1BarPath)
      assert(index.findExactTypeId("Baz").getFile.getFilename == sources2BazPath)
      assert(index.findExactTypeId("Baz.Bar").getFile.getFilename == sources2BazPath)

      assert(index.fuzzyFindTypeId("F").getFile.getFilename == sources2FooPath)
      assert(index.fuzzyFindTypeId("B").getFile.getFilename == sources2BazPath)

      val fFind = index.fuzzyFindTypeIds("F")
      assert(fFind.size() == 1)
      assert(fFind.get(0).getFile.getFilename == sources2FooPath)

      val bFind = index.fuzzyFindTypeIds("B")
      assert(bFind.size() == 2)
      assert(bFind.get(0).getFile.getFilename == sources1BarPath)
      assert(bFind.get(1).getFile.getFilename == sources2BazPath)

      val nsFind = index.findTypeIdsByNamespace("")
      assert(nsFind.size() == 4)
      assert(nsFind.get(0).getFile.getFilename == sources1BarPath)
      assert(nsFind.get(1).getFile.getFilename == sources2FooPath)
      assert(nsFind.get(2).getFile.getFilename == sources2BazPath)
      assert(nsFind.get(3).getFile.getFilename == sources2BazPath)
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
        "sources2/Baz.cls" -> "public class Baz { public class Bar {} }",
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      val sources1BarPath = root.join("sources1").join("Bar.cls").toString
      val sources2FooPath = root.join("sources2").join("Foo.cls").toString
      val sources2BazPath = root.join("sources2").join("Baz.cls").toString

      assert(index.hasUpdatedIssues.isEmpty)
      assert(index.findExactTypeId("Foo").getFile.getFilename == sources2FooPath)
      assert(index.findExactTypeId("Bar").getFile.getFilename == sources1BarPath)
      assert(index.findExactTypeId("Baz").getFile.getFilename == sources2BazPath)
      assert(index.findExactTypeId("Baz.Bar").getFile.getFilename == sources2BazPath)

      assert(index.fuzzyFindTypeId("F").getFile.getFilename == sources2FooPath)
      assert(index.fuzzyFindTypeId("B").getFile.getFilename == sources2BazPath)

      val fFind = index.fuzzyFindTypeIds("F")
      assert(fFind.size() == 1)
      assert(fFind.get(0).getFile.getFilename == sources2FooPath)

      val bFind = index.fuzzyFindTypeIds("B")
      assert(bFind.size() == 2)
      assert(bFind.get(0).getFile.getFilename == sources1BarPath)
      assert(bFind.get(1).getFile.getFilename == sources2BazPath)

      val nsFind = index.findTypeIdsByNamespace("")
      assert(nsFind.size() == 4)
      assert(nsFind.get(0).getFile.getFilename == sources1BarPath)
      assert(nsFind.get(1).getFile.getFilename == sources2FooPath)
      assert(nsFind.get(2).getFile.getFilename == sources2BazPath)
      assert(nsFind.get(3).getFile.getFilename == sources2BazPath)
    }
  }

  test("SFDX get resource file") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { }",
      "FooBar.cls" -> "public class FooBar { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooPath = root.join("Foo.cls").toString
      val fooBarPath = root.join("FooBar.cls").toString
      val missingPath = root.join("Missing.cls").toString

      val fooResource = index.getResourceFile(fooPath)
      assert(fooResource != null)
      assert(fooResource.getTypes.size() == 1)
      assert(fooResource.getFilename == fooPath)
      val fooBarResource = index.getResourceFile(fooBarPath)
      assert(fooBarResource != null)
      assert(fooBarResource.getTypes.size() == 2)
      assert(fooBarResource.getFilename == fooBarPath)
      val missingResource = index.getResourceFile(missingPath)
      assert(missingResource == null)
    }
  }

  test("SFDX find resource file") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { }",
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val expectedFooPath = root.join("Foo.cls").toString

      var fooPath = root.join("foo.cls").toString
      var fooResource = index.findResourceFile(fooPath)
      assert(fooResource != null)
      assert(fooResource.getTypes.size() == 1)
      assert(fooResource.getFilename == expectedFooPath)

      fooPath = root.join("FOO.CLS").toString
      fooResource = index.findResourceFile(fooPath)
      assert(fooResource != null)
      assert(fooResource.getTypes.size() == 1)
      assert(fooResource.getFilename == expectedFooPath)
    }
  }

  test("SFDX fuzzy find resource file") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { public class Zig {} }",
      "FooBar.cls" -> "public class FooBar { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val searchPath = root.join("Foo").toString
      val fooResources = index.fuzzyFindResourceFile(searchPath)
      assert(fooResources.size() == 2)

      val fooResource = fooResources.get(0)
      assert(fooResource.getTypes.size() == 2)
      val expectedFooPath = root.join("Foo.cls").toString
      assert(fooResource.getFilename == expectedFooPath)
      assert(fooResource.getTypes.stream().allMatch(_.getFile.getFilename == expectedFooPath))

      val fooBarResource = fooResources.get(1)
      assert(fooBarResource.getTypes.size() == 2)
      val expectedFooBarPath = root.join("FooBar.cls").toString
      assert(fooBarResource.getFilename == expectedFooBarPath)
      assert(fooBarResource.getTypes.stream().allMatch(_.getFile.getFilename == expectedFooBarPath))
    }
  }

  test("MDAPI ApexType") {
    FileSystemHelper.run(Map(
      "Foo.cls" -> "public class Foo { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val foo = index.findExactTypeId("Foo")
      assert(foo != null)
      assert(foo.getApexName == "Foo")
      assert(foo.getApexNamespace == "")
      assert(foo.getEnclosingType == null)

      val bar = index.findExactTypeId("Foo.Bar")
      assert(bar != null)
      assert(bar.getApexName == "Foo.Bar")
      assert(bar.getApexNamespace == "")
      assert(bar.getEnclosingType.getApexName == "Foo")

      val fooFile = foo.getFile
      assert(fooFile != null)
      assert(fooFile.getFilename == "/Foo.cls")
      assert(!fooFile.hasError)
      assert(fooFile.getTypes.size() == 2)
      assert(fooFile.getTypes.contains(foo))
      assert(fooFile.getTypes.contains(bar))

      val barFile = bar.getFile
      assert(barFile != null)
      assert(barFile.getFilename == "/Foo.cls")
      assert(!barFile.hasError)
      assert(barFile.getTypes.size() == 2)
      assert(fooFile.getTypes.contains(foo))
      assert(fooFile.getTypes.contains(bar))
    }
  }

  test("SFDX ApexType (no namespace)") {
    FileSystemHelper.run(Map(
      "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}]}",
      "sources/Foo.cls" -> "public class Foo { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val foo = index.findExactTypeId("Foo")
      assert(foo != null)
      assert(foo.getApexName == "Foo")
      assert(foo.getApexNamespace == "")
      assert(foo.getEnclosingType == null)

      val bar = index.findExactTypeId("Foo.Bar")
      assert(bar != null)
      assert(bar.getApexName == "Foo.Bar")
      assert(bar.getApexNamespace == "")
      assert(bar.getEnclosingType.getApexName == "Foo")

      val fooFile = foo.getFile
      assert(fooFile != null)
      assert(fooFile.getFilename == "/sources/Foo.cls")
      assert(!fooFile.hasError)
      assert(fooFile.getTypes.size() == 2)
      assert(fooFile.getTypes.contains(foo))
      assert(fooFile.getTypes.contains(bar))

      val barFile = bar.getFile
      assert(barFile != null)
      assert(barFile.getFilename == "/sources/Foo.cls")
      assert(!barFile.hasError)
      assert(barFile.getTypes.size() == 2)
      assert(fooFile.getTypes.contains(foo))
      assert(fooFile.getTypes.contains(bar))
    }
  }

  test("SFDX ApexType (namespaced)") {
    FileSystemHelper.run(Map(
      "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
      "sources/Foo.cls" -> "public class Foo { public class Bar {} }"
    )) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val foo = index.findExactTypeId("ns.Foo")
      assert(foo != null)
      assert(foo.getApexName == "ns.Foo")
      assert(foo.getApexNamespace == "ns")
      assert(foo.getEnclosingType == null)

      val bar = index.findExactTypeId("ns.Foo.Bar")
      assert(bar != null)
      assert(bar.getApexName == "ns.Foo.Bar")
      assert(bar.getApexNamespace == "ns")
      assert(bar.getEnclosingType.getApexName == "ns.Foo")

      val fooFile = foo.getFile
      assert(fooFile != null)
      assert(fooFile.getFilename == "/sources/Foo.cls")
      assert(!fooFile.hasError)
      assert(fooFile.getTypes.size() == 2)
      assert(fooFile.getTypes.contains(foo))
      assert(fooFile.getTypes.contains(bar))

      val barFile = bar.getFile
      assert(barFile != null)
      assert(barFile.getFilename == "/sources/Foo.cls")
      assert(!barFile.hasError)
      assert(barFile.getTypes.size() == 2)
      assert(fooFile.getTypes.contains(foo))
      assert(fooFile.getTypes.contains(bar))
    }
  }

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
