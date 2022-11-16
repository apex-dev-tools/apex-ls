/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import io.github.apexdevtools.apexls.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexMethodTest extends AnyFunSuite {

  test("Basic method") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {String func() {return null;}}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val methods = fooType.getMethods

        assert(methods.size() == 1)
        assert(methods.get(0).getMethodName == "func")
        assert(!methods.get(0).isConstructor)
        assert(methods.get(0).getModifiers == "")
        assert(methods.get(0).getReturnType.isResolved)
        assert(methods.get(0).getReturnType.getApexName == "System.String")
        assert(methods.get(0).getParameters.size() == 0)

        assert(methods.get(0).getDefiningType.getApexName == "Foo")
        val location = methods.get(0).getLocation
        assert(location.getLine == 1)
        assert(location.getColumn == 19)
        assert(location.getStartIndex == 18)
        assert(location.getEndIndex == 46)
    }
  }

  test("Self referencing return method") {
    FileSystemHelper.run(
      Map("Foo.cls" -> "public class Foo {public static Foo func(String a) {return null;}}")
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "func")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "public static")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "Foo")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 1)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")

      assert(methods.get(0).getDefiningType.getApexName == "Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 65)
    }
  }

  test("Unknown type return method") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {Bar func(Foo a) {return null;}}")) {
      root: PathLike =>
        val index = new MDIndex(root)
        assert(index.hasUpdatedIssues.isEmpty)

        val fooType = index.findExactTypeId("Foo")
        val methods = fooType.getMethods

        assert(methods.size() == 1)
        assert(methods.get(0).getMethodName == "func")
        assert(!methods.get(0).isConstructor)
        assert(methods.get(0).getModifiers == "")
        assert(!methods.get(0).getReturnType.isResolved)
        assert(methods.get(0).getReturnType.getApexName == "Bar")

        val parameters = methods.get(0).getParameters
        assert(parameters.size() == 1)
        assert(parameters.get(0).getName == "a")
        assert(parameters.get(0).getModifier == "")
        assert(parameters.get(0).getArgumentTypeId.isResolved)
        assert(parameters.get(0).getArgumentTypeId.getApexName == "Foo")

        assert(methods.get(0).getDefiningType.getApexName == "Foo")
        val location = methods.get(0).getLocation
        assert(location.getLine == 1)
        assert(location.getColumn == 19)
        assert(location.getStartIndex == 18)
        assert(location.getEndIndex == 48)
    }
  }

  test("Local type return method") {
    FileSystemHelper.run(
      Map(
        "Foo.cls" -> "public class Foo {Bar func(Bar a, String b) {return null;}}",
        "Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "func")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "Bar")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 2)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "Bar")
      assert(parameters.get(1).getName == "b")
      assert(parameters.get(1).getModifier == "")
      assert(parameters.get(1).getArgumentTypeId.isResolved)
      assert(parameters.get(1).getArgumentTypeId.getApexName == "System.String")

      assert(methods.get(0).getDefiningType.getApexName == "Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 58)
    }
  }

  test("Multiple methods") {
    FileSystemHelper.run(
      Map(
        "Foo.cls" -> "public class Foo {void func1(final String a) {} String func2() {return null;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 2)
      assert(methods.get(0).getMethodName == "func1")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(!methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "void")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 1)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "final")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")
      assert(methods.get(1).getParameters.size() == 0)

      assert(methods.get(0).getDefiningType.getApexName == "Foo")
      assert(methods.get(1).getDefiningType.getApexName == "Foo")

      val location1 = methods.get(0).getLocation
      assert(location1.getLine == 1)
      assert(location1.getColumn == 19)
      assert(location1.getStartIndex == 18)
      assert(location1.getEndIndex == 47)

      val location2 = methods.get(1).getLocation
      assert(location2.getLine == 1)
      assert(location2.getColumn == 49)
      assert(location2.getStartIndex == 48)
      assert(location2.getEndIndex == 77)

    }
  }

  test("Basic method (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {String func() {return null;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "func")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "System.String")
      assert(methods.get(0).getParameters.size() == 0)

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 46)
    }
  }

  test("Self referencing return method (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {public static Foo func(String a) {return null;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "func")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "public static")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "ns.Foo")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 1)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 65)
    }
  }

  test("Unknown type return method (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {Bar func(Foo a) {return null;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "func")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(!methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "Bar")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 1)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "ns.Foo")

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 48)
    }
  }

  test("Local type return method (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {Bar func(Bar a, String b) {return null;}}",
        "sources/Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "func")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "ns.Bar")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 2)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "ns.Bar")
      assert(parameters.get(1).getName == "b")
      assert(parameters.get(1).getModifier == "")
      assert(parameters.get(1).getArgumentTypeId.isResolved)
      assert(parameters.get(1).getArgumentTypeId.getApexName == "System.String")

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 58)
    }
  }

  test("Multiple methods (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls" -> "public class Foo {void func1(final String a) {} String func2() {return null;}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 2)
      assert(methods.get(0).getMethodName == "func1")
      assert(!methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(!methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "void")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 1)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "final")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")
      assert(methods.get(1).getParameters.size() == 0)

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      assert(methods.get(1).getDefiningType.getApexName == "ns.Foo")

      val location1 = methods.get(0).getLocation
      assert(location1.getLine == 1)
      assert(location1.getColumn == 19)
      assert(location1.getStartIndex == 18)
      assert(location1.getEndIndex == 47)

      val location2 = methods.get(1).getLocation
      assert(location2.getLine == 1)
      assert(location2.getColumn == 49)
      assert(location2.getStartIndex == 48)
      assert(location2.getEndIndex == 77)
    }
  }
}
