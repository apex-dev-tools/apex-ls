/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.api

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MDIndexConstructorTest extends AnyFunSuite {

  test("Basic constructor") {
    FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {Foo() {}}")) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "Foo")
      assert(methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "Foo")
      assert(methods.get(0).getParameters.size() == 0)

      assert(methods.get(0).getDefiningType.getApexName == "Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 26)
    }
  }

  test("Multiple args constructor") {
    FileSystemHelper.run(
      Map(
        "Foo.cls" -> "public class Foo {private Foo(String a, Bar b) {}}",
        "Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "Foo")
      assert(methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "private")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "Foo")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 2)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")
      assert(parameters.get(1).getName == "b")
      assert(parameters.get(1).getModifier == "")
      assert(parameters.get(1).getArgumentTypeId.isResolved)
      assert(parameters.get(1).getArgumentTypeId.getApexName == "Bar")

      assert(methods.get(0).getDefiningType.getApexName == "Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 49)
    }
  }

  test("Multiple constructors") {
    FileSystemHelper.run(
      Map(
        "Foo.cls" -> "public class Foo {Foo() {} private Foo(String a, Bar b) {}}",
        "Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 2)
      assert(methods.get(0).getMethodName == "Foo")
      assert(methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "Foo")
      assert(methods.get(0).getParameters.size() == 0)
      assert(methods.get(1).getMethodName == "Foo")
      assert(methods.get(1).isConstructor)
      assert(methods.get(1).getModifiers == "private")
      assert(methods.get(1).getReturnType.isResolved)
      assert(methods.get(1).getReturnType.getApexName == "Foo")
      assert(methods.get(1).getParameters.size() == 2)

      val parameters = methods.get(1).getParameters
      assert(parameters.size() == 2)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")
      assert(parameters.get(1).getName == "b")
      assert(parameters.get(1).getModifier == "")
      assert(parameters.get(1).getArgumentTypeId.isResolved)
      assert(parameters.get(1).getArgumentTypeId.getApexName == "Bar")

      assert(methods.get(0).getDefiningType.getApexName == "Foo")
      val location1 = methods.get(0).getLocation
      assert(location1.getLine == 1)
      assert(location1.getColumn == 19)
      assert(location1.getStartIndex == 18)
      assert(location1.getEndIndex == 26)

      val location2 = methods.get(1).getLocation
      assert(location2.getLine == 1)
      assert(location2.getColumn == 28)
      assert(location2.getStartIndex == 27)
      assert(location2.getEndIndex == 58)
    }
  }

  test("Basic constructor (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo {Foo() {}}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "Foo")
      assert(methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "ns.Foo")
      assert(methods.get(0).getParameters.size() == 0)

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 26)
    }
  }

  test("Multiple args constructor (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo {private Foo(String a, Bar b) {}}",
        "sources/Bar.cls"   -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 1)
      assert(methods.get(0).getMethodName == "Foo")
      assert(methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "private")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "ns.Foo")

      val parameters = methods.get(0).getParameters
      assert(parameters.size() == 2)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")
      assert(parameters.get(1).getName == "b")
      assert(parameters.get(1).getModifier == "")
      assert(parameters.get(1).getArgumentTypeId.isResolved)
      assert(parameters.get(1).getArgumentTypeId.getApexName == "ns.Bar")

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location = methods.get(0).getLocation
      assert(location.getLine == 1)
      assert(location.getColumn == 19)
      assert(location.getStartIndex == 18)
      assert(location.getEndIndex == 49)
    }
  }

  test("Multiple constructors (with ns)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{ \"packageDirectories\": [{\"path\": \"sources\"}], \"namespace\": \"ns\"}",
        "sources/Foo.cls"   -> "public class Foo {Foo() {} private Foo(String a, Bar b) {}}",
        "sources/Bar.cls"   -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = new MDIndex(root)
      assert(index.hasUpdatedIssues.isEmpty)

      val fooType = index.findExactTypeId("ns.Foo")
      val methods = fooType.getMethods

      assert(methods.size() == 2)
      assert(methods.get(0).getMethodName == "Foo")
      assert(methods.get(0).isConstructor)
      assert(methods.get(0).getModifiers == "")
      assert(methods.get(0).getReturnType.isResolved)
      assert(methods.get(0).getReturnType.getApexName == "ns.Foo")
      assert(methods.get(0).getParameters.size() == 0)
      assert(methods.get(1).getMethodName == "Foo")
      assert(methods.get(1).isConstructor)
      assert(methods.get(1).getModifiers == "private")
      assert(methods.get(1).getReturnType.isResolved)
      assert(methods.get(1).getReturnType.getApexName == "ns.Foo")
      assert(methods.get(1).getParameters.size() == 2)

      val parameters = methods.get(1).getParameters
      assert(parameters.size() == 2)
      assert(parameters.get(0).getName == "a")
      assert(parameters.get(0).getModifier == "")
      assert(parameters.get(0).getArgumentTypeId.isResolved)
      assert(parameters.get(0).getArgumentTypeId.getApexName == "System.String")
      assert(parameters.get(1).getName == "b")
      assert(parameters.get(1).getModifier == "")
      assert(parameters.get(1).getArgumentTypeId.isResolved)
      assert(parameters.get(1).getArgumentTypeId.getApexName == "ns.Bar")

      assert(methods.get(0).getDefiningType.getApexName == "ns.Foo")
      val location1 = methods.get(0).getLocation
      assert(location1.getLine == 1)
      assert(location1.getColumn == 19)
      assert(location1.getStartIndex == 18)
      assert(location1.getEndIndex == 26)

      val location2 = methods.get(1).getLocation
      assert(location2.getLine == 1)
      assert(location2.getColumn == 28)
      assert(location2.getStartIndex == 27)
      assert(location2.getEndIndex == 58)
    }
  }

}
