/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ExtendsTest extends AnyFunSuite with TestHelper {

  test("Duplicate inner type names") {
    assert(
      typeDeclarations(
        Map("Dummy.cls" -> "global class Dummy {class Inner {} interface Inner{}}")
      ).nonEmpty
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 35-52: Duplicate type name 'Inner'\n"
    )
  }

  test("Duplicate outer & inner type names") {
    assert(typeDeclarations(Map("Dummy.cls" -> "global class Dummy {class Dummy{}}")).nonEmpty)
    assert(
      dummyIssues ==
        "Error: line 1 at 20-33: Duplicate type name 'Dummy'\n"
    )
  }

  test("Missing superclass") {
    assert(typeDeclarations(Map("Dummy.cls" -> "global class Dummy extends Foo {}")).nonEmpty)
    assert(
      dummyIssues ==
        "Missing: line 1 at 13-18: No type declaration found for 'Foo'\n"
    )
  }

  test("Private superclass") {
    assert(
      typeDeclarations(
        Map(
          "Outer.cls" -> "global class Outer { private virtual class SuperClass {}}",
          "Dummy.cls" -> "global class Dummy extends Outer.SuperClass {}"
        )
      ).size == 2
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 13-18: Parent class 'Outer.SuperClass' is private, it must be public or global\n"
    )
  }

  test("Private superclass (from test)") {
    assert(
      typeDeclarations(
        Map(
          "Outer.cls" -> "global class Outer { private virtual class SuperClass {}}",
          "Dummy.cls" -> "@isTest public class Dummy {class InTest extends Outer.SuperClass {}}"
        )
      ).size == 2
    )
    assert(dummyIssues.isEmpty)
  }

  test("Private superclass (peer)") {
    assert(
      typeDeclarations(
        Map(
          "Dummy.cls" -> "global class Dummy {private virtual class SuperClass {} class Inner extends SuperClass {}}"
        )
      ).nonEmpty
    )
    assert(dummyIssues.isEmpty)
  }

  test("Non-virtual superclass") {
    assert(
      typeDeclarations(
        Map(
          "SuperClass.cls" -> "global class SuperClass {}",
          "Dummy.cls"      -> "global class Dummy extends SuperClass {}"
        )
      ).size == 2
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 13-18: Parent class 'SuperClass' must be declared virtual or abstract\n"
    )
  }

  test("Interface superclass") {
    assert(
      typeDeclarations(
        Map(
          "SuperInterface.cls" -> "global interface SuperInterface {}",
          "Dummy.cls"          -> "global class Dummy extends SuperInterface {}"
        )
      ).size == 2
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 13-18: Parent type 'SuperInterface' must be a class\n"
    )
  }

  test("Enum superclass") {
    assert(
      typeDeclarations(
        Map(
          "SuperEnum.cls" -> "global enum SuperEnum {}",
          "Dummy.cls"     -> "global class Dummy extends SuperEnum {}"
        )
      ).size == 2
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 13-18: Parent type 'SuperEnum' must be a class\n"
    )
  }

  test("External superclass") {
    assert(
      typeDeclarations(
        Map(
          "SuperClass.cls" -> "global virtual class SuperClass {}",
          "Dummy.cls"      -> "global class Dummy extends SuperClass {}"
        )
      ).size == 2
    )
    assert(!hasIssues)
  }

  test("Inner superclass") {
    assert(
      typeDeclarations(
        Map("Dummy.cls" -> "global class Dummy extends Inner {virtual class Inner{}}")
      ).nonEmpty
    )
    assert(!hasIssues)
  }

  test("Exception superclass with wrong name") {
    assert(typeDeclarations(Map("Dummy.cls" -> "public class Dummy extends Exception {}")).nonEmpty)
    assert(
      dummyIssues == "Error: line 1 at 13-18: Class 'Dummy' extending an Exception must have a name ending in Exception\n"
    )
  }

  test("Exception class with no Exception superclass") {
    assert(typeDeclarations(Map("DummyException.cls" -> "public class DummyException{}")).nonEmpty)
    assert(
      getMessages(
        root.join("DummyException.cls")
      ) == "Error: line 1 at 13-27: Exception class 'DummyException' must extend another Exception class\n"
    )
  }

  test("Exception class with no custom exception superclass") {
    assert(
      typeDeclarations(
        Map(
          "BaseException.cls"  -> "public virtual class BaseException extends Exception {}",
          "DummyException.cls" -> "public class DummyException extends BaseException {}"
        )
      ).nonEmpty
    )
    assert(!hasIssues)
  }

  test("Outer superclass") {
    assert(
      typeDeclarations(
        Map("Dummy.cls" -> "global virtual class Dummy {class Inner extends Dummy {}}")
      ).nonEmpty
    )
    assert(!hasIssues)
  }
}
