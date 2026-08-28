/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import org.scalatest.funsuite.AnyFunSuite

class MethodMapOrderTest extends AnyFunSuite with TestHelper {

  private def overloadParameterTypes(map: MethodMap, name: String, arity: Int): Seq[String] = {
    map.methodsByName((Name(name), arity)).map(_.parameterTypes).toSeq
  }

  test("Overload group is ordered by parameter type") {
    val td = typeDeclaration("public class Dummy {void f(Integer a) {} void f(String b) {} }")
    assert(overloadParameterTypes(td.methodMap, "f", 1) == Seq("System.Integer", "System.String"))
  }

  test("Overload group order does not depend on declaration order") {
    val td = typeDeclaration("public class Dummy {void f(String b) {} void f(Integer a) {} }")
    assert(overloadParameterTypes(td.methodMap, "f", 1) == Seq("System.Integer", "System.String"))
  }

  test("Inherited overload group is ordered by parameter type") {
    val tds = typeDeclarations(
      Map(
        "Base.cls"  -> "public virtual class Base {public void f(Integer a) {} }",
        "Dummy.cls" -> "public class Dummy extends Base {public void f(String b) {} }"
      )
    )
    val dummy = tds.find(_.name == Name("Dummy")).get
    assert(
      overloadParameterTypes(dummy.methodMap, "f", 1) == Seq("System.Integer", "System.String")
    )
  }

  test("Platform overload group is ordered by parameter type") {
    // java.lang.Class.getMethods does not give a stable order, so without an explicit ordering the
    // members of this group vary between runs, see #553
    val setOfId =
      TypeName(Names.Set$, Seq(TypeNames.IdType), Some(TypeNames.System))
    val td = PlatformTypes.get(setOfId, None).getOrElse(fail(s"Could not load $setOfId"))
    assert(
      overloadParameterTypes(MethodMap(td), "removeAll", 1) ==
        Seq("System.List<System.Id>", "System.Set<System.Id>")
    )
  }
}
