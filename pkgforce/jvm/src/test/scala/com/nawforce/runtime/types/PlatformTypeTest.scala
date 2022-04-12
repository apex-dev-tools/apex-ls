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

package com.nawforce.runtime.types

import com.financialforce.oparser.{
  CLASS_NATURE,
  ENUM_NATURE,
  INTERFACE_NATURE,
  TypeArguments,
  TypeNameSegment,
  UnresolvedTypeRef
}
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class PlatformTypeTest extends AnyFunSuite {

  test("Bad class not found") {
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("Foo")), 0)
    assert(PlatformTypeDeclaration.get(null, typeRef).isEmpty)
  }

  test("Unscoped class not found") {
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("String")), 0)
    assert(PlatformTypeDeclaration.get(null, typeRef).isEmpty)
  }

  test("Scoped class found") {
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("String")), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)

    val td = tdOpt.get
    assert(td.nature == CLASS_NATURE)
    assert(td.id.toString == "String")
    assert(td.typeNameSegment.toString == "String")
    assert(td.typeName.mkString(".") == "String")
    assert(td.enclosing.isEmpty)
    assert(td.innerTypes.isEmpty)
  }

  test("Case insensitive class name") {
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("StrIng")), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)
  }

  test("Case insensitive namespace") {
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("SyStem"), TypeNameSegment("StrIng")), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)
  }

  test("Interface found") {
    val typeRef =
      UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("Callable")), 0)
    val tdOpt = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)

    val td = tdOpt.get
    assert(td.nature == INTERFACE_NATURE)
    assert(td.id.toString == "Callable")
    assert(td.typeNameSegment.toString == "Callable")
    assert(td.typeName.mkString(".") == "Callable")
    assert(td.enclosing.isEmpty)
    assert(td.innerTypes.isEmpty)
  }

  test("Enum found") {
    val typeRef =
      UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("RoundingMode")), 0)
    val tdOpt = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)

    val td = tdOpt.get
    assert(td.nature == ENUM_NATURE)
    assert(td.id.toString == "RoundingMode")
    assert(td.typeNameSegment.toString == "RoundingMode")
    assert(td.typeName.mkString(".") == "RoundingMode")
    assert(td.enclosing.isEmpty)
    assert(td.innerTypes.isEmpty)
  }

  test("Inner class found") {
    val typeRef = UnresolvedTypeRef(
      Array(
        TypeNameSegment("Messaging"),
        TypeNameSegment("InboundEmail"),
        TypeNameSegment("BinaryAttachment")
      ),
      0
    )
    val tdOpt = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)
  }

  test("Generic class found") {
    val stringTD = PlatformTypeDeclaration.get(
      null,
      UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("String")), 0)
    )
    assert(stringTD.nonEmpty)

    val listTS = TypeNameSegment("List")
    listTS.typeArguments = Some(TypeArguments(ArraySeq(stringTD.get)))
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), listTS), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.nonEmpty)
  }

  test("Non-Generic class with type arguments") {
    val stringTD = PlatformTypeDeclaration.get(
      null,
      UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("String")), 0)
    )
    assert(stringTD.nonEmpty)

    val integerTS = TypeNameSegment("Integer")
    integerTS.typeArguments = Some(TypeArguments(ArraySeq(stringTD.get)))
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), integerTS), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.isEmpty)
  }

  test("Generic class with too many type arguments") {
    val stringTD = PlatformTypeDeclaration.get(
      null,
      UnresolvedTypeRef(Array(TypeNameSegment("System"), TypeNameSegment("String")), 0)
    )
    assert(stringTD.nonEmpty)

    val listTS = TypeNameSegment("List")
    listTS.typeArguments = Some(TypeArguments(ArraySeq(stringTD.get, stringTD.get)))
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), listTS), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.isEmpty)
  }

  test("Generic class with too few type arguments") {
    val listTS = TypeNameSegment("List")
    listTS.typeArguments = Some(TypeArguments(ArraySeq()))
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), listTS), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.isEmpty)
  }

  test("Generic class with unresolved type argument") {
    val listTS = TypeNameSegment("List")
    listTS.typeArguments = Some(TypeArguments(Array("String")))
    val typeRef = UnresolvedTypeRef(Array(TypeNameSegment("System"), listTS), 0)
    val tdOpt   = PlatformTypeDeclaration.get(null, typeRef)
    assert(tdOpt.isEmpty)
  }
}
