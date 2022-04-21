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

import com.financialforce.oparser.Modifiers.{PUBLIC_MODIFIER, VIRTUAL_MODIFIER}
import com.financialforce.oparser.StringUtils.{
  asConstructorSignatureString,
  asMethodSignatureString
}
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

  test("Extending class") {
    val typeRef = UnresolvedTypeRef("ConnectApi.FeedItem").toOption.get
    val td      = PlatformTypeDeclaration.get(null, typeRef)

    assert(td.nonEmpty)
    assert(td.get.id.toString == "FeedItem")
    assert(td.get.getFullName == "ConnectApi.FeedItem")
    assert(td.get.extendsTypeRef.isInstanceOf[PlatformTypeDeclaration])
    assert(td.get.extendsTypeRef.getFullName == "ConnectApi.FeedElement")
    assert(td.get.implementsTypeList == null)
    assert(td.get.nature == CLASS_NATURE)
    assert(td.get.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER))
    assert(td.get.enclosing.isEmpty)
    assert(td.get.innerTypes.isEmpty)
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
  test("Field access") {
    val typeRef = UnresolvedTypeRef("System.Address").toOption.get

    val td = PlatformTypeDeclaration
      .get(null, typeRef)

    assert(td.nonEmpty)
    assert(td.get.id.toString == "Address")
    assert(td.get.getFullName == "System.Address")
    assert(td.get.extendsTypeRef == null)
    assert(td.get.implementsTypeList == null)
    assert(td.get.nature == CLASS_NATURE)
    assert(td.get.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER))
    assert(td.get.enclosing.isEmpty)
    assert(td.get.innerTypes.isEmpty)

    val fields = td.get.fields.sortBy(_.id.toString)
    assert(fields.length == 8)
    assert(
      fields.map(_.id.toString) sameElements
        Array(
          "city",
          "country",
          "countryCode",
          "geocodeAccuracy",
          "postalCode",
          "state",
          "stateCode",
          "street"
        )
    )
    assert(fields.filter(_.modifiers == ArraySeq(PUBLIC_MODIFIER)) sameElements fields)
    assert(fields.map(_.typeRef.isInstanceOf[PlatformTypeDeclaration]).forall(b => b))
    assert(fields.filter(_.typeRef.getFullName == "System.String") sameElements fields)
  }

  test("Constructor access") {
    val typeRef = UnresolvedTypeRef("System.DmlException").toOption.get
    val td = PlatformTypeDeclaration
      .get(null, typeRef)

    assert(td.nonEmpty)
    assert(td.get.id.toString == "DmlException")
    assert(td.get.getFullName == "System.DmlException")
    assert(td.get.extendsTypeRef.getFullName == "System.Exception")
    assert(td.get.implementsTypeList == null)
    assert(td.get.nature == CLASS_NATURE)
    assert(td.get.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER))
    assert(td.get.enclosing.isEmpty)
    assert(td.get.innerTypes.isEmpty)

    val constructors = td.get.constructors.sortBy(_.toString)
    assert(constructors.length == 4)
    assert(
      constructors
        .filter(_.modifiers == ArraySeq(PUBLIC_MODIFIER)) sameElements constructors
    )
    assert(asConstructorSignatureString(constructors.head) == "public System.DmlException()")
    assert(
      asConstructorSignatureString(
        constructors(1)
      ) == "public System.DmlException(System.Exception param1)"
    )
    assert(
      asConstructorSignatureString(
        constructors(2)
      ) == "public System.DmlException(System.String param1)"
    )
    assert(
      asConstructorSignatureString(
        constructors(3)
      ) == "public System.DmlException(System.String param1, System.Exception param2)"
    )
  }

  test("Method access") {
    val typeRef = UnresolvedTypeRef("System.Address").toOption.get
    val td      = PlatformTypeDeclaration.get(null, typeRef)

    assert(td.nonEmpty)
    assert(td.get.id.toString == "Address")
    assert(td.get.getFullName == "System.Address")
    assert(td.get.extendsTypeRef == null)
    assert(td.get.implementsTypeList == null)
    assert(td.get.nature == CLASS_NATURE)
    assert(td.get.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER))
    assert(td.get.enclosing.isEmpty)
    assert(td.get.innerTypes.isEmpty)

    val methods = td.get.methods.sortBy(_.id.toString)
    assert(methods.length == 11)
    assert(
      methods.map(_.id.toString) sameElements
        Array(
          "getCity",
          "getCountry",
          "getCountryCode",
          "getDistance",
          "getGeocodeAccuracy",
          "getLatitude",
          "getLongitude",
          "getPostalCode",
          "getState",
          "getStateCode",
          "getStreet"
        )
    )
    assert(
      methods
        .filter(_.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER)) sameElements methods
    )
    assert(
      asMethodSignatureString(
        methods
          .filter(_.id.toString == "getCity")
          .head
      )
        == "public virtual System.String getCity()"
    )
    assert(
      asMethodSignatureString(methods.filter(_.id.toString == "getDistance").head) ==
        "public virtual System.Double getDistance(System.Location other, System.String unit)"
    )
  }

  test("Exception") {
    val typeRef = UnresolvedTypeRef("eventbus.RetryableException").toOption.get
    val td = PlatformTypeDeclaration
      .get(null, typeRef)

    assert(td.nonEmpty)
    assert(td.get.id.toString == "RetryableException")
    assert(td.get.getFullName == "eventbus.RetryableException")
    assert(td.get.extendsTypeRef.getFullName == "System.Exception")
    assert(td.get.implementsTypeList == null)
    assert(td.get.nature == CLASS_NATURE)
    assert(td.get.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER))
    assert(td.get.enclosing.isEmpty)
    assert(td.get.innerTypes.isEmpty)

    val methods = td.get.methods.sortBy(_.id.toString)
    assert(methods.length == 15)
    assert(
      methods.map(_.id.toString) sameElements Array(
        "getCause",
        "getDmlFieldNames",
        "getDmlFields",
        "getDmlId",
        "getDmlIndex",
        "getDmlMessage",
        "getDmlStatusCode",
        "getDmlType",
        "getLineNumber",
        "getMessage",
        "getNumDml",
        "getStackTraceString",
        "getTypeName",
        "initCause",
        "setMessage"
      )
    )
    assert(
      methods
        .filter(_.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER)) sameElements methods
    )
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
    val td      = PlatformTypeDeclaration.get(null, typeRef)

    assert(td.nonEmpty)
    assert(td.get.id.toString == "List")
    assert(td.get.getFullName == "System.List<System.String>")
    assert(td.get.extendsTypeRef == null)
    assert(td.get.implementsTypeList.typeRefs.length == 1)
    assert(td.get.implementsTypeList.typeRefs.head.isInstanceOf[PlatformTypeDeclaration])
    assert(td.get.implementsTypeList.typeRefs.head.getFullName == "System.Iterable<System.String>")
    assert(td.get.nature == CLASS_NATURE)
    assert(td.get.modifiers == ArraySeq(PUBLIC_MODIFIER, VIRTUAL_MODIFIER))
    assert(td.get.enclosing.isEmpty)
    assert(td.get.innerTypes.isEmpty)

    assert(
      td.get.methods.map(asMethodSignatureString).sorted.mkString("\n") == Seq(
        "public virtual System.List<System.String> clone()",
        "public virtual void add(System.String listElement)",
        "public virtual void add(System.Integer index, System.String listElement)",
        "public virtual void addAll(System.List<System.String> fromList)",
        "public virtual void addAll(System.Set<System.String> fromSet)",
        "public virtual void clear()",
        "public virtual System.Boolean contains(System.String listElement)",
        "public virtual System.List<System.String> deepClone()",
        "public virtual System.List<System.String> deepClone(System.Boolean preserveId)",
        "public virtual System.List<System.String> deepClone(System.Boolean preserveId, System.Boolean preserveReadonlyTimestamps)",
        "public virtual System.List<System.String> deepClone(System.Boolean preserveId, System.Boolean preserveReadonlyTimestamps, System.Boolean preserveAutonumber)",
        "public virtual System.String get(System.Integer index)",
        "public virtual Schema.SObjectType getSObjectType()",
        "public virtual System.Integer indexOf(System.String listElement)",
        "public virtual System.Iterator<System.String> iterator()",
        "public virtual System.Boolean isEmpty()",
        "public virtual System.String remove(System.Integer index)",
        "public virtual void set(System.Integer index, System.String listElement)",
        "public virtual System.Integer size()",
        "public virtual void sort()",
        "public virtual System.String toString()",
        "public virtual System.Boolean equals(System.List<System.String> other)",
        "public virtual System.Integer hashCode()"
      ).sorted.mkString("\n")
    )
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
