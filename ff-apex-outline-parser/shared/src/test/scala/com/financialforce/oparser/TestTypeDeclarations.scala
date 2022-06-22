/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import com.financialforce.types._
import com.financialforce.types.base.{
  Annotation,
  IdWithLocation,
  Location,
  Modifier,
  TypeNameSegment,
  TypeRef
}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait ITestTypeDeclaration extends ITypeDeclaration {

  override def enclosing: Option[ITestTypeDeclaration]
  override def innerTypes: ArraySeq[ITestTypeDeclaration]

  override def equals(that: Any): Boolean = {
    that match {
      case other: ITestTypeDeclaration => other.eq(this)
      case _                           => false
    }
  }

  // Identity hash, may not be unique, storing as val does not appear to improve performance
  override def hashCode(): Int = System.identityHashCode(this)
}

trait IMutableTestTypeDeclaration extends IMutableTypeDeclaration with ITestTypeDeclaration {
  override def enclosing: Option[IMutableTestTypeDeclaration]
  override def innerTypes: ArraySeq[IMutableTestTypeDeclaration]
}

sealed class TestTypeDeclaration(
  val path: String,
  val nature: TypeNature,
  _enclosing: IMutableTestTypeDeclaration
) extends IMutableTestTypeDeclaration {
  var _location: Location = _

  var _id: IdWithLocation                    = _
  var _extendsTypeRef: TypeRef               = _
  var _implementsTypeList: ArraySeq[TypeRef] = _
  var _modifiers: Array[Modifier]            = Modifier.emptyArray
  var _annotations: Array[Annotation]        = Annotation.emptyArray

  val _initializers: mutable.ArrayBuffer[Initializer]            = mutable.ArrayBuffer()
  val _innerTypes: mutable.ArrayBuffer[TestTypeDeclaration]      = mutable.ArrayBuffer()
  val _constructors: mutable.ArrayBuffer[ConstructorDeclaration] = mutable.ArrayBuffer()
  val _methods: mutable.ArrayBuffer[MethodDeclaration]           = mutable.ArrayBuffer()
  val _properties: mutable.ArrayBuffer[PropertyDeclaration]      = mutable.ArrayBuffer()
  val _fields: mutable.ArrayBuffer[FieldDeclaration]             = mutable.ArrayBuffer()

  override def paths: Array[String] = Array(path)
  override def location: Location   = _location

  override def id: IdWithLocation = _id

  override def typeNameSegment: TypeNameSegment = new TypeNameSegment(id, TypeRef.emptyArraySeq)

  override def enclosing: Option[IMutableTestTypeDeclaration] = Option(_enclosing)
  override def extendsTypeRef: TypeRef                        = _extendsTypeRef
  override def implementsTypeList: ArraySeq[TypeRef]          = _implementsTypeList

  override def modifiers: Array[Modifier]          = _modifiers
  override def annotations: Array[Annotation]      = _annotations
  override def initializers: ArraySeq[Initializer] = ArraySeq.unsafeWrapArray(_initializers.toArray)

  override def innerTypes: ArraySeq[TestTypeDeclaration] =
    ArraySeq.unsafeWrapArray(_innerTypes.toArray)
  override def constructors: ArraySeq[ConstructorDeclaration] =
    ArraySeq.unsafeWrapArray(_constructors.toArray)
  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.unsafeWrapArray(_methods.toArray)
  override def properties: ArraySeq[PropertyDeclaration] =
    ArraySeq.unsafeWrapArray(_properties.toArray)
  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.unsafeWrapArray(_fields.toArray)

  override def setId(id: IdWithLocation): Unit                      = _id = id
  override def setLocation(location: Location): Unit                = _location = location
  override def setExtends(typeRef: TypeRef): Unit                   = _extendsTypeRef = typeRef
  override def setImplements(typeList: ArraySeq[TypeRef]): Unit     = _implementsTypeList = typeList
  override def setModifiers(modifiers: Array[Modifier]): Unit       = _modifiers = modifiers
  override def setAnnotations(annotations: Array[Annotation]): Unit = _annotations = annotations

  override def appendInitializer(init: Initializer): Unit = _initializers.append(init)
  override def appendInnerType(inner: IMutableTypeDeclaration): Unit = {
    // This is rather messy, we need to accept IMutableTypeDeclaration for the caller(s) but only want to
    // expose as TypeDeclaration, it should not fail at run time, and maybe is fixable via some generics magic
    inner match { case td: TestTypeDeclaration => _innerTypes.append(td) }
  }
  override def appendConstructor(ctor: ConstructorDeclaration): Unit = _constructors.append(ctor)
  override def appendProperty(prop: PropertyDeclaration): Unit       = _properties.append(prop)
  override def appendField(field: FieldDeclaration): Unit            = _fields.append(field)
  override def appendMethod(md: MethodDeclaration): Unit             = _methods.append(md)

  override def onComplete(): Unit = { /* Not needed, appending is immediate. */ }
}

object TestTypeDeclaration {
  final val emptyArrayBuffer = mutable.ArrayBuffer[TestTypeDeclaration]()
}

class TestClassTypeDeclaration(path: String, enclosing: IMutableTestTypeDeclaration)
    extends TestTypeDeclaration(path, CLASS_NATURE, enclosing) {

  override def toString: String = {
    val base =
      s"""Class:      $id
         |Path:       $path
         |Location:   ${id.location}
         |Annotation: ${_annotations.mkString(" ")}
         |Modifiers:  ${_modifiers.mkString(" ")}
         |Extends:    ${_extendsTypeRef}
         |Implements: ${_implementsTypeList}
         |""".stripMargin

    val c =
      if (_constructors.isEmpty) ""
      else
        s"""
           |Constructors:
           |${_constructors.mkString("\n")}
           |""".stripMargin

    val m =
      if (_methods.isEmpty) ""
      else
        s"""
           |Methods:
           |${_methods.mkString("\n")}
           |""".stripMargin

    val p =
      if (_properties.isEmpty) ""
      else
        s"""
           |Properties:
           |${_properties.mkString("\n")}
           |""".stripMargin

    val f =
      if (_fields.isEmpty) ""
      else
        s"""
           |Fields:
           |${_fields.mkString("\n")}
           |""".stripMargin

    val i =
      if (innerTypes.isEmpty) ""
      else
        s"""
           |Inner types:
           |${innerTypes.mkString("\n")}
           |""".stripMargin

    base + c + m + p + f + i
  }
}

class TestInterfaceTypeDeclaration(path: String, enclosing: IMutableTestTypeDeclaration)
    extends TestTypeDeclaration(path, INTERFACE_NATURE, enclosing) {

  override def toString: String = {
    s"""Interface:  $id
       |Path:       $path
       |Location:   ${id.location}
       |Annotation: ${_annotations.mkString(" ")}
       |Modifiers:  ${_modifiers.mkString(" ")}
       |Implements: ${_implementsTypeList}
       |Methods:
       |${methods.mkString("\n")}
       |
       |""".stripMargin
  }
}

class TestEnumTypeDeclaration(path: String, enclosing: IMutableTestTypeDeclaration)
    extends TestTypeDeclaration(path, ENUM_NATURE, enclosing) {

  override def toString: String = {
    s"""Enum:       $id
       |Path:       $path
       |Location:   ${id.location}
       |Annotation: ${_annotations.mkString(" ")}
       |Modifiers:  ${_modifiers.mkString(" ")}
       |Constants:
       |${fields.map(f => s"${f.id.location} ${f.id.name}").mkString("\n")}
       |
       |""".stripMargin
  }
}

object TestClassFactory extends TypeDeclFactory[IMutableTestTypeDeclaration, String] {
  override def create(
    ctx: String,
    nature: TypeNature,
    path: String,
    enclosing: Option[IMutableTestTypeDeclaration]
  ): IMutableTestTypeDeclaration = {
    nature match {
      case CLASS_NATURE     => new TestClassTypeDeclaration(path, enclosing.orNull)
      case INTERFACE_NATURE => new TestInterfaceTypeDeclaration(path, enclosing.orNull)
      case ENUM_NATURE      => new TestEnumTypeDeclaration(path, enclosing.orNull)
    }
  }
}
