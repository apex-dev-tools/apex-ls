/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.collection.immutable.ArraySeq

// All types conform to this trait, can be used as a resolved TypeRef
trait ITypeDeclaration extends TypeRef {
  def paths: Array[String]
  def location: Location

  def id: LocatableId

  def typeNameSegment: TypeNameSegment

  def enclosing: Option[ITypeDeclaration]
  def extendsTypeRef: TypeRef
  def implementsTypeList: TypeList
  def modifiers: Array[Modifier]
  def annotations: Array[Annotation]

  def initializers: ArraySeq[Initializer]
  def innerTypes: ArraySeq[ITypeDeclaration]
  def constructors: ArraySeq[ConstructorDeclaration]
  def methods: ArraySeq[MethodDeclaration]
  def properties: ArraySeq[PropertyDeclaration]
  def fields: ArraySeq[FieldDeclaration]

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

  def typeName: Array[TypeNameSegment] = {
    enclosing match {
      case Some(enc) => Array(enc.typeNameSegment, typeNameSegment)
      case None      => Array(typeNameSegment)
    }
  }

  override def getFullName: String = {
    enclosing match {
      case Some(enc) => s"${enc.id.toString}.${typeNameSegment.toString}"
      case None      => typeNameSegment.toString
    }
  }
}

// A mutable variant of ITypeDeclaration to support incremental construction
trait IMutableTypeDeclaration extends ITypeDeclaration with MutableTypeAppendable {

  // Specialise the type of inner types
  override def innerTypes: ArraySeq[IMutableTypeDeclaration]

  // Setters for standard attributes of the type
  def setId(id: LocatableId): Unit
  def setLocation(location: Location): Unit
  def setExtends(typeRef: TypeRef): Unit
  def setImplements(typeList: TypeList): Unit
  def setModifiers(modifiers: Array[Modifier]): Unit
  def setAnnotations(annotations: Array[Annotation]): Unit

  // Appends for incremental construction
  def appendInitializer(init: Initializer): Unit
  def appendInnerType(inner: IMutableTypeDeclaration): Unit
  def appendConstructor(ctor: ConstructorDeclaration): Unit
  def appendMethod(method: MethodDeclaration): Unit
  def appendProperty(prop: PropertyDeclaration): Unit
  def appendField(field: FieldDeclaration): Unit

  // Called after construction is complete
  def onComplete(): Unit
}

// Marker interface for appendable elements such as constructors and fields
trait MutableTypeAppendable
