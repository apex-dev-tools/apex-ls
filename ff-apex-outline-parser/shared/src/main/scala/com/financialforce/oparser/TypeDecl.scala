/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.collection.immutable.ArraySeq

trait ITypeDeclaration extends TypeRef {
  def paths: Array[String]
  def location: Location

  def id: Id

  def typeNameSegment: TypeNameSegment

  def enclosing: Option[ITypeDeclaration]
  def extendsTypeRef: TypeRef
  def implementsTypeList: TypeList

  def modifiers: ArraySeq[Modifier]
  def annotations: ArraySeq[Annotation]
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

trait IMutableTypeDeclaration
    extends ITypeDeclaration
    with IdAssignable
    with TypeRefAssignable
    with TypeListAssignable
    with MethodDeclarationAssignable
    with InitializerAssignable {

  def setLocation(location: Location): Unit
  def setExtends(typeRef: TypeRef): Unit
  def setImplements(typeList: TypeList): Unit
  def setModifiers(modifiers: ArraySeq[Modifier]): Unit
  def setAnnotations(annotations: ArraySeq[Annotation]): Unit

  def innerTypes: ArraySeq[IMutableTypeDeclaration]
  def appendInnerType(inner: IMutableTypeDeclaration): Unit

  def appendConstructor(ctor: ConstructorDeclaration): Unit
  def appendProperty(prop: PropertyDeclaration): Unit
  def appendField(prop: FieldDeclaration): Unit
}
