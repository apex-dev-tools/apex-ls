/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base._

import scala.collection.immutable.ArraySeq

/** Class, enum, interface type declaration. May be used as a resolved TypeRef. Uses Identity equality. */
trait ITypeDeclaration extends TypeRef with AnnotationsAndModifiers {
  def paths: Array[String]
  def location: Location

  def id: IdWithLocation

  def typeNameSegment: TypeNameSegment

  def enclosing: Option[ITypeDeclaration]
  def extendsTypeRef: TypeRef
  def implementsTypeList: ArraySeq[TypeRef]
  override def modifiers: Array[Modifier]
  override def annotations: Array[Annotation]

  def initializers: ArraySeq[IInitializer]
  def innerTypes: ArraySeq[ITypeDeclaration]
  def constructors: ArraySeq[IConstructorDeclaration]
  def methods: ArraySeq[IMethodDeclaration]
  def properties: ArraySeq[IPropertyDeclaration]
  def fields: ArraySeq[IFieldDeclaration]

  def typeName: Array[TypeNameSegment] = {
    enclosing match {
      case Some(enc) => Array(enc.typeNameSegment, typeNameSegment)
      case None      => Array(typeNameSegment)
    }
  }

  override def fullName: String = {
    enclosing.map(_.fullName + ".").getOrElse("") + typeNameSegment.toString
  }

  override def toString: String = fullName

  override def equals(that: Any): Boolean = {
    that match {
      case other: ITypeDeclaration => other.eq(this)
      case _                       => false
    }
  }

  override def hashCode(): Int = System.identityHashCode(this)
}
