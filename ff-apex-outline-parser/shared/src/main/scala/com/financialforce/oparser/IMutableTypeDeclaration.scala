/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import com.financialforce.types._
import com.financialforce.types.base.{Annotation, IdWithLocation, Location, Modifier, TypeRef}

import scala.collection.immutable.ArraySeq

// A mutable variant of ITypeDeclaration to support incremental construction
trait IMutableTypeDeclaration extends ITypeDeclaration with MutableTypeAppendable {

  // Specialise the type of inner types
  override def innerTypes: ArraySeq[IMutableTypeDeclaration]

  // Setters for standard attributes of the type
  def setId(id: IdWithLocation): Unit
  def setLocation(location: Location): Unit
  def setExtends(typeRef: TypeRef): Unit
  def setImplements(typeList: ArraySeq[TypeRef]): Unit
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
