/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import com.financialforce.types.base.{
  Annotation,
  Location,
  Modifier,
  QualifiedName,
  TypeNameSegment,
  TypeRef,
  UnresolvedTypeRef
}

import scala.collection.immutable.ArraySeq

trait DeclarationGeneratorHelper {
  def toTypeRef(
    typeNameWithArguments: Map[String, Option[Array[UnresolvedTypeRef]]],
    totalSubscripts: Int = 0
  ): UnresolvedTypeRef = {
    val typNames: Array[TypeNameSegment] = typeNameWithArguments map {
      case (name, args) => toTypeNames(name, args)
    } to Array
    toTypeRef(typNames, totalSubscripts)
  }

  def toTypeNames(
    typeName: String,
    maybeArguments: Option[Array[UnresolvedTypeRef]]
  ): TypeNameSegment = {
    new TypeNameSegment(
      toId(typeName),
      maybeArguments
        .map(arguments => toTypeArguments(Some(arguments)))
        .getOrElse(TypeRef.emptyArraySeq)
    )
  }

  def toTypeArguments(maybeTypes: Option[Array[UnresolvedTypeRef]]): ArraySeq[TypeRef] = {
    maybeTypes.map(types => toTypeList(types)).getOrElse(TypeRef.emptyArraySeq)
  }

  def toTypeList(types: Array[UnresolvedTypeRef]): ArraySeq[TypeRef] = {
    ArraySeq.unsafeWrapArray(types)
  }

  def toTypeRef(typeNames: Array[TypeNameSegment], totalSubscripts: Int): UnresolvedTypeRef = {
    UnresolvedTypeRef(typeNames, totalSubscripts)
  }

  def toParameter(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: LocatableIdToken
  ): FormalParameter = {
    FormalParameter(annotations, modifiers, typeRef, id)
  }

  def toModifier(m: String): Modifier = {
    Modifier(m)
  }

  def toIdToken(token: String): LocatableIdToken = {
    LocatableIdToken(token, Location.default)
  }

  def toAnnotation(ids: Array[String], parameter: Option[String]): Annotation = {
    Annotation(toQName(ids).toString, parameter)
  }

  def toQName(ids: Array[String]): QualifiedName = {
    QualifiedName(ids.map(x => toId(x)))
  }

  def toId(id: String): LocatableIdToken = {
    toIdToken(id)
  }

  def toParameterList(fps: Array[FormalParameter]): ArraySeq[FormalParameter] = {
    ArraySeq.unsafeWrapArray(fps)
  }

  def toConstructor(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    names: Array[String],
    parameters: ArraySeq[FormalParameter]
  ): ConstructorDeclaration = {
    ConstructorDeclaration(annotation, modifiers, toQName(names), parameters)
  }

  def toMethodDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: LocatableIdToken,
    parameters: ArraySeq[FormalParameter]
  ): MethodDeclaration = {
    MethodDeclaration(annotation, modifiers, Some(typeRef), id, parameters)
  }

  def toPropertyDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: UnresolvedTypeRef,
    id: LocatableIdToken
  ): PropertyDeclaration = {
    PropertyDeclaration(annotation, modifiers, typeRef, Array(), id)
  }

  def toFieldDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: UnresolvedTypeRef,
    id: LocatableIdToken
  ): FieldDeclaration = {
    FieldDeclaration(annotation, modifiers, typeRef, id)
  }
}
