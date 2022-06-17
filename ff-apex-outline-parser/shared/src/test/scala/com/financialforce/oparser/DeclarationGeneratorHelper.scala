/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

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
        .getOrElse(TypeArguments.empty)
    )
  }

  def toTypeArguments(maybeTypes: Option[Array[UnresolvedTypeRef]]): TypeArguments = {
    new TypeArguments(maybeTypes.map(types => toTypeList(types)).getOrElse(TypeList.empty))
  }

  def toTypeList(types: Array[UnresolvedTypeRef]): TypeList = {
    TypeList(ArraySeq.unsafeWrapArray(types))
  }

  def toTypeRef(typeNames: Array[TypeNameSegment], totalSubscripts: Int): UnresolvedTypeRef = {
    val tr = new UnresolvedTypeRef()
    typeNames.foreach(tr.typeNameSegments.append)
    tr.arraySubscripts = totalSubscripts
    tr
  }

  def toParameter(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: IdToken
  ): FormalParameter = {
    FormalParameter(annotations, modifiers, typeRef, id)
  }

  def toModifier(m: String): Modifier = {
    Modifier(m)
  }

  def toIdToken(token: String): IdToken = {
    IdToken(token, Location.default)
  }

  def toAnnotation(ids: Array[String], parameter: Option[String]): Annotation = {
    Annotation(toQName(ids).toString, parameter)
  }

  def toQName(ids: Array[String]): QualifiedName = {
    QualifiedName(ids.map(x => toId(x)))
  }

  def toId(id: String): IdToken = {
    toIdToken(id)
  }

  def toParameterList(fps: Array[FormalParameter]): FormalParameterList = {
    FormalParameterList(ArraySeq.unsafeWrapArray(fps))
  }

  def toConstructor(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    names: Array[String],
    parameters: FormalParameterList
  ): ConstructorDeclaration = {
    ConstructorDeclaration(annotation, modifiers, toQName(names), parameters)
  }

  def toMethodDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: IdToken,
    parameters: FormalParameterList
  ): MethodDeclaration = {
    MethodDeclaration(annotation, modifiers, Some(typeRef), id, parameters)
  }

  def toPropertyDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: UnresolvedTypeRef,
    id: IdToken
  ): PropertyDeclaration = {
    PropertyDeclaration(annotation, modifiers, typeRef, id)
  }

  def toFieldDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: UnresolvedTypeRef,
    id: IdToken
  ): FieldDeclaration = {
    FieldDeclaration(annotation, modifiers, typeRef, id)
  }
}
