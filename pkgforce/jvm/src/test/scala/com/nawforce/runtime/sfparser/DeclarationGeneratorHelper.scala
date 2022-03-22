package com.nawforce.runtime.sfparser

import com.financialforce.oparser.{Annotation, ArraySubscripts, ConstructorDeclaration, FieldDeclaration, FormalParameter, FormalParameterList, Id, IdToken, Location, MethodDeclaration, Modifier, PropertyDeclaration, QualifiedName, TypeArguments, TypeList, TypeName, TypeRef}

import scala.collection.immutable.ArraySeq

trait DeclarationGeneratorHelper {
  def toTypeRef(
    typeNameWithArguments: Map[String, Option[Array[TypeRef]]],
    totalSubscripts: Int = 0
  ): TypeRef = {
    val typNames: Array[TypeName] = typeNameWithArguments map {
      case (name, args) => toTypeNames(name, args)
    } to Array
    toTypeRef(typNames, totalSubscripts)
  }

  def toTypeNames(typeName: String, maybeArguments: Option[Array[TypeRef]]): TypeName = {
    val tp = new TypeName(toId(typeName))
    maybeArguments match {
      case Some(arguments) => tp.add(toTypeArguments(Some(arguments)))
      case _               =>
    }
    tp
  }

  def toTypeArguments(maybeTypes: Option[Array[TypeRef]]): TypeArguments = {
    val ta = new TypeArguments()
    maybeTypes match {
      case Some(types) => ta.typeList = Some(toTypeList(types))
      case _           =>
    }
    ta
  }

  def toTypeList(types: Array[TypeRef]): TypeList = {
    val tl = new TypeList()
    types.foreach(tl.add)
    tl
  }

  def toTypeRef(typeNames: Array[TypeName], totalSubscripts: Int): TypeRef = {
    val tr = new TypeRef()
    typeNames.foreach(tr.add)
    Array.fill(totalSubscripts)(new ArraySubscripts).foreach(tr.add)
    tr
  }

  def toParameter(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: Option[TypeRef] = None,
    id: Option[Id] = None
  ): FormalParameter = {
    val fp = new FormalParameter()
    modifiers.foreach(fp.add)
    annotations.foreach(fp.add)
    fp.typeRef = typeRef
    fp.id = id
    fp
  }

  def toModifier(m: String): Modifier = {
    Modifier(toIdToken(m))
  }

  def toIdToken(token: String): IdToken = {
    IdToken(token, Location.default)
  }

  def toAnnotation(ids: Array[String], parameter: Option[String]): Annotation = {
    Annotation(toQName(ids), parameter)
  }

  def toQName(ids: Array[String]): QualifiedName = {
    val qName = new QualifiedName()
    ids.foreach(x => qName.add(toId(x)))
    qName
  }

  def toId(id: String): Id = {
    Id(toIdToken(id))
  }

  def toParameterList(fps: Array[FormalParameter]): FormalParameterList = {
    val fpl = new FormalParameterList()
    fps.foreach(fpl.add)
    fpl
  }

  def toConstructor(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    names: Array[String],
    parameters: FormalParameterList
  ): ConstructorDeclaration = {
    ConstructorDeclaration(
      ArraySeq.unsafeWrapArray(annotation),
      ArraySeq.unsafeWrapArray(modifiers),
      toQName(names),
      parameters
    )
  }

  def toMethodDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: Id,
    parameters: FormalParameterList
  ): MethodDeclaration = {
    MethodDeclaration(
      ArraySeq.unsafeWrapArray(annotation),
      ArraySeq.unsafeWrapArray(modifiers),
      typeRef,
      id,
      parameters
    )
  }

  def toPropertyDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: Id
  ): PropertyDeclaration = {
    new PropertyDeclaration(ArraySeq.unsafeWrapArray(annotation), ArraySeq.unsafeWrapArray(modifiers), typeRef, id)
  }

  def toFieldDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: Id
  ): FieldDeclaration = {
    FieldDeclaration(ArraySeq.unsafeWrapArray(annotation), ArraySeq.unsafeWrapArray(modifiers), typeRef, id)
  }
}
