package com.nawforce.runtime.sfparser_test

import com.financialforce.oparser._

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
    typeRef: Option[TypeRef] = None,
    id: Option[Id] = None
  ): FormalParameter = {
    val fp = new FormalParameter()
    fp.setModifiers(ArraySeq.unsafeWrapArray(modifiers))
    fp.setAnnotations(ArraySeq.unsafeWrapArray(annotations))
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
    FormalParameterList(ArraySeq.unsafeWrapArray(fps))
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
    typeRef: UnresolvedTypeRef,
    id: Id
  ): PropertyDeclaration = {
    new PropertyDeclaration(
      ArraySeq.unsafeWrapArray(annotation),
      ArraySeq.unsafeWrapArray(modifiers),
      typeRef,
      id
    )
  }

  def toFieldDeclaration(
    annotation: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: UnresolvedTypeRef,
    id: Id
  ): FieldDeclaration = {
    FieldDeclaration(
      ArraySeq.unsafeWrapArray(annotation),
      ArraySeq.unsafeWrapArray(modifiers),
      typeRef,
      id
    )
  }
}
