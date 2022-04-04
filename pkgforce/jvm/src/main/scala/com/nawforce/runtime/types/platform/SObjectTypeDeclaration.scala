package com.nawforce.runtime.types.platform

import com.financialforce.oparser.{
  Annotation,
  ConstructorDeclaration,
  FieldDeclaration,
  ITypeDeclaration,
  Id,
  Initializer,
  Location,
  MethodDeclaration,
  Modifier,
  PropertyDeclaration,
  TypeList,
  TypeNameSegment,
  TypeRef
}
import com.nawforce.pkgforce.documents.MetadataDocument
import com.nawforce.runtime.types.platform.SObjectTypeDeclaration.{emptyPaths, getTypeInfo}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import scala.collection.immutable.ArraySeq

class SObjectTypeDeclaration(_module: IPM.Module, md: MetadataDocument)
    extends IModuleTypeDeclaration {
  module = _module

  final val typeInfo = getTypeInfo(md)

  override def enclosing: Option[ITypeDeclaration] = None

  override val paths: Array[String] = emptyPaths //TODO: potentially use md.path

  override val location: Location = Location.default

  override def id: Id = typeInfo.typeName.id

  override def typeNameSegment: TypeNameSegment = typeInfo.typeName

  override def extendsTypeRef: TypeRef = null // TODO

  override def implementsTypeList: TypeList = null // TODO

  override def modifiers: ArraySeq[Modifier] = ArraySeq.empty // TODO

  override def annotations: ArraySeq[Annotation] = ArraySeq.empty // TODO

  override def initializers: ArraySeq[Initializer] = ArraySeq.empty // TODO

  override def innerTypes: ArraySeq[ITypeDeclaration] = ArraySeq.empty

  override def constructors: ArraySeq[ConstructorDeclaration] = ArraySeq.empty // TODO

  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.empty // TODO

  override def properties: ArraySeq[PropertyDeclaration] = ArraySeq.empty // TODO

  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.empty // TODO

}

object SObjectTypeDeclaration {
  final val emptyPaths: Array[String] = Array.empty
  final val emptyArgs: Array[String]  = Array.empty

  def apply(module: IPM.Module, md: MetadataDocument): SObjectTypeDeclaration = {
    new SObjectTypeDeclaration(module, md)
  }

  def getTypeInfo(md: MetadataDocument): TypeInfo = {
    val typeName = md.typeName(None)
    val ns       = typeName.outer.map(_.name.value).get
    val name     = TypeNameSegment(typeName.name.value)
    TypeInfo(ns, emptyArgs, name)
  }
}