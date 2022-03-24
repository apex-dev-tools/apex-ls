/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.runtime.types.platform

import com.financialforce.oparser._
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration.{
  createTypeName,
  emptyPaths,
  emptyTypeDeclarations
}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/* Wrapper for the few generic types we support, this specialises the methods of the type so that
 * List<T> presents as say a List<Foo>.
 */
class GenericPlatformTypeDeclaration(
  typeArgs: ArraySeq[IModuleTypeDeclaration],
  genericDecl: PlatformTypeDeclaration
) extends PlatformTypeDeclaration(genericDecl.native, genericDecl.enclosing) {
  assert(genericDecl.typeInfo.args.length == typeArgs.length)

  final private val paramsMap: Map[Name, IModuleTypeDeclaration] = {
    genericDecl.typeInfo.args
      .zip(typeArgs)
      .map(p => (Name(p._1), p._2))
      .toMap
  }

  override def module: Option[IPM.Module] = None

  override val paths: Array[String] = emptyPaths

  override val location: Location = Location.default

  override val id: Id = typeInfo.typeName.id

  override val typeNameSegment: TypeNameSegment = createTypeName(genericDecl.id.toString, typeArgs)

  override def extendsTypeRef: TypeRef = null // TODO

  override def implementsTypeList: TypeList = null // TODO

  override def modifiers: ArraySeq[Modifier] = ArraySeq.empty // TODO

  override def annotations: ArraySeq[Annotation] = ArraySeq.empty // TODO

  override def initializers: ArraySeq[Initializer] = ArraySeq.empty // TODO

  override def innerTypes: ArraySeq[ITypeDeclaration] = emptyTypeDeclarations

  override def constructors: ArraySeq[ConstructorDeclaration] = ArraySeq.empty // TODO

  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.empty // TODO

  override def properties: ArraySeq[PropertyDeclaration] = ArraySeq.empty // TODO

  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.empty // TODO
}

object GenericPlatformTypeDeclaration {

  /* Cache of constructed generic platform types */
  private val declarationCache = mutable.Map[TypeName, GenericPlatformTypeDeclaration]()

  def get(
    typeName: TypeName,
    typeArgs: ArraySeq[IModuleTypeDeclaration],
    td: PlatformTypeDeclaration
  ): GenericPlatformTypeDeclaration = {
    declarationCache.getOrElseUpdate(
      typeName, {
        new GenericPlatformTypeDeclaration(typeArgs, td)
      }
    )
  }

}
