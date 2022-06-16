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
import com.nawforce.pkgforce.names.Name
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration.{createTypeName, emptyPaths}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import java.lang.reflect.Method
import scala.collection.immutable.ArraySeq

/* Wrapper for the few generic types we support, this specialises the methods of the type so that
 * List<T> presents as say a List<Foo>.
 */
class GenericPlatformTypeDeclaration(
  _module: IPM.Module,
  typeArgs: ArraySeq[IModuleTypeDeclaration],
  genericDecl: PlatformTypeDeclaration
) extends PlatformTypeDeclaration(_module, genericDecl.native, genericDecl.enclosing) {
  assert(genericDecl.typeInfo.args.length == typeArgs.length)

  final lazy private val paramsMap: Map[Name, IModuleTypeDeclaration] = {
    genericDecl.typeInfo.args
      .zip(typeArgs)
      .map(p => (Name(p._1), p._2))
      .toMap
  }

  // TODO: Set module
  // override def module: Option[IPM.Module] = None

  override protected def genericToType(name: String): String = {
    val decl = paramsMap.get(Name(name))
    if (decl.nonEmpty) decl.get.getFullName else name
  }

  override val paths: Array[String] = emptyPaths

  override val location: Location = Location.default

  override val id: IdToken = typeInfo.typeName.id

  override val typeNameSegment: TypeNameSegment =
    createTypeName(genericDecl.id.toString, Some(typeArgs))
}

object GenericPlatformTypeDeclaration {

  def get(
    module: IPM.Module,
    typeArgs: ArraySeq[IModuleTypeDeclaration],
    td: PlatformTypeDeclaration
  ): GenericPlatformTypeDeclaration = {
    new GenericPlatformTypeDeclaration(module, typeArgs, td)
  }

}
