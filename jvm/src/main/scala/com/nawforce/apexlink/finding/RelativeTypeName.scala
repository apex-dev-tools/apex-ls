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

package com.nawforce.apexlink.finding

import com.nawforce.apexlink.cst.{ScopeVerifyContext, CST, VerifyContext}
import com.nawforce.apexlink.finding.TypeResolver.TypeResponse
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.parsers.Nature
import com.nawforce.pkgforce.path.PathLocation

import scala.collection.mutable

/** Context to aid RelativeTypeName resolve via the originating ApexDeclaration. This needs freezing
  * after RelativeTypeNames are constructed due to the FullDeclaration not being constructed until
  * after its constituent parts, such as constructors and methods which use RelativeTypeName.
  */
final class RelativeTypeContext {
  var contextTypeDeclaration: TypeDeclaration = _
  private val typeCache                       = mutable.Map[TypeName, TypeResponse]()

  /** Freeze the RelativeTypeContext by providing access to the enclosing Apex class. */
  def freeze(typeDeclaration: TypeDeclaration): Unit = {
    assert(contextTypeDeclaration == null)
    contextTypeDeclaration = typeDeclaration
  }

  /** Reset internal caching, for use during re-validation. */
  def reset(): Unit = {
    typeCache.clear()
  }

  /** Resolve the passed typeName relative to the context class, returns None for ghosted types. */
  def resolve(typeName: TypeName): Option[TypeResponse] = {
    // Only use cache for positive results when result is not dead (has been superseded)
    // Otherwise we need to query so we get latest refresh state as types maybe removed/added
    typeCache.get(typeName) match {
      case Some(Right(td)) if !td.dead => Some(Right(td))
      case _ =>
        val response =
          // Workaround a stupid platform bug where the wrong type is used sometimes...
          if (typeName.outer.nonEmpty) {
            TypeResolver(typeName, contextTypeDeclaration.moduleDeclaration.get) match {
              case Right(td) => Right(td)
              case Left(_)   => TypeResolver(typeName, contextTypeDeclaration)
            }
          } else {
            TypeResolver(typeName, contextTypeDeclaration)
          }

        if (
          response.isLeft && contextTypeDeclaration.moduleDeclaration.get.isGhostedType(typeName)
        ) {
          None
        } else {
          if (response.isRight)
            typeCache.put(typeName, response)
          Some(response)
        }
    }
  }
}

/* Lazy TypeName resolver for relative types. The package & enclosing (outer) typename are used to allow
 * the relative TypeName to be converted to an absolute form. Assumes outerTypeName can always be resolved
 * against the module!
 */
final case class RelativeTypeName(typeContext: RelativeTypeContext, relativeTypeName: TypeName) {

  /** Is this the magical void? */
  def isVoid: Boolean = {
    relativeTypeName == TypeNames.Void
  }

  /** Obtain absolute type or fallback to relative if not found. */
  def typeName: TypeName = {
    if (isVoid) return TypeNames.Void
    typeContext.resolve(relativeTypeName) match {
      case Some(Right(td)) => td.typeName
      case _               => relativeTypeName
    }
  }

  /** Helper for creating a dependence on a relative type name. */
  def dependOn(location: PathLocation, context: VerifyContext): Unit = {
    if (relativeTypeName != TypeNames.Void) {
      typeContext.resolve(relativeTypeName) match {
        case Some(Left(error)) =>
          if (!context.module.isGulped)
            context.log(error.asIssue(location))
        case Some(Right(td)) =>
          context.addDependency(td)
          td.typeName.params.foreach(typeName =>
            context.getTypeAndAddDependency(typeName, typeContext.contextTypeDeclaration)
          )
        case None => ()
      }
    }
  }

  /** Helper for obtaining the nature of the outer type. */
  def outerNature: Nature = typeContext.contextTypeDeclaration.nature
}
