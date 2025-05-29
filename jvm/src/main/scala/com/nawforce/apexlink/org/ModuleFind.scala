/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.org

import com.nawforce.apexlink.finding.{InaccessibleType, MissingType, TypeResolver}
import com.nawforce.apexlink.finding.TypeResolver.TypeResponse
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.{InternalInterface, InternalObject, TypeNameUtils}
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.{DotName, EncodedName, Name, TypeName}

import scala.annotation.tailrec

/** Provides type resolution utilities for modules, including searching for type declarations
  * within the current module, its package, and dependencies.
  *
  * This trait is intended to be mixed into an `OPM.Module` and supplies methods for:
  * - Resolving types by name, with or without context.
  * - Handling nested and platform types.
  * - Enforcing visibility and accessibility rules during type lookup.
  *
  * The main entry points are the `findType` methods, which delegate to internal helpers
  * for package, module, and nested type resolution.
  */
trait ModuleFind {
  self: OPM.Module =>

  /** Finds a type declaration for the given type name, using the specified context type as the origin.
    *
    * @param typeName The type name to search for.
    * @param from     The originating type declaration for context.
    * @return         Either the resolved TypeDeclaration, or an error if not found or inaccessible.
    */
  def findType(typeName: TypeName, from: TypeDeclaration): TypeResponse = {
    findType(typeName, Some(from))
  }

  /** Finds a type declaration for the given type name, without a specific context type.
    * @param typeName The type name to search for.
    * @return         Either the resolved TypeDeclaration, or an error if not found or inaccessible.
    */
  def findType(typeName: TypeName): TypeResponse = {
    findType(typeName, None)
  }

  /** Core type resolution logic for finding a type declaration by name, optionally using a context type.
    *
    * @param typeName The type name to search for.
    * @param from     Optional context type declaration for platform type resolution.
    * @return         Either the resolved TypeDeclaration, or an error if not found or inaccessible.
    */
  private def findType(typeName: TypeName, from: Option[TypeDeclaration]): TypeResponse = {
    val targetType = TypeNames.aliasOrReturn(typeName)
    if ((typeName ne InternalObject) && (typeName ne InternalInterface)) {

      // Search for package type
      findPackageType(targetType) match {
        case Right(declaration)                         => return Right(declaration)
        case Left(InaccessibleType(typeName: TypeName)) => return Left(InaccessibleType(typeName))
        case Left(_)                                    => ()
      }
    }

    // Search for platform type
    // From may be used to locate type variable types so must be accurate even for a platform type request
    from
      .map(TypeResolver.platformType(targetType, _))
      .orElse(Some(TypeResolver.platformTypeOnly(typeName, this)))
      .get
  }

  /** Finds a type declaration within the current package or its dependencies.
    * - Returns `Left(InaccessibleType)` if the type is found but not visible.
    * - Returns `Left(MissingType)` if the type cannot be found in any module.
    *
    * @param typeName The type name to search for.
    * @return         Either the resolved TypeDeclaration, or an error if not found or inaccessible.
    */
  private def findPackageType(typeName: TypeName): TypeResponse = {
    // As we may have many modules to search it's best to pre-generate typenames to match against first
    var searchTypeNamesWithoutNamespace: List[(TypeName, Option[TypeName])] = Nil
    var searchTypeNamesWithNamespace: List[(TypeName, Option[TypeName])]    = Nil
    var target: Option[TypeName]                                            = Some(typeName)
    var residual: Option[TypeName]                                          = None
    while (target.nonEmpty) {
      searchTypeNamesWithoutNamespace = (target.get, residual) :: searchTypeNamesWithoutNamespace
      if (namespace.nonEmpty)
        searchTypeNamesWithNamespace =
          (target.get.withTail(TypeName(namespace.get)), residual) :: searchTypeNamesWithNamespace
      residual = residual
        .map(_.withTail(target.get.inner()))
        .orElse(Some(TypeName(target.get.name, target.get.params, None)))
      target = target.get.outer
    }
    val searchTypeNames =
      searchTypeNamesWithNamespace.reverse ++ searchTypeNamesWithoutNamespace.reverse

    // Search over modules
    var inPackage                        = true
    var targetModule: Option[OPM.Module] = Some(this)
    while (targetModule.nonEmpty) {
      var targetTypeNames = searchTypeNames
      while (targetTypeNames != Nil) {
        val (searchTypeName, residual) = targetTypeNames.head
        targetModule.get
          .findModuleType(searchTypeName)
          .foreach(declaration => {
            // Found something, so terminate search
            return if (!inPackage && !declaration.isExternallyVisible) {
              Left(InaccessibleType(searchTypeName))
            } else if (residual.nonEmpty) {
              resolveNestedType(searchTypeName, inPackage, declaration, residual.get.asDotName)
            } else {
              Right(declaration)
            }
          })
        targetTypeNames = targetTypeNames.tail
      }
      targetModule = targetModule.flatMap(_.nextModule)
      inPackage = targetModule.exists(_.pkg == pkg)
    }
    Left(MissingType(typeName))
  }

  /** Recursively resolves a nested type within a type declaration.
    *
    * @param searchTypeName The current (outer) type name being resolved.
    * @param inPackage      True if searching within the same package, affecting visibility checks.
    * @param declaration    The current type declaration to search for nested types.
    * @param names          The remaining nested names to resolve, as a DotName.
    * @return               Either the resolved nested TypeDeclaration, or an error if not found or inaccessible.
    */
  @tailrec
  private def resolveNestedType(
    searchTypeName: TypeName,
    inPackage: Boolean,
    declaration: TypeDeclaration,
    names: DotName
  ): TypeResponse = {
    val typeName = TypeName(names.firstName, Nil, Some(searchTypeName))
    declaration
      .findNestedType(names.firstName) match {
      case Some(declaration) if inPackage || declaration.isExternallyVisible =>
        if (names.names.size == 1)
          Right(declaration)
        else resolveNestedType(typeName, inPackage, declaration, names.tail)
      case Some(_) =>
        Left(InaccessibleType(typeName))
      case None =>
        Left(MissingType(typeName))
    }
  }

  /** Find a type declaration searching within this module.
    * - Returns `None` if the type is not found in this module.
    */
  def findModuleType(typeName: TypeName): Option[TypeDeclaration] = {
    // Direct hit
    val declaration = types.get(typeName)
    if (declaration.nonEmpty)
      return declaration

    // SObject and alike, we want module specific version of these
    if (
      typeName.params.isEmpty &&
      (typeName.outer.isEmpty || typeName.outer.contains(TypeNames.Schema))
    ) {
      if (EncodedName.encodedNeedsNamespace(typeName.name))
        types.getWithSchema(TypeName(Name(namespacePrefix + typeName.name.value), Nil, None))
      else
        types.getWithSchema(typeName)
    } else {
      None
    }
  }
}
