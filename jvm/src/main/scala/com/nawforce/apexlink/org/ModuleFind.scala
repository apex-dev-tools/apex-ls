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

import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.finding.TypeResolver.TypeResponse
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.modifiers.GLOBAL_MODIFIER
import com.nawforce.pkgforce.names.{EncodedName, Name, TypeName}

trait ModuleFind {
  self: OPM.Module =>

  /* Find a package/platform type. For general needs don't call this direct, use TypeRequest which will delegate here
   * if needed. This is the fallback handling for the TypeFinder which performs local searching for types, so this is
   * only useful if you know local searching is not required. */
  def findType(typeName: TypeName, from: TypeDeclaration): TypeResponse = {
    findType(typeName, Some(from))
  }

  def findType(typeName: TypeName): TypeResponse = {
    findType(typeName, None)
  }

  private def findType(typeName: TypeName, from: Option[TypeDeclaration]): TypeResponse = {
    if (namespace.nonEmpty) {
      val td = findPackageType(typeName.withTail(TypeName(namespace.get)), from).map(Right(_))
      if (td.nonEmpty)
        return td.get
    }

    val td = findPackageType(typeName, from).map(Right(_))
    if (td.nonEmpty)
      return td.get

    // From may be used to locate type variable types so must be accurate even for a platform type request
    from
      .map(TypeResolver.platformType(typeName, _))
      .orElse(Some(TypeResolver.platformTypeOnly(typeName, this)))
      .get
  }

  // Find locally, or fallback to a searching base packages
  def findPackageType(
    typeName: TypeName,
    from: Option[TypeDeclaration],
    inPackage: Boolean = true
  ): Option[TypeDeclaration] = {
    // Use aliased type name here so we don't mishandle an ambiguous typename when searching
    val targetType = TypeNames.aliasOrReturn(typeName)

    findPackageTypeInternal(targetType, from, inPackage)
  }

  private def findPackageTypeInternal(
    typeName: TypeName,
    from: Option[TypeDeclaration],
    inPackage: Boolean
  ): Option[TypeDeclaration] = {
    // Might be an outer in this module
    var declaration = findModuleType(typeName)
    if (declaration.nonEmpty) {
      if (inPackage || declaration.get.modifiers.contains(GLOBAL_MODIFIER))
        return declaration
      else
        return None
    }

    // Or maybe an inner
    if (typeName.outer.nonEmpty) {
      declaration = findPackageTypeInternal(typeName.outer.get, from, inPackage = inPackage)
        .flatMap(_.findNestedType(typeName.name).filter(td => td.isExternallyVisible || inPackage))
      if (declaration.nonEmpty)
        return declaration
    }

    // Try base modules & packages of this module
    nextModule.flatMap(next => next.findPackageType(typeName, from, next.pkg == pkg))
  }

  /** Find a type just in this module. */
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
