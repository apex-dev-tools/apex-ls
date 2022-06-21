/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.workspace

import com.financialforce.types.base.{TypeNameSegment, TypeRef, UnresolvedTypeRef}

object TypeFinder {

  /**
    * Will try and find the type in this order
    * 1) Check if the type is a scalar type
    * 2) Check if the type is from the local declaration including nested types and super types
    * 3) Find the type from the baseModule and will push down the search all the way down to platform types
    * Returns None if it cant resolve the type
    */
  def get(
    baseModule: IPM.Module,
    typeRef: TypeRef,
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {
    typeRef match {
      case declaration: IModuleTypeDeclaration => Some(declaration)
      case unresolved: UnresolvedTypeRef =>
        val typeNames = unresolved.typeNameSegments
        findScalarType(typeNames)
          .orElse(
            findLocalTypeFor(baseModule, typeNames, from)
              .orElse(getType(baseModule, typeNames, from))
          )
      case _ => None
    }

  }

  private def getType(
    baseModule: IPM.Module,
    typeNames: Array[TypeNameSegment],
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {

    //Pre resolve relative type arguments
    val resolvedTypeNames = typeNames.map(segment => {
      val args = segment.typeArguments
      val newArgs = args.flatMap {
        case unref: UnresolvedTypeRef =>
          get(baseModule, unref, from)
        case other => Some(other)
      }
      if (args.nonEmpty && args.length == newArgs.length)
        segment.replaceArguments(newArgs)
      else
        segment
    })

    // If we have a ns, try it first before falling back to without for injected types that carry their own ns
    val unresolved = UnresolvedTypeRef(resolvedTypeNames, 0)
    from.module.namespace
      .flatMap(ns => baseModule.findExactTypeId(ns.value + "." + unresolved.fullName, unresolved))
      .orElse(baseModule.findExactTypeId(unresolved.fullName, unresolved))
      .orElse(baseModule.findExactTypeId(unresolved.fullName))
  }

  private def findScalarType(typeNames: Array[TypeNameSegment]): Option[IModuleTypeDeclaration] = {
    //TODO: We should implement this to gain some perf improvement but not needed to function properly as we
    // will push the search down to System package through findExactTypeId anyway
    None
  }

  private def findLocalTypeFor(
    baseModule: IPM.Module,
    typeNames: Array[TypeNameSegment],
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {
    //Shortcut self reference
    if (typeNames.nonEmpty && !isCompound(typeNames) && from.id == typeNames.head.id)
      return Some(from)
    // Remove self reference but avoid false positive match against an inner
    else if (isCompound(typeNames) && from.id == typeNames.head.id && from.enclosing.isEmpty)
      return findLocalTypeFor(baseModule, typeNames.tail, from)
    getNestedType(typeNames, from)
      .orElse(
        getFromOuterType(baseModule, typeNames, from)
          .orElse(getFromSuperType(baseModule, typeNames, from))
      )
  }

  private def getNestedType(
    typeNames: Array[TypeNameSegment],
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {
    if (isCompound(typeNames)) {
      None
    } else {
      findNestedType(from, typeNames.head)
    }
  }

  private def getFromOuterType(
    baseModule: IPM.Module,
    typeNames: Array[TypeNameSegment],
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {
    if (isCompound(typeNames) || from.enclosing.isEmpty) {
      None
    } else {
      val outerType = findLocalTypeFor(baseModule, typeNames, from.enclosing.get)
      if (outerType.nonEmpty) {
        if (outerType.get.id == typeNames.head.id)
          outerType
        else
          findLocalTypeFor(baseModule, typeNames, outerType.get)
      } else {
        None
      }
    }
  }

  private def findNestedType(
    from: IModuleTypeDeclaration,
    name: TypeNameSegment
  ): Option[IModuleTypeDeclaration] = {
    from.innerTypes.find(x => x.id == name.id)
  }

  private def getFromSuperType(
    baseModule: IPM.Module,
    typeNames: Array[TypeNameSegment],
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {
    def isTypeFromInner(toCheck: IModuleTypeDeclaration, from: IModuleTypeDeclaration): Boolean = {
      val outerTypeNames = toCheck.enclosing.map(x => x.typeName)
      outerTypeNames.nonEmpty && outerTypeNames.get.contains(from.typeNameSegment)
    }

    if (from.extendsTypeRef == null)
      return None

    from.extendsTypeRef match {
      case resolved: IModuleTypeDeclaration =>
        if (!isTypeFromInner(resolved, from)) {
          return findLocalTypeFor(baseModule, typeNames, resolved)
        }
        None
      case unresolvedTypeRef: UnresolvedTypeRef =>
        val superTypeTypeNames = unresolvedTypeRef.typeNameSegments
        if (typeNames sameElements superTypeTypeNames)
          return None

        val superType = findLocalTypeFor(baseModule, superTypeTypeNames, from).orElse({
          getType(baseModule, superTypeTypeNames, from)
        })

        //TODO: check namespaces?
        if (superType.nonEmpty && !isTypeFromInner(superType.get, from))
          return superType.flatMap(st => findLocalTypeFor(baseModule, typeNames, st))
        None
    }
  }

  private def isCompound(typeNames: Array[TypeNameSegment]): Boolean = {
    typeNames.length > 1
  }
}
