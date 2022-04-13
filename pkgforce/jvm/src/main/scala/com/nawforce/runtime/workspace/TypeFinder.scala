/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.workspace

import com.financialforce.oparser.{TypeNameSegment, TypeRef, UnresolvedTypeRef}

import scala.collection.mutable.ArrayBuffer

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
    val typeNames = getUnresolvedTypeNames(typeRef)

    findScalarType(typeNames)
      .orElse(
        findLocalTypeFor(baseModule, typeNames, from)
          .orElse(getType(baseModule, typeNames, from))
      )
  }

  private def getUnresolvedTypeNames(typeRef: TypeRef): ArrayBuffer[TypeNameSegment] = {
    typeRef match {
      case ur: UnresolvedTypeRef => ur.typeNameSegments
      case _                     => ArrayBuffer.empty
    }
  }

  private def getType(
    baseModule: IPM.Module,
    typeNames: ArrayBuffer[TypeNameSegment],
    from: IModuleTypeDeclaration
  ): Option[IModuleTypeDeclaration] = {
    //Pre resolve relative type arguments
    //TODO: check efficiency of this, we are resolving all the arguments to just to turn into a string and resolve
    // it again in findExactTypeId
    typeNames.foreach(segment => {
      val args = segment.getArguments
      val newArgs = args.flatMap {
        case unref: UnresolvedTypeRef =>
          get(baseModule, unref, from)
        case other => Some(other)
      }
      if (args.nonEmpty && args.length == newArgs.length)
        segment.replaceArguments(newArgs)
    })

    // If we have a ns, try it first before falling back to without for injected types that carry their own ns
    val fullName = asFullName(typeNames)
    from.module.namespace
      .flatMap(ns => baseModule.findExactTypeId(ns.value + "." + fullName))
      .orElse(baseModule.findExactTypeId(fullName))
  }

  private def findScalarType(
    typeNames: ArrayBuffer[TypeNameSegment]
  ): Option[IModuleTypeDeclaration] = {
    //TODO: We should implement this to gain some perf improvement but not needed to function properly as we
    // will push the search down to System package through findExactTypeId anyway
    None
  }

  private def findLocalTypeFor(
    baseModule: IPM.Module,
    typeNames: ArrayBuffer[TypeNameSegment],
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
    typeNames: ArrayBuffer[TypeNameSegment],
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
    typeNames: ArrayBuffer[TypeNameSegment],
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
    typeNames: ArrayBuffer[TypeNameSegment],
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
      case _ =>
        val superTypeTypeNames = getUnresolvedTypeNames(from.extendsTypeRef)
        if (typeNames == superTypeTypeNames)
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

  private def isCompound(typeNames: ArrayBuffer[TypeNameSegment]): Boolean = {
    typeNames.size > 1
  }

  private def asFullName(typeNames: ArrayBuffer[TypeNameSegment]): String = {
    typeNames.map(_.toString).mkString(".")
  }

}
