package com.nawforce.runtime.workspace

import com.financialforce.oparser.{ITypeDeclaration, TypeNameSegment, TypeRef, UnresolvedTypeRef}

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
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    val typeNames = getUnresolvedTypeNames(typeRef)

    findScalarType(typeNames)
      .orElse(
        findLocalTypeFor(baseModule, typeNames, from)
          .orElse(getType(baseModule, typeNames))
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
    typeNames: ArrayBuffer[TypeNameSegment]
  ): Option[ITypeDeclaration] = {
    baseModule.findExactTypeId(asFullName(typeNames))
  }

  private def findScalarType(typeNames: ArrayBuffer[TypeNameSegment]): Option[ITypeDeclaration] = {
    //TODO
    None
  }

  private def findLocalTypeFor(
    baseModule: IPM.Module,
    typeNames: ArrayBuffer[TypeNameSegment],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    //Shortcut self reference
    if (typeNames.nonEmpty && !isCompound(typeNames) && from.id == typeNames.head.id)
      return Some(from)
    // Remove self reference but avoid false positive match against an inner
    else if (isCompound(typeNames) && from.id == typeNames.head.id && from.enclosing.isEmpty)
      findLocalTypeFor(baseModule, typeNames.tail, from)

    getNestedType(typeNames, from)
      .orElse(
        getFromOuterType(baseModule, typeNames, from)
          .orElse(getFromSuperType(baseModule, typeNames, from))
      )
  }

  private def getNestedType(
    typeNames: ArrayBuffer[TypeNameSegment],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    if (isCompound(typeNames)) {
      None
    } else {
      findNestedType(from, typeNames.head)
    }
  }

  private def getFromOuterType(
    baseModule: IPM.Module,
    typeNames: ArrayBuffer[TypeNameSegment],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
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
    from: ITypeDeclaration,
    name: TypeNameSegment
  ): Option[ITypeDeclaration] = {
    from.innerTypes.find(x => x.id == name.id)
  }

  private def getFromSuperType(
    baseModule: IPM.Module,
    typeNames: ArrayBuffer[TypeNameSegment],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    if (from.extendsTypeRef == null)
      return None

    from.extendsTypeRef match {
      case resolved: ITypeDeclaration => findLocalTypeFor(baseModule, typeNames, resolved)
      case _ =>
        val superTypeTypeNames = getUnresolvedTypeNames(from.extendsTypeRef)
        if (typeNames == superTypeTypeNames)
          return None

        val superType = findLocalTypeFor(baseModule, superTypeTypeNames, from).orElse({
          baseModule.findExactTypeId(asFullName(superTypeTypeNames))
        })

        //TODO: check namespaces?
        superType.flatMap(st => findLocalTypeFor(baseModule, typeNames, st))
    }
  }

  private def isCompound(typeNames: ArrayBuffer[TypeNameSegment]): Boolean = {
    typeNames.size > 1
  }

  private def asFullName(typeNames: ArrayBuffer[TypeNameSegment]): String = {
    typeNames.map(_.toString).mkString(".")
  }

}
