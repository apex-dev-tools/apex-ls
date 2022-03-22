package com.nawforce.runtime.workspace

import com.financialforce.oparser.{ITypeDeclaration, TypeName, TypeRef, UnresolvedTypeRef}

import scala.collection.mutable.ArrayBuffer

trait TypeFinder {
  self: IPM.Module =>

  def findType(typeRef: TypeRef, from: ITypeDeclaration): Option[ITypeDeclaration] = {
    val typeNames = getUnresolvedTypeNames(typeRef)
    if (typeNames.flatMap(_.typeArguments).nonEmpty) {
      //TODO: Resolve types with arguments properly
      return None
    }
    findScalarType(typeNames)
      .orElse(
        findLocalTypeFor(typeNames, from)
          .orElse(
            getType(typeNames)
              .orElse(getSystemTypes(typeNames))
          )
      )
  }

  private def getUnresolvedTypeNames(typeRef: TypeRef): ArrayBuffer[TypeName] = {
    typeRef match {
      case ur: UnresolvedTypeRef => ur.typeNames
      case _                     => ArrayBuffer.empty
    }
  }

  private def getType(typeNames: ArrayBuffer[TypeName]): Option[ITypeDeclaration] = {
    self.findExactTypeId(asFullName(typeNames))
  }

  private def findScalarType(typeNames: ArrayBuffer[TypeName]): Option[ITypeDeclaration] = {
    //TODO
    //Would it return a TD?
    None
  }

  private def getSystemTypes(typeNames: ArrayBuffer[TypeName]): Option[ITypeDeclaration] = {
    //TODO
    None
  }

  private def findLocalTypeFor(
    typeNames: ArrayBuffer[TypeName],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    //Shortcut self reference
    if (typeNames.nonEmpty && !isCompound(typeNames) && from.id == typeNames.head.id)
      return Some(from)
    // Remove self reference but avoid false positive match against an inner
    else if (isCompound(typeNames) && from.id == typeNames.head.id && from.enclosing.isEmpty)
      findLocalTypeFor(typeNames.tail, from)

    getNestedType(typeNames, from)
      .orElse(
        getFromOuterType(typeNames, from)
          .orElse(getFromSuperType(typeNames, from))
      )
  }

  private def getNestedType(
    typeNames: ArrayBuffer[TypeName],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    if (isCompound(typeNames)) {
      None
    } else {
      findNestedType(from, typeNames.head)
    }
  }

  private def getFromOuterType(
    typeNames: ArrayBuffer[TypeName],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    if (isCompound(typeNames) || from.enclosing.isEmpty) {
      None
    } else {
      val outerType = this.findLocalTypeFor(typeNames, from.enclosing.get)
      if (outerType.nonEmpty) {
        if (outerType.get.id == typeNames.head.id)
          outerType
        else
          findLocalTypeFor(typeNames, outerType.get)
      } else {
        None
      }
    }
  }

  private def findNestedType(from: ITypeDeclaration, name: TypeName): Option[ITypeDeclaration] = {
    from.innerTypes.find(x => x.id == name.id)
  }

  private def getFromSuperType(
    typeNames: ArrayBuffer[TypeName],
    from: ITypeDeclaration
  ): Option[ITypeDeclaration] = {
    if (from.extendsTypeRef == null)
      return None

    from.extendsTypeRef match {
      case resolved: ITypeDeclaration => findLocalTypeFor(typeNames, resolved)
      case _ =>
        val superTypeTypeNames = getUnresolvedTypeNames(from.extendsTypeRef)
        if (typeNames == superTypeTypeNames)
          return None

        val superType = findLocalTypeFor(superTypeTypeNames, from).orElse({
          self.findExactTypeId(asFullName(superTypeTypeNames))
        })

        //TODO: check namespaces?
        superType.flatMap(st => findLocalTypeFor(typeNames, st))
    }
  }

  private def isCompound(typeNames: ArrayBuffer[TypeName]): Boolean = {
    typeNames.size > 1
  }

  private def asFullName(typeNames: ArrayBuffer[TypeName]): String = {
    typeNames.map(_.id.id.contents).mkString(".")
  }

}
