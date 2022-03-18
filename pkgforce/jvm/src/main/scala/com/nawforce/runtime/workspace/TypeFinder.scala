package com.nawforce.runtime.workspace

import com.financialforce.oparser.{TypeDeclaration, TypeName, TypeRef, UnresolvedTypeRef}
import com.nawforce.pkgforce.names.Name

import scala.collection.mutable.ArrayBuffer

trait TypeFinder {
  self: IPM.Module =>

  def findType(typeRef: TypeRef, from: TypeDeclaration): Option[TypeDeclaration] = {
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

  private def getType(typeNames: ArrayBuffer[TypeName]): Option[TypeDeclaration] = {
    self.findExactTypeId(asFullName(typeNames))
  }

  private def findScalarType(typeNames: ArrayBuffer[TypeName]): Option[TypeDeclaration] = {
    //TODO
    //Would it return a TD?
    None
  }

  private def getSystemTypes(typeNames: ArrayBuffer[TypeName]): Option[TypeDeclaration] = {
    //TODO
    None
  }

  private def findLocalTypeFor(
    typeNames: ArrayBuffer[TypeName],
    from: TypeDeclaration
  ): Option[TypeDeclaration] = {
    //Shortcut self reference
    if (typeNames.nonEmpty && !isCompound(typeNames) && from.id.contains(typeNames.head.id))
      return Some(from)
    // Remove self reference but avoid false positive match against an inner
    else if (isCompound(typeNames) && from.id.contains(typeNames.head.id) && from.enclosing.isEmpty)
      findLocalTypeFor(typeNames.tail, from)

    getNestedType(typeNames, from)
      .orElse(
        getFromOuterType(typeNames, from)
          .orElse(getFromSuperType(typeNames, from))
      )
  }

  private def getNestedType(
    typeNames: ArrayBuffer[TypeName],
    from: TypeDeclaration
  ): Option[TypeDeclaration] = {
    if (isCompound(typeNames)) {
      None
    } else {
      findNestedType(from, typeNames.head)
    }
  }

  private def getFromOuterType(
    typeNames: ArrayBuffer[TypeName],
    from: TypeDeclaration
  ): Option[TypeDeclaration] = {
    if (isCompound(typeNames) || from.enclosing.isEmpty) {
      None
    } else {
      val outerType = this.findLocalTypeFor(typeNames, from.enclosing.get)
      if (outerType.nonEmpty) {
        if (outerType.get.id.contains(typeNames.head.id))
          outerType
        else
          findLocalTypeFor(typeNames, outerType.get)
      } else {
        None
      }
    }
  }

  private def findNestedType(from: TypeDeclaration, name: TypeName): Option[TypeDeclaration] = {
    from.innerTypes.find(x => x.id.nonEmpty && x.id.get == name.id)
  }

  private def getFromSuperType(
    typeNames: ArrayBuffer[TypeName],
    from: TypeDeclaration
  ): Option[TypeDeclaration] = {
    if (from.extendsTypeRef.isEmpty)
      return None

    from.extendsTypeRef.get match {
      case resolved: TypeDeclaration => findLocalTypeFor(typeNames, resolved)
      case _ =>
        val superTypeTypeNames = getUnresolvedTypeNames(from.extendsTypeRef.get)
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
