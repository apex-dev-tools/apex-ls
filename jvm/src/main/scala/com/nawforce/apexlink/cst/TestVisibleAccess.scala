/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.types.apex.{ApexDeclaration, ApexFieldLike, ApexMethodLike}
import com.nawforce.apexlink.types.core.{
  AnyReturnMethodDeclaration,
  FieldDeclaration,
  MethodDeclaration,
  TypeDeclaration
}
import com.nawforce.pkgforce.modifiers.{
  GLOBAL_MODIFIER,
  Modifier,
  PRIVATE_MODIFIER,
  PROTECTED_MODIFIER,
  PUBLIC_MODIFIER,
  TEST_VISIBLE_ANNOTATION
}
import com.nawforce.pkgforce.names.TypeName

object TestVisibleAccess {
  sealed trait AccessResult {
    def isAccessible: Boolean
    def errorMessage: Option[String]
  }

  case object Accessible extends AccessResult {
    override val isAccessible: Boolean         = true
    override val errorMessage: Option[String] = None
  }

  case class Inaccessible(message: String) extends AccessResult {
    override val isAccessible: Boolean         = false
    override val errorMessage: Option[String] = Some(message)
  }

  def fieldAccessError(field: FieldDeclaration, calledFrom: TypeDeclaration): Option[String] = {
    field.visibility.flatMap(_ => access(field, Some(calledFrom)).errorMessage)
  }

  def methodAccessError(method: MethodDeclaration, calledFrom: TypeDeclaration): Option[String] = {
    // Resolve through any Any-return wrapper so the checks below see the real declaration; the
    // wrapper is not an ApexMethodLike and carries no owning type, which would otherwise make a
    // same-file private/protected call appear inaccessible.
    val resolved = method match {
      case wrapped: AnyReturnMethodDeclaration => wrapped.method
      case other                               => other
    }
    resolved.visibility.flatMap(_ => access(resolved, Some(calledFrom)).errorMessage)
  }

  def access(
    field: FieldDeclaration,
    calledFrom: Option[TypeDeclaration]
  ): AccessResult = {
    if (calledFrom.exists(isInvalidPrivateFieldAccess(field, _))) {
      Inaccessible("Private @TestVisible fields can only be accessed from @IsTest classes")
    } else if (!isAccessible(
                 field.visibility.getOrElse(PRIVATE_MODIFIER),
                 field.thisTypeIdOpt.map(_.typeName),
                 isSameApexFile(field, calledFrom),
                 field.isTestVisible,
                 calledFrom
               )) {
      Inaccessible(s"Field is not visible: ${field.name}")
    } else {
      Accessible
    }
  }

  def access(
    method: MethodDeclaration,
    calledFrom: Option[TypeDeclaration]
  ): AccessResult = {
    val resolved = method match {
      case wrapped: AnyReturnMethodDeclaration => wrapped.method
      case other                               => other
    }
    if (calledFrom.exists(isInvalidPrivateMethodAccess(resolved, _))) {
      Inaccessible("Private @TestVisible methods can only be accessed from @IsTest classes")
    } else if (!isAccessible(
                 resolved.visibility.getOrElse(PRIVATE_MODIFIER),
                 resolved.thisTypeIdOpt.map(_.typeName),
                 isSameApexFile(resolved, calledFrom),
                 resolved.isTestVisible,
                 calledFrom
               )) {
      Inaccessible(s"Method is not visible: ${resolved.nameAndParameterTypes}")
    } else {
      Accessible
    }
  }

  def access(
    declaration: TypeDeclaration,
    calledFrom: Option[TypeDeclaration]
  ): AccessResult = {
    if (!isAccessible(
          declaration.visibility.getOrElse(PRIVATE_MODIFIER),
          declaration.outerTypeName,
          isSameApexFile(declaration, calledFrom),
          declaration.modifiers.contains(TEST_VISIBLE_ANNOTATION),
          calledFrom
        )) {
      Inaccessible(s"Type is not visible: ${declaration.typeName}")
    } else {
      Accessible
    }
  }

  private def isInvalidPrivateFieldAccess(
    field: FieldDeclaration,
    calledFrom: TypeDeclaration
  ): Boolean = {
    field.isTestVisible &&
    field.visibility.contains(PRIVATE_MODIFIER) &&
    !isSameApexFile(field, Some(calledFrom)) &&
    !calledFrom.isUnitTestContext
  }

  private def isInvalidPrivateMethodAccess(
    method: MethodDeclaration,
    calledFrom: TypeDeclaration
  ): Boolean = {
    method.isTestVisible &&
    method.visibility.contains(PRIVATE_MODIFIER) &&
    !isSameApexFile(method, Some(calledFrom)) &&
    !calledFrom.isUnitTestContext
  }

  private def isAccessible(
    visibility: Modifier,
    ownerTypeName: Option[TypeName],
    isSameApexFile: Boolean,
    isTestVisible: Boolean,
    calledFrom: Option[TypeDeclaration]
  ): Boolean = {
    lazy val isSameTypeOrSubtype =
      calledFrom.exists(from =>
        ownerTypeName.exists(owner => from.typeName == owner || from.extendsOrImplements(owner))
      )
    lazy val isUnitTestVisible = isTestVisible && calledFrom.exists(_.isUnitTestContext)

    visibility match {
      case PUBLIC_MODIFIER | GLOBAL_MODIFIER => true
      case PROTECTED_MODIFIER => isSameApexFile || isSameTypeOrSubtype || isUnitTestVisible
      case PRIVATE_MODIFIER   => isSameApexFile || isUnitTestVisible
      case _                  => false
    }
  }

  private def isSameApexFile(
    field: FieldDeclaration,
    calledFrom: Option[TypeDeclaration]
  ): Boolean = {
    (field, calledFrom.orNull) match {
      case (af: ApexFieldLike, ad: ApexDeclaration) => af.location.path == ad.location.path
      case _                                        => false
    }
  }

  private def isSameApexFile(
    method: MethodDeclaration,
    calledFrom: Option[TypeDeclaration]
  ): Boolean = {
    (method, calledFrom.orNull) match {
      case (am: ApexMethodLike, ad: ApexDeclaration) => am.location.path == ad.location.path
      case _                                         => false
    }
  }

  private def isSameApexFile(
    declaration: TypeDeclaration,
    calledFrom: Option[TypeDeclaration]
  ): Boolean = {
    (declaration, calledFrom.orNull) match {
      case (target: ApexDeclaration, from: ApexDeclaration) =>
        target.location.path == from.location.path
      case _ => false
    }
  }
}
