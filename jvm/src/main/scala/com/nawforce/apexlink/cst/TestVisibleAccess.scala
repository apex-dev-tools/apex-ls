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
import com.nawforce.apexlink.types.core.{FieldDeclaration, MethodDeclaration, TypeDeclaration}
import com.nawforce.pkgforce.modifiers.{
  GLOBAL_MODIFIER,
  Modifier,
  PRIVATE_MODIFIER,
  PROTECTED_MODIFIER,
  PUBLIC_MODIFIER
}
import com.nawforce.pkgforce.names.TypeName

object TestVisibleAccess {
  def fieldAccessError(field: FieldDeclaration, calledFrom: TypeDeclaration): Option[String] = {
    if (isInvalidPrivateFieldAccess(field, calledFrom)) {
      Some("Private @TestVisible fields can only be accessed from @IsTest classes")
    } else if (!isFieldAccessible(field, calledFrom)) {
      Some(s"Field is not visible: ${field.name}")
    } else {
      None
    }
  }

  def methodAccessError(method: MethodDeclaration, calledFrom: TypeDeclaration): Option[String] = {
    if (isInvalidPrivateMethodAccess(method, calledFrom)) {
      Some("Private @TestVisible methods can only be accessed from @IsTest classes")
    } else if (!isMethodAccessible(method, calledFrom)) {
      Some(s"Method is not visible: ${method.nameAndParameterTypes}")
    } else {
      None
    }
  }

  def isInvalidPrivateFieldAccess(field: FieldDeclaration, calledFrom: TypeDeclaration): Boolean = {
    field.isTestVisible &&
    field.visibility.contains(PRIVATE_MODIFIER) &&
    !isSameApexFile(field, calledFrom) &&
    !calledFrom.isUnitTestContext
  }

  def isInvalidPrivateMethodAccess(
    method: MethodDeclaration,
    calledFrom: TypeDeclaration
  ): Boolean = {
    method.isTestVisible &&
    method.visibility.contains(PRIVATE_MODIFIER) &&
    !isSameApexFile(method, calledFrom) &&
    !calledFrom.isUnitTestContext
  }

  private def isFieldAccessible(field: FieldDeclaration, calledFrom: TypeDeclaration): Boolean = {
    field.visibility
      .forall(visibility =>
        isAccessible(
          visibility,
          field.thisTypeIdOpt.map(_.typeName),
          isSameApexFile(field, calledFrom),
          field.isTestVisible,
          calledFrom
        )
      )
  }

  private def isMethodAccessible(
    method: MethodDeclaration,
    calledFrom: TypeDeclaration
  ): Boolean = {
    method.visibility
      .forall(visibility =>
        isAccessible(
          visibility,
          method.thisTypeIdOpt.map(_.typeName),
          isSameApexFile(method, calledFrom),
          method.isTestVisible,
          calledFrom
        )
      )
  }

  private def isAccessible(
    visibility: Modifier,
    ownerTypeName: Option[TypeName],
    isSameApexFile: Boolean,
    isTestVisible: Boolean,
    calledFrom: TypeDeclaration
  ): Boolean = {
    lazy val isSameTypeOrSubtype =
      ownerTypeName.exists(owner =>
        calledFrom.typeName == owner || calledFrom.extendsOrImplements(owner)
      )
    lazy val isUnitTestVisible = isTestVisible && calledFrom.isUnitTestContext

    visibility match {
      case PUBLIC_MODIFIER | GLOBAL_MODIFIER => true
      case PROTECTED_MODIFIER => isSameApexFile || isSameTypeOrSubtype || isUnitTestVisible
      case PRIVATE_MODIFIER   => isSameApexFile || isUnitTestVisible
      case _                  => false
    }
  }

  private def isSameApexFile(field: FieldDeclaration, calledFrom: TypeDeclaration): Boolean = {
    (field, calledFrom) match {
      case (af: ApexFieldLike, ad: ApexDeclaration) => af.location.path == ad.location.path
      case _                                        => false
    }
  }

  private def isSameApexFile(method: MethodDeclaration, calledFrom: TypeDeclaration): Boolean = {
    (method, calledFrom) match {
      case (am: ApexMethodLike, ad: ApexDeclaration) => am.location.path == ad.location.path
      case _                                         => false
    }
  }
}
