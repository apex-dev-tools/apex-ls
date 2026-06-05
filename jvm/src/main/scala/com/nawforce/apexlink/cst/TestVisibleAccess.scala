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
import com.nawforce.pkgforce.modifiers.PRIVATE_MODIFIER

object TestVisibleAccess {
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
