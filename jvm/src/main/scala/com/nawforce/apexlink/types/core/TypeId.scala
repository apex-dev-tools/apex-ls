/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.types.core

import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.org.OPM
import com.nawforce.pkgforce.names.{TypeIdentifier, TypeName}

import scala.reflect.ClassTag

case class TypeId(module: OPM.Module, typeName: TypeName) {
  def asTypeIdentifier: TypeIdentifier = {
    TypeIdentifier(module.pkg.namespace, typeName)
  }

  private[nawforce] def toTypeDeclaration[T <: TypeDeclaration: ClassTag]: Option[T] = {
    TypeResolver(typeName, module).toOption.collect { case td: T => td }
  }

  override def toString: String = asTypeIdentifier.toString
}
