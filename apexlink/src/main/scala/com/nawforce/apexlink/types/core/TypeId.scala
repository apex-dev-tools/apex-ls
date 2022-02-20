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

import com.nawforce.apexlink.org.OPM
import com.nawforce.pkgforce.names.{TypeIdentifier, TypeName}

case class TypeId(module: OPM.Module, typeName: TypeName) {
  def asTypeIdentifier: TypeIdentifier = {
    TypeIdentifier(module.pkg.namespace, typeName)
  }

  override def toString: String = asTypeIdentifier.toString
}

object TypeId {
  def apply(module: OPM.Module, typeIdentifier: TypeIdentifier): Option[TypeId] = {
    // Quick test if module is the right one
    val typeName = typeIdentifier.typeName
    if (typeIdentifier.namespace == module.namespace)
      return Some(new TypeId(module, typeName))

    // Fallback to searching for the right module
    val pkg = module.pkg.org.packagesByNamespace.get(typeIdentifier.namespace)
    if (pkg.nonEmpty) {
      var module = pkg.get.orderedModules.find(_.findModuleType(typeName).nonEmpty)
      if (module.nonEmpty)
        return Some(TypeId(module.get, typeName))

      if (typeName.outer.nonEmpty) {
        module = pkg.get.orderedModules.find(_.findModuleType(typeName.outer.get).nonEmpty)
        if (module.nonEmpty)
          return Some(TypeId(module.get, typeName))
      }
    }

    assert(false)
    None
  }
}
