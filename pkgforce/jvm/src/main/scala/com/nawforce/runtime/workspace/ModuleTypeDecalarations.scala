/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser._
import com.nawforce.runtime.workspace.IPM.Module

trait ModuleScoped {
  var module: Option[IPM.Module] = None
}

object ModuleScoped {
  def module(td: TypeDeclaration): Option[Module] = {
    td match {
      case scoped: ModuleScoped => scoped.module
      case _                    => None
    }
  }
}

class ModuleClassTypeDeclaration(path: String, enclosing: Option[ClassTypeDeclaration])
    extends ClassTypeDeclaration(path, enclosing)
    with ModuleScoped

class ModuleInterfaceTypeDeclaration(path: String, enclosing: Option[ClassTypeDeclaration])
    extends InterfaceTypeDeclaration(path, enclosing)
    with ModuleScoped

class ModuleEnumTypeDeclaration(path: String, enclosing: Option[ClassTypeDeclaration])
    extends EnumTypeDeclaration(path, enclosing)
    with ModuleScoped

object ModuleClassFactory extends TypeDeclarationFactory {
  override def createClassTypeDeclaration(
    path: String,
    enclosing: Option[ClassTypeDeclaration]
  ): ClassTypeDeclaration = {
    new ModuleClassTypeDeclaration(path, enclosing)
  }

  override def createInterfaceTypeDeclaration(
    path: String,
    enclosing: Option[ClassTypeDeclaration]
  ): InterfaceTypeDeclaration = {
    new ModuleInterfaceTypeDeclaration(path, enclosing)
  }

  override def createEnumTypeDeclaration(
    path: String,
    enclosing: Option[ClassTypeDeclaration]
  ): EnumTypeDeclaration = {
    new ModuleEnumTypeDeclaration(path, enclosing)
  }
}
