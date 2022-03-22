/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser._
import com.nawforce.runtime.workspace.IPM.Module

import scala.collection.immutable.ArraySeq

trait ModuleScoped {
  var _module: Option[IPM.Module] = None

  def module: Option[IPM.Module] = _module
}

trait IModuleTypeDeclaration extends ITypeDeclaration with ModuleScoped {
  /* TODO: Remove these hacks, think we need generics in Outline parser */
  def enclosingModule: Option[IModuleTypeDeclaration] =
    enclosing.map(td => td.asInstanceOf[IModuleTypeDeclaration])
  def innerTypesModule: ArraySeq[IModuleTypeDeclaration] =
    innerTypes.map(td => td.asInstanceOf[IModuleTypeDeclaration])
}

object ModuleScoped {
  def module(td: TypeDeclaration): Option[Module] = {
    td match {
      case scoped: ModuleScoped => scoped.module
      case _                    => None
    }
  }
}

class ModuleClassTypeDeclaration(path: String, enclosing: ClassTypeDeclaration)
    extends ClassTypeDeclaration(path, enclosing)
    with IModuleTypeDeclaration

class ModuleInterfaceTypeDeclaration(path: String, enclosing: ClassTypeDeclaration)
    extends InterfaceTypeDeclaration(path, enclosing)
    with IModuleTypeDeclaration

class ModuleEnumTypeDeclaration(path: String, enclosing: ClassTypeDeclaration)
    extends EnumTypeDeclaration(path, enclosing)
    with IModuleTypeDeclaration

object ModuleClassFactory extends TypeDeclarationFactory {
  override def createClassTypeDeclaration(
    path: String,
    enclosing: ClassTypeDeclaration
  ): ClassTypeDeclaration = {
    new ModuleClassTypeDeclaration(path, enclosing)
  }

  override def createInterfaceTypeDeclaration(
    path: String,
    enclosing: ClassTypeDeclaration
  ): InterfaceTypeDeclaration = {
    new ModuleInterfaceTypeDeclaration(path, enclosing)
  }

  override def createEnumTypeDeclaration(
    path: String,
    enclosing: ClassTypeDeclaration
  ): EnumTypeDeclaration = {
    new ModuleEnumTypeDeclaration(path, enclosing)
  }
}
