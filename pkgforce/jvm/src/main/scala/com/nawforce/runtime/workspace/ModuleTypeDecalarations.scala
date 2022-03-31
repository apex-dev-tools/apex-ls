/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser._
import com.nawforce.pkgforce.memory.IdentityEquality

import scala.collection.immutable.ArraySeq

trait IModuleTypeDeclaration extends ITypeDeclaration with IdentityEquality {

  var module: IPM.Module = _

  /* TODO: Remove these hacks, think we need generics in Outline parser */
  def enclosingModule: Option[IModuleTypeDeclaration] =
    enclosing.map(td => td.asInstanceOf[IModuleTypeDeclaration])
  def innerTypesModule: ArraySeq[IModuleTypeDeclaration] =
    innerTypes.map(td => td.asInstanceOf[IModuleTypeDeclaration])
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
