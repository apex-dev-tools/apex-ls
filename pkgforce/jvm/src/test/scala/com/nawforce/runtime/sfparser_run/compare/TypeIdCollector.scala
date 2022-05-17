package com.nawforce.runtime.sfparser_run.compare

import com.financialforce.oparser.Id
import com.nawforce.runtime.workspace.{
  ClassTypeDeclaration,
  IModuleTypeDeclaration,
  TypeDeclaration
}

import scala.collection.mutable.ArrayBuffer

trait TypeIdResolver {
  def canBeResolved(id: Id): Boolean
}

class TypeIdCollector(allIds: ArrayBuffer[Id]) extends TypeIdResolver {
  def canBeResolved(id: Id): Boolean = {
    allIds.contains(id)
  }
}

object TypeIdCollector {
  def fromIModuleTypeDecls(tds: List[IModuleTypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[Id] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: IModuleTypeDeclaration, acc: ArrayBuffer[Id]): Unit = {
    acc.append(td.id)
    td match {
      case cls: ClassTypeDeclaration =>
        cls.innerTypes.foreach(appendToIds(_, acc))
      case _ =>
    }
  }

  def fromTypeDecls(tds: ArrayBuffer[TypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[Id] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

}
