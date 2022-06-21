package com.nawforce.runtime.sfparser_run.compare

import com.financialforce.oparser.LocatableId
import com.nawforce.runtime.workspace.{
  ClassTypeDeclaration,
  IModuleTypeDeclaration,
  TypeDeclaration
}

import scala.collection.mutable.ArrayBuffer

trait TypeIdResolver {
  def canBeResolved(id: LocatableId): Boolean
}

class TypeIdCollector(allIds: ArrayBuffer[LocatableId]) extends TypeIdResolver {
  def canBeResolved(id: LocatableId): Boolean = {
    allIds.contains(id)
  }
}

object TypeIdCollector {
  def fromIModuleTypeDecls(tds: List[IModuleTypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[LocatableId] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: IModuleTypeDeclaration, acc: ArrayBuffer[LocatableId]): Unit = {
    acc.append(td.id)
    td match {
      case cls: ClassTypeDeclaration =>
        cls.innerTypes.foreach(appendToIds(_, acc))
      case _ =>
    }
  }

  def fromTypeDecls(tds: ArrayBuffer[TypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[LocatableId] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

}
