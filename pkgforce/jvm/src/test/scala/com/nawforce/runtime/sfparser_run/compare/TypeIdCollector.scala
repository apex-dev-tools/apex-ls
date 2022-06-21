package com.nawforce.runtime.sfparser_run.compare

import com.financialforce.types.IdWithLocation
import com.nawforce.runtime.workspace.{
  ClassTypeDeclaration,
  IModuleTypeDeclaration,
  TypeDeclaration
}

import scala.collection.mutable.ArrayBuffer

trait TypeIdResolver {
  def canBeResolved(id: IdWithLocation): Boolean
}

class TypeIdCollector(allIds: ArrayBuffer[IdWithLocation]) extends TypeIdResolver {
  def canBeResolved(id: IdWithLocation): Boolean = {
    allIds.contains(id)
  }
}

object TypeIdCollector {
  def fromIModuleTypeDecls(tds: List[IModuleTypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[IdWithLocation] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: IModuleTypeDeclaration, acc: ArrayBuffer[IdWithLocation]): Unit = {
    acc.append(td.id)
    td match {
      case cls: ClassTypeDeclaration =>
        cls.innerTypes.foreach(appendToIds(_, acc))
      case _ =>
    }
  }

  def fromTypeDecls(tds: ArrayBuffer[TypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[IdWithLocation] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

}
