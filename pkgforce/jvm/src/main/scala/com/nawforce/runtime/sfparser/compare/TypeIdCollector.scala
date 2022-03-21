package com.nawforce.runtime.sfparser.compare

import com.financialforce.oparser.{ClassTypeDeclaration, Id, TypeDeclaration}

import scala.collection.mutable.ArrayBuffer

trait TypeIdResolver {
  def canBeResolved(id: Id): Boolean
}

class TypeIdCollector(tds: List[TypeDeclaration]) extends TypeIdResolver {
  private val allIds: ArrayBuffer[Id] = ArrayBuffer()
  tds.foreach(td => appendToIds(td, allIds))

  def canBeResolved(id: Id): Boolean = {
    allIds.contains(id)
  }

  private def appendToIds(td: TypeDeclaration, acc: ArrayBuffer[Id]): Unit = {
    acc.append(td.id)
    td match {
      case cls: ClassTypeDeclaration =>
        cls.innerTypes.foreach(appendToIds(_, acc))
      case _ =>
    }
  }
}
