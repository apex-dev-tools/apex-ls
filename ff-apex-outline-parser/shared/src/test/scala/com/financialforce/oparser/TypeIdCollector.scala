/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

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
  def apply(tds: List[ITypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[LocatableId] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: ITypeDeclaration, acc: ArrayBuffer[LocatableId]): Unit = {
    acc.append(td.id)
    td.innerTypes.foreach(appendToIds(_, acc))
  }
}
