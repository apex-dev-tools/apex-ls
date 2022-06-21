/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import com.financialforce.types.ITypeDeclaration
import com.financialforce.types.base.IdWithLocation

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
  def apply(tds: List[ITypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[IdWithLocation] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: ITypeDeclaration, acc: ArrayBuffer[IdWithLocation]): Unit = {
    acc.append(td.id)
    td.innerTypes.foreach(appendToIds(_, acc))
  }
}
