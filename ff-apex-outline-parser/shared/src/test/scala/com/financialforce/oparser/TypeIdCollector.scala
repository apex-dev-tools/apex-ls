/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import scala.collection.mutable.ArrayBuffer

trait TypeIdResolver {
  def canBeResolved(id: IdToken): Boolean
}

class TypeIdCollector(allIds: ArrayBuffer[IdToken]) extends TypeIdResolver {
  def canBeResolved(id: IdToken): Boolean = {
    allIds.contains(id)
  }
}

object TypeIdCollector {
  def apply(tds: List[ITypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[IdToken] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: ITypeDeclaration, acc: ArrayBuffer[IdToken]): Unit = {
    acc.append(td.id)
    td.innerTypes.foreach(appendToIds(_, acc))
  }
}
