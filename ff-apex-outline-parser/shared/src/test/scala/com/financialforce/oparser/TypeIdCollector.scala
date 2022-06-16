/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import com.financialforce.oparser.Id
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
  def apply(tds: List[ITypeDeclaration]): TypeIdCollector = {
    val allIds: ArrayBuffer[Id] = ArrayBuffer()
    tds.foreach(td => appendToIds(td, allIds))
    new TypeIdCollector(allIds)
  }

  private def appendToIds(td: ITypeDeclaration, acc: ArrayBuffer[Id]): Unit = {
    acc.append(td.id)
    td.innerTypes.foreach(appendToIds(_, acc))
  }
}

