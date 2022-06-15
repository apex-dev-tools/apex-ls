/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser.testutil

import apex.jorje.semantic.compiler.Namespace
import apex.jorje.semantic.compiler.sfdc.SymbolProvider
import apex.jorje.semantic.symbol.`type`.TypeInfo
import apex.jorje.semantic.symbol.resolver.SymbolResolver

/**
  * Empty class that doesn't provide any symbols that are not part of source.
  * It is needed to provide a concrete implementation for the apex compiler
  */
class EmptySymbolProvider extends SymbolProvider {
  override def find(symbolResolver: SymbolResolver, typeInfo: TypeInfo, s: String): TypeInfo = null

  override def getVfComponentType(
    symbolResolver: SymbolResolver,
    typeInfo: TypeInfo,
    namespace: Namespace,
    s: String
  ): TypeInfo = null

  override def getFlowInterviewType(
    symbolResolver: SymbolResolver,
    typeInfo: TypeInfo,
    namespace: Namespace,
    s: String
  ): TypeInfo = null

  override def getSObjectType(typeInfo: TypeInfo, s: String): TypeInfo = null

  override def getAggregateResultType(typeInfo: TypeInfo): TypeInfo = null

  override def getPageReference(typeInfo: TypeInfo, s: String): String = null

  override def hasLabelField(typeInfo: TypeInfo, namespace: Namespace, s: String): Boolean = false

  override def getQuickAction(typeInfo: TypeInfo, s: String, s1: String): String = null

  override def isDynamicTypeNamespace(s: String): Boolean = false

  override def isDynamicTypeNamespace(s: String, s1: String): Boolean = false
}

object EmptySymbolProvider {

  def apply(): EmptySymbolProvider = {
    new EmptySymbolProvider()
  }
}
