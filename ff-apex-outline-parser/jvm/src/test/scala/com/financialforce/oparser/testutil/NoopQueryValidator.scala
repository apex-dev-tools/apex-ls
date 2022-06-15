/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser.testutil

import apex.jorje.semantic.ast.expression.{SoqlExpression, SoslExpression}
import apex.jorje.semantic.ast.visitor.ValidationScope
import apex.jorje.semantic.compiler.sfdc.QueryValidator
import apex.jorje.semantic.symbol.resolver.SymbolResolver

/**
  * Empty classes to provide a concrete implementation for the apex compiler
  */
class NoopQueryValidator extends QueryValidator {
  override def validateSoql(
    symbolResolver: SymbolResolver,
    validationScope: ValidationScope,
    soqlExpression: SoqlExpression
  ): String = soqlExpression.getCanonicalQuery

  override def validateSosl(
    symbolResolver: SymbolResolver,
    validationScope: ValidationScope,
    soslExpression: SoslExpression
  ): String = soslExpression.getCanonicalQuery
}
object NoopQueryValidator {
  def apply(): NoopQueryValidator = {
    new NoopQueryValidator
  }
}
