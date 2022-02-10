package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.expression.{SoqlExpression, SoslExpression}
import apex.jorje.semantic.ast.visitor.ValidationScope
import apex.jorje.semantic.compiler.sfdc.QueryValidator
import apex.jorje.semantic.symbol.resolver.SymbolResolver

class NoopQueryValidator extends QueryValidator {
  override def validateSoql(symbolResolver: SymbolResolver, validationScope: ValidationScope, soqlExpression: SoqlExpression): String = soqlExpression.getCanonicalQuery

  override def validateSosl(symbolResolver: SymbolResolver, validationScope: ValidationScope, soslExpression: SoslExpression): String = soslExpression.getCanonicalQuery
}
object NoopQueryValidator {
  def apply(): NoopQueryValidator = {
    new NoopQueryValidator
  }
}