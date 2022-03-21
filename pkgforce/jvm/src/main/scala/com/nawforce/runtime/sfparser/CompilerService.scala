/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor}
import apex.jorje.semantic.compiler
import apex.jorje.semantic.compiler._
import apex.jorje.semantic.compiler.parser.ParserEngine
import apex.jorje.semantic.compiler.sfdc.NoopCompilerProgressCallback

import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.javaapi.CollectionConverters

object CompilerService {
  var suppressErrors = true

  def visitAstFromString(
    sourceFiles: List[SourceFile],
    parserEngineType: ParserEngine.Type
  ): (ApexCompiler, List[CodeUnit]) = {
    val compilationUnit = createCompilationInput(sourceFiles)
    compile(compilationUnit, parserEngineType)
  }

  /**
    * Configure a CompilationInput with the default configurations:
    * EmptySymbolProvider, doesn't provide any symbols that are not part of source.
    * NoopAccessEvaluator, doesn't provide any validation.
    * NoopQueryValidators, no validation of queries.
    */
  private def createCompilationInput(sourceFiles: List[SourceFile]): CompilationInput = {
    new CompilationInput(
      CollectionConverters.asJava(sourceFiles),
      EmptySymbolProvider(),
      NoopAccessEvaluator(),
      NoopQueryValidator(),
      new AstVisitor[AdditionalPassScope](),
      NoopCompilerProgressCallback.get()
    )
  }

  private def compile(
    compilationInput: CompilationInput,
    parserEngineType: ParserEngine.Type
  ): (ApexCompiler, List[compiler.CodeUnit]) = {
    val compiler = ApexCompiler
      .builder()
      .setInput(compilationInput)
      .setParserType(parserEngineType)
      .build()
    val cu = compiler.compile(CompilerStage.ADDITIONAL_VALIDATE)
    showParserErrorsIfAny(compiler)
    (compiler, cu.asScala.toList)
  }

  private def showParserErrorsIfAny(apexCompiler: ApexCompiler): Unit = {
    if (!suppressErrors)
      apexCompiler.getErrors.asScala.foreach(e => {
        println(e.getError)
      })
  }
}