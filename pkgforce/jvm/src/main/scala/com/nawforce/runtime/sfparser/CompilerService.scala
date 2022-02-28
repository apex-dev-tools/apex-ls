/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor}
import apex.jorje.semantic.compiler._
import apex.jorje.semantic.compiler.parser.ParserEngine
import apex.jorje.semantic.compiler.sfdc.NoopCompilerProgressCallback
import org.apache.commons.lang3.reflect.{FieldUtils, MethodUtils}

import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.javaapi.CollectionConverters

object CompilerService {
  var suppressErrors = true

  def visitAstFromString(
    source: String,
    visitor: AstVisitor[AdditionalPassScope],
    parserEngineType: ParserEngine.Type
  ): ApexCompiler = {
    val sourceFile      = SourceFile.builder().setBody(source).build()
    val compilationUnit = createCompilationInput(List(sourceFile), visitor)
    compile(compilationUnit, parserEngineType)
  }

  /**
    * Configure a CompilationInput with the default configurations:
    * EmptySymbolProvider, doesn't provide any symbols that are not part of source.
    * NoopAccessEvaluator, doesn't provide any validation.
    * NoopQueryValidators, no validation of queries.
    */
  private def createCompilationInput(
    sourceFiles: List[SourceFile],
    visitor: AstVisitor[AdditionalPassScope]
  ): CompilationInput = {
    new CompilationInput(
      CollectionConverters.asJava(sourceFiles),
      EmptySymbolProvider(),
      NoopAccessEvaluator(),
      NoopQueryValidator(),
      visitor,
      NoopCompilerProgressCallback.get()
    )
  }

  private def compile(
    compilationInput: CompilationInput,
    parserEngineType: ParserEngine.Type
  ): ApexCompiler = {
    val compiler = ApexCompiler
      .builder()
      .setInput(compilationInput)
      .setParserType(parserEngineType)
      .build()
    compiler.compile(CompilerStage.POST_TYPE_RESOLVE)
    callAdditionalPassVisitor(compiler)
    showParserErrorsIfAny(compiler)
    compiler
  }

  /**
    * Taken from https://github.com/forcedotcom/idecore/blob/master/com.salesforce.ide.apex.core/src/com/salesforce/ide/apex/internal/core/CompilerService.java#L171
    * This is temporary workaround to bypass the validation stage of the compiler while *still* doing the
    * additional_validate stage. We are bypassing the validation stage because it does a deep validation that we don't
    * have all the parts for yet in the offline compiler. Rather than stop all work on that, we bypass it so that we
    * can still do useful things like find all your types, find all your methods, etc.
    */
  private def callAdditionalPassVisitor(apexCompiler: ApexCompiler): Unit = {
    try {
      val allUnits = FieldUtils
        .readDeclaredField(apexCompiler, "allUnits", true)
        .asInstanceOf[java.util.List[CodeUnit]]
        .asScala

      val compilerContext = FieldUtils
        .readDeclaredField(apexCompiler, "compilerContext", true)
        .asInstanceOf[CompilerContext]
      val getOperation =
        CompilerStage.ADDITIONAL_VALIDATE.getClass.getDeclaredMethod("getOperation")
      getOperation.setAccessible(true)
      allUnits.foreach(unit => {
        val op: CompilerOperation = getOperation
          .invoke(CompilerStage.ADDITIONAL_VALIDATE)
          .asInstanceOf[CompilerOperation]
        op.invoke(compilerContext, unit)
      })
    } catch {
      case e: IllegalArgumentException     => throw new RuntimeException(e)
      case e: ReflectiveOperationException => throw new RuntimeException(e)
    }
  }

  private def showParserErrorsIfAny(apexCompiler: ApexCompiler): Unit = {
    if (!suppressErrors)
      apexCompiler.getErrors.asScala.foreach(e => {
        println(e.getError)
      })
  }
}
