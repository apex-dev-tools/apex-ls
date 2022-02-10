package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor}
import apex.jorje.semantic.compiler._
import apex.jorje.semantic.compiler.sfdc.NoopCompilerProgressCallback
import org.apache.commons.lang3.reflect.{FieldUtils, MethodUtils}

import scala.jdk.CollectionConverters.ListHasAsScala
import scala.jdk.javaapi.CollectionConverters

object CompilerService {

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
    compilerStage: CompilerStage
  ): ApexCompiler = {
    val compiler = ApexCompiler.builder().setInput(compilationInput).build()
    compiler.compile(compilerStage)
    callAdditionalPassVisitor(compiler)
    //TODO: throw parse errors
    compiler
  }

  private def callAdditionalPassVisitor(apexCompiler: ApexCompiler): Unit = {
    try {
      val allUnits = FieldUtils
        .readDeclaredField(apexCompiler, "allUnits", true)
        .asInstanceOf[java.util.List[CodeUnit]]
        .asScala
        .toList

      val compilerContext = FieldUtils
        .readDeclaredField(apexCompiler, "compilerContext", true)
        .asInstanceOf[CompilerContext]

      allUnits.foreach(unit => {
        val op: CompilerOperation = MethodUtils
          .invokeMethod(CompilerStage.ADDITIONAL_VALIDATE, true, "getOperation")
          .asInstanceOf[CompilerOperation]
        op.invoke(compilerContext, unit)
      })
    } catch {
      case e:IllegalArgumentException     => throw new RuntimeException(e)
      case e:ReflectiveOperationException => throw new RuntimeException(e)
    }
  }

  def visitAstFromString(source: String, visitor: AstVisitor[AdditionalPassScope]): ApexCompiler = {
    val sourceFile      = SourceFile.builder().setBody(source).build()
    val compilationUnit = createCompilationInput(List(sourceFile), visitor)
    compile(compilationUnit, CompilerStage.POST_TYPE_RESOLVE)
  }

}
