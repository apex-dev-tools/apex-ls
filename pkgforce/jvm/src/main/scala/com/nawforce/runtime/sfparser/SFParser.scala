package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.compilation._
import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.parsers.{Source, SourceData}

class SFParser(val source: Source) {

  def parse(): Compilation = {
    val visitor = new TopLevelVisitor()
    CompilerService.visitAstFromString(source.code.asString, visitor)
    visitor.getTopLevel
  }

  private class TopLevelVisitor extends AstVisitor[AdditionalPassScope] {
    private var topLevel: Option[Compilation] = None

    def getTopLevel: Compilation = topLevel.get

    override def visitEnd(node: UserClass, scope: AdditionalPassScope): Unit = topLevel = Some(node)

    override def visitEnd(node: UserEnum, scope: AdditionalPassScope): Unit = topLevel = Some(node)

    override def visitEnd(node: UserInterface, scope: AdditionalPassScope): Unit =
      topLevel = Some(node)

    override def visitEnd(node: UserTrigger, scope: AdditionalPassScope): Unit =
      topLevel = Some(node)
  }
}

object SFParser {

  def apply(path: PathLike, code: SourceData): SFParser = {
    new SFParser(Source(path, code, 0, 0, None))
  }
}
