/*
 [The "BSD licence"]
 Copyright (c) 2017 Kevin Jones
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package io.github.nawforce.apexlink.metadata

import java.io.{File, FileInputStream}

import io.github.nawforce.apexlink.antlr.{ApexLexer, ApexParser}
import io.github.nawforce.apexlink.cst._
import io.github.nawforce.apexlink.utils._
import org.antlr.v4.runtime.CommonTokenStream

import scala.collection.mutable

case class ApexClass(location: Location, fullName: String, compilationUnit: CompilationUnit) extends Symbol {
  val scopedName : String = fullName
  val index = new CSTIndex

  compilationUnit.resolve(index)

  def methodDeclarations : mutable.Set[CST] = index.get("MethodDeclaration")

  def expressions : List[Expression] = {
    expressions(compilationUnit)
  }

  def statements : List[Statement] = {
    statements(compilationUnit)
  }

  private def expressions(cst: CST) : List[Expression] = {
    cst match {
      case e: Expression => List(e)
      case _ => cst.children().flatMap(x => expressions(x))
    }
  }

  private def statements(cst: CST) : List[Statement] = {
    cst match {
      case s: Statement => List(s)
      case _ => cst.children().flatMap(x => statements(x))
    }
  }
}

object ApexClass {
  def create(fullName: String, path: String): Option[ApexClass] = {

    try {
      val listener = new ThrowingErrorListener
      val fis: FileInputStream = new FileInputStream(new File(path))
      val cis: CaseInsensitiveInputStream = new CaseInsensitiveInputStream(fis)
      val lexer: ApexLexer = new ApexLexer(cis)
      lexer.removeErrorListeners()
      lexer.addErrorListener(listener)

      val tokens: CommonTokenStream = new CommonTokenStream(lexer)
      tokens.fill()

      val parser: ApexParser = new ApexParser(tokens)
      parser.removeErrorListeners()
      parser.setTrace(false)
      parser.addErrorListener(listener)

      val cu = CompilationUnit.construct(parser.compilationUnit())
      Some(new ApexClass(new Location(path, 0), fullName, cu))
    } catch {
      case se: SyntaxException =>
        LinkerLog.logMessage(path, se.line, se.msg)
        None
    }
  }

  def main(args: Array[String]): Unit = {
    val fis: FileInputStream = new FileInputStream(new File(args.head))
    val cis: CaseInsensitiveInputStream = new CaseInsensitiveInputStream(fis)
    cis.dump()
    val lexer: ApexLexer = new ApexLexer(cis)

    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    tokens.fill()

    val parser: ApexParser = new ApexParser(tokens)
    parser.compilationUnit()
  }
}


