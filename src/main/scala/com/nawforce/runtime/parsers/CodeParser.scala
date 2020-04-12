/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
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
package com.nawforce.runtime.parsers

import java.io.ByteArrayInputStream

import com.nawforce.common.documents.{PositionImpl, RangeLocationImpl}
import com.nawforce.common.path.PathLike
import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import org.antlr.v4.runtime.CommonTokenStream

import scala.collection.JavaConverters._
import scala.util.hashing.MurmurHash3

case class Source(path: PathLike, code: String) {
  lazy val hash: Int = MurmurHash3.stringHash(code)
}

class ClippedStream(val source: Source, start: Int, stop: Int, val line: Int, val column: Int) {
  def parse(): Either[SyntaxException, ApexParser.BlockContext] = {
    val clipped = source.code.substring(start, stop+1)
    CodeParser(source.path, clipped).parseBlock()
  }
}

class CodeParser(val source: Source) extends
  CaseInsensitiveInputStream(new ByteArrayInputStream(source.code.getBytes)) {

  // CommonTokenStream is buffered so we can access to retrieve token sequence if needed
  val tokenStream = new CommonTokenStream(new ApexLexer(this))
  tokenStream.fill()

  def parseClass(): Either[SyntaxException, ApexParser.CompilationUnitContext] = {
    try {
      Right(getParser.compilationUnit())
    } catch {
      case ex: SyntaxException => Left(ex)
    }
  }

  def parseTrigger(): Either[SyntaxException, ApexParser.TriggerUnitContext] = {
    try {
      Right(getParser.triggerUnit())
    } catch {
      case ex: SyntaxException => Left(ex)
    }
  }

  def parseBlock(): Either[SyntaxException, ApexParser.BlockContext] = {
    try {
      Right(getParser.block())
    } catch {
      case ex: SyntaxException => Left(ex)
    }
  }

  // Test use only
  def parseLiteral(): ApexParser.LiteralContext = {
      getParser.literal()
  }

  def getRangeLocation(context: ParserRuleContext, lineOffset: Int=0, positionOffset: Int=0): RangeLocationImpl = {
    RangeLocationImpl(
      source.path.toString,
      PositionImpl(context.start.getLine, context.start.getCharPositionInLine)
        .adjust(lineOffset, positionOffset),
      PositionImpl(context.stop.getLine, context.stop.getCharPositionInLine + context.stop.getText.length)
        .adjust(lineOffset, positionOffset)
    )
  }

  def clipStream(context: ParserRuleContext): ClippedStream = {
    new ClippedStream(source,
      context.start.getStartIndex, context.stop.getStopIndex,
      context.start.getLine-1, context.start.getCharPositionInLine)
  }

  private def getParser: ApexParser = {
    val parser = new ApexParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(new ThrowingErrorListener())
    parser
  }
}

object CodeParser {
  type ParserRuleContext = org.antlr.v4.runtime.ParserRuleContext
  type TerminalNode = org.antlr.v4.runtime.tree.TerminalNode

  def apply(path: PathLike, code: String): CodeParser = {
    new CodeParser(Source(path, code))
  }

  // Helper for JS Portability
  def getText(context: ParserRuleContext): String = {
    context.getText
  }

  // Helper for JS Portability
  def getText(context: TerminalNode): String = {
    context.getText
  }

  // Helper for JS Portability
  def toScala[T](collection: java.util.List[T]): Seq[T] = {
    collection.asScala
  }

  // Helper for JS Portability
  def toScala[T](value: T): Option[T] = {
    Option(value)
  }
}
