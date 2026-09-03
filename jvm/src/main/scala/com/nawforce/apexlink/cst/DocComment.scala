/*
 Copyright (c) 2026 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import com.nawforce.runtime.parsers.{CodeParser, SourceData}
import io.github.apexdevtools.apexparser.ApexLexer
import org.antlr.v4.runtime.{BufferedTokenStream, Token}

/** A declaration that may carry the ApexDoc comment written immediately before it. */
trait DocumentedDeclaration {

  /** Byte span of the attached `/** ... */` comment, sharing the bytes of the source it was parsed
    * from rather than copying them. Absent for undocumented declarations.
    */
  var docComment: Option[SourceData] = None
}

object DocComment {

  /** Find the doc comment attached to a declaration whose first token is the start of `anchor`.
    *
    * The anchor must be the start of the whole declaration, including any annotations and
    * modifiers, so that these never separate a doc comment from what it documents. Only whitespace
    * may sit between the comment and the anchor; any other token, including an ordinary comment,
    * prevents attachment.
    */
  def find(parser: CodeParser, anchor: ParserRuleContext): Option[SourceData] = {
    val start = if (anchor == null) null else anchor.start
    if (start == null)
      None
    else
      parser.lastTokenStream
        .flatMap(stream => findToken(stream, start.getTokenIndex))
        .map(token => parser.source.code.subdata(token.getStartIndex, token.getStopIndex + 1))
  }

  /* Walks the hidden tokens left of the anchor, stopping at the first token on the default channel */
  private def findToken(stream: BufferedTokenStream, tokenIndex: Int): Option[Token] = {
    var i = math.min(tokenIndex, stream.size) - 1
    while (i >= 0) {
      val token = stream.get(i)
      if (token.getChannel == Token.DEFAULT_CHANNEL)
        return None
      token.getType match {
        case ApexLexer.WS          => i -= 1
        case ApexLexer.DOC_COMMENT => return Some(token)
        case _                     => return None
      }
    }
    None
  }

  /** Plain text of a doc comment with the delimiters and leading line asterisks removed. Absent
    * when nothing but decoration remains, such as for a banner of asterisks.
    */
  def text(docComment: SourceData): Option[String] = text(docComment.asString)

  def text(raw: String): Option[String] = {
    var body = raw.trim
    if (body.startsWith("/**"))
      body = body.substring(3)
    if (body.endsWith("*/"))
      body = body.substring(0, body.length - 2)

    val lines = body
      .split("\r?\n", -1)
      .map(line => {
        val stripped = line.trim.dropWhile(_ == '*')
        val unpadded = if (stripped.startsWith(" ")) stripped.substring(1) else stripped
        unpadded.replaceAll("\\s+$", "")
      })
    val trimmed = lines.dropWhile(_.isEmpty).reverse.dropWhile(_.isEmpty).reverse
    if (trimmed.isEmpty) None else Some(trimmed.mkString("\n"))
  }
}
