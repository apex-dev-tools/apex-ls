/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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
 */
package com.nawforce.runtime.parsers

import com.nawforce.pkgforce.diagnostics.{Diagnostic, Issue, SYNTAX_CATEGORY}
import com.nawforce.pkgforce.path.{Location, PathLike}
import io.github.apexdevtools.apexparser.ApexLexer
import org.antlr.v4.runtime.{BaseErrorListener, Parser, RecognitionException, Recognizer, Token}

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

class CollectingErrorListener(path: PathLike) extends BaseErrorListener {
  var _issues: mutable.ArrayBuffer[Issue] = _

  // Pattern to match lexer "token recognition error" messages containing escape sequences
  private val tokenErrorPattern = """token recognition error at: '(.*)'""".r
  private val escapePattern     = """.*(\\.).*""".r

  override def syntaxError(
    recognizer: Recognizer[_, _],
    offendingSymbol: Any,
    line: Int,
    charPositionInLine: Int,
    msg: String,
    e: RecognitionException
  ): Unit = {
    if (_issues == null)
      _issues = new mutable.ArrayBuffer[Issue]()

    val improvedMsg = msg match {
      case tokenErrorPattern(content) =>
        content match {
          case escapePattern(escape) =>
            s"Invalid escape sequence '$escape' in string"
          case _ => msg
        }
      case _ =>
        malformedMultilineStringMessage(recognizer, offendingSymbol).getOrElse(msg)
    }

    // Drop syntax errors that are recovery noise from an earlier error on the same line.
    // ANTLR's lexer recovery frequently emits cascading diagnostics at adjacent columns
    // (e.g. an invalid escape \s reports first at the backslash, then again at the closing
    // quote of the same string once recovery re-enters string lexing).
    if (!isRecoveryNoise(line, improvedMsg)) {
      _issues.addOne(
        new Issue(
          path,
          Diagnostic(SYNTAX_CATEGORY, Location(line, charPositionInLine), improvedMsg)
        )
      )
    }
  }

  private def isRecoveryNoise(line: Int, message: String): Boolean = {
    if (_issues.isEmpty) false
    else {
      val previous = _issues.last.diagnostic
      // Only collapse the typical "invalid escape" recovery cascade where one bad escape
      // in a string produces multiple diagnostics at neighbouring columns. Other same-line
      // duplicates (e.g. two genuine "missing ';'" errors on a one-line class) are kept.
      previous.location.startLine == line &&
      previous.message.startsWith("Invalid escape sequence") &&
      message.startsWith("Invalid escape sequence")
    }
  }

  // Detect the `'''abc'''` / `''''''` shape — three textually adjacent StringLiteral
  // tokens where the outer two are empty `''`. The lexer can't fold these into a
  // MultilineStringLiteral because the body must start on a new line, so they
  // surface as a generic "mismatched input" parser error on the middle token.
  // A WS token between the literals breaks adjacency and skips the rewrite.
  private def malformedMultilineStringMessage(
    recognizer: Recognizer[_, _],
    offendingSymbol: Any
  ): Option[String] = {
    (recognizer, offendingSymbol) match {
      case (parser: Parser, token: Token) if token.getType == ApexLexer.StringLiteral =>
        val tokens = parser.getTokenStream
        val idx    = token.getTokenIndex
        if (idx > 0 && idx < tokens.size - 1) {
          val prev = tokens.get(idx - 1)
          val next = tokens.get(idx + 1)
          if (
            prev.getType == ApexLexer.StringLiteral &&
            next.getType == ApexLexer.StringLiteral &&
            prev.getText == "''" &&
            next.getText == "''" &&
            prev.getStopIndex + 1 == token.getStartIndex &&
            token.getStopIndex + 1 == next.getStartIndex
          )
            Some(
              "Malformed multi-line string literal, the body of '''...''' must start on a new line"
            )
          else
            None
        } else None
      case _ => None
    }
  }

  def issues: ArraySeq[Issue] = {
    if (_issues != null)
      ArraySeq.unsafeWrapArray(_issues.toArray)
    else
      Issue.emptyArray
  }
}
