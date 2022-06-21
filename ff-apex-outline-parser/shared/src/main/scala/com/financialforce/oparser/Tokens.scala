/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import com.financialforce.types.base.{IdLocationHolder, Location}

import scala.collection.mutable

object Tokens {
  val Newline: Char          = '\n'
  val CarriageReturn: Char   = '\r'
  val LBrace: Char           = '{'
  val LBraceStr: String      = "{"
  val RBrace: Char           = '}'
  val RBraceStr: String      = "}"
  val LParen: Char           = '('
  val LParenStr: String      = "("
  val RParen: Char           = ')'
  val RParenStr: String      = ")"
  val LBrack: Char           = '['
  val LBrackStr: String      = "["
  val RBrack: Char           = ']'
  val RBrackStr: String      = "]"
  val LessThan: Char         = '<'
  val LessThanStr: String    = "<"
  val GreaterThan: Char      = '>'
  val GreaterThanStr: String = ">"
  val Comma: Char            = ','
  val CommaStr: String       = ","
  val Space: Char            = ' '
  val Tab: Char              = '\t'
  val FormFeed: Char         = 12
  val LowerA                 = 'a'
  val UpperA                 = 'A'
  val LowerZ                 = 'z'
  val UpperZ                 = 'Z'
  val Dollar                 = '$'
  val Underscore             = '_'
  val Zero                   = '0'
  val Nine                   = '9'
  val ForwardSlash           = '/'
  val BackSlash              = '\\'
  val Asterisk               = '*'
  val SingleQuote            = '\''
  val Semicolon              = ';'

  val AtSignStr    = "@"
  val DotStr       = "."
  val SemicolonStr = ";"

  val ClassStr     = "class"
  val EnumStr      = "enum"
  val InterfaceStr = "interface"

  val GlobalStr     = "global"
  val PublicStr     = "public"
  val ProtectedStr  = "protected"
  val PrivateStr    = "private"
  val TransientStr  = "transient"
  val AbstractStr   = "abstract"
  val FinalStr      = "final"
  val WebserviceStr = "webservice"
  val OverrideStr   = "override"
  val VirtualStr    = "virtual"
  val TestMethodStr = "testmethod"
  val WithStr       = "with"
  val WithoutStr    = "without"
  val InheritedStr  = "inherited"
  val SharingStr    = "sharing"
  val BooleanStr    = "boolean"
  val StaticStr     = "static"
  val VoidStr       = "void"
  val StringStr     = "string"
  val DecimalStr    = "decimal"
  val DateStr       = "date"
  val IntegerStr    = "date"
  val EqualsStr     = "="
  val ExtendsStr    = "extends"
  val ImplementsStr = "implements"

  def isIdChar(c: Char): Boolean = {
    (c >= Tokens.LowerA && c <= Tokens.LowerZ) ||
    (c >= Tokens.UpperA && c <= Tokens.UpperZ) ||
    (c >= Tokens.Zero && c <= Tokens.Nine) ||
    (c == Tokens.Dollar) ||
    (c == Tokens.Underscore)
  }
}

sealed trait Token {
  def contents: String
  def location: Location

  def lowerCaseContents: String = contents.toLowerCase

  def matches(other: String): Boolean = {
    contents.equalsIgnoreCase(other)
  }
}

/* An Id and its associated location, beware equality is defined only over the id. */
class LocatableIdToken private (override val name: String, _location: Location)
    extends IdLocationHolder(_location)
    with Token {

  override def contents: String = name

  override def toString: String = name

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[LocatableIdToken]
    lowerCaseContents.equals(other.lowerCaseContents)
  }

  override val hashCode: Int = lowerCaseContents.hashCode
}

object LocatableIdToken {
  private val stringCache = mutable.HashMap[String, String]()

  def apply(contents: String, location: Location): LocatableIdToken = {
    new LocatableIdToken(stringCache.getOrElseUpdate(contents, contents), location)
  }
}

case class NonIdToken(contents: String, location: Location) extends Token

final class Tokens {

  private val tokens = mutable.ArrayBuffer[Token]()

  def head: Token = {
    tokens.head
  }

  def get(index: Int): Token = {
    tokens(index)
  }

  def matches(index: Int, value: String): Boolean = {
    if (index < tokens.length)
      tokens(index).matches(value)
    else
      false
  }

  //noinspection IndexBoundsCheck
  def apply(index: Int): Option[Token] = {
    // Avoid lift() here, it's expensive
    if (index >= tokens.length) None else Some(tokens(index))
  }

  def length: Int = {
    tokens.length
  }

  def isEmpty: Boolean = {
    tokens.isEmpty
  }

  def clear(): Unit = {
    tokens.clear()
  }

  def append(token: Token): Unit = {
    tokens.append(token)
  }

  override def toString: String = {
    if (tokens.isEmpty) return ""
    val s = tokens(0).location
    s.toString
    val e = tokens.last.location
    s"[${s.startLine}.${s.startLineOffset} -> ${e.endLine}.${e.endLineOffset}] ${tokens.map(_.contents).mkString(" ")}"
  }

  def findIndex(f: Token => Boolean): Int = {
    findIndex(0, f)
  }

  def findIndex(startIndex: Int, f: Token => Boolean): Int = {
    var index = startIndex
    while (index < tokens.length) {
      if (f(tokens(index))) return index
      index += 1
    }
    -1
  }

  def hasToken(ofInterest: Set[String]): Option[Token] = {
    tokens.find(t => ofInterest.contains(t.lowerCaseContents))
  }

  def last(): Token = {
    tokens.last
  }

}
