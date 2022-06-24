/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import com.financialforce.oparser.OutlineParser.singleCharacterTokens
import com.financialforce.types.base.{Location, Position, PropertyBlock}

import scala.annotation.tailrec
import scala.collection.{BitSet, mutable}

sealed abstract class TypeNature(val value: String)

case object CLASS_NATURE     extends TypeNature("class")
case object INTERFACE_NATURE extends TypeNature("interface")
case object ENUM_NATURE      extends TypeNature("enum")

trait TypeDeclFactory[TypeDecl <: IMutableTypeDeclaration, Ctx] {
  def create(ctx: Ctx, nature: TypeNature, path: String, enclosing: Option[TypeDecl]): TypeDecl
}

final class OutlineParser[TypeDecl <: IMutableTypeDeclaration, Ctx](
  path: String,
  contents: String,
  factory: TypeDeclFactory[TypeDecl, Ctx],
  ctx: Ctx
) {

  private var charOffset                        = 0
  private var byteOffset                        = 0
  private var currentChar: Char                 = 0
  private var line                              = 0
  private var lineOffset                        = 0
  private val length                            = contents.length
  private var finished                          = false
  private var typeDeclaration: Option[TypeDecl] = None

  private val buffer = new Buffer(contents)
  private val tokens = new Tokens()

  def parse(): (Boolean, Option[String], Option[TypeDecl]) = {
    if (contents.isEmpty) return (false, Some("Empty file"), None)
    reset()
    try {
      parseTypeDeclaration()
      (true, None, typeDeclaration)
    } catch {
      case ex: Throwable => (false, Some(ex.getMessage), typeDeclaration)
    }
  }

  private def parseHelper[T, R](
    discardCommentsAndWhitespace: Boolean,
    provider: () => T,
    f: T => (Boolean, Option[R])
  ): Option[R] = {

    @tailrec
    def loop(): Option[R] = {
      val currentOffset = charOffset
      if (discardCommentsAndWhitespace)
        consumeCommentOrWhitespace()
      val (continue, res) = f(provider())
      if (!continue) res
      else if (atEnd())
        throw new Exception("End of file")
      else if (currentOffset == charOffset)
        throw new Exception("No Progress")
      else
        loop()
    }

    loop()
  }

  private def statefulParseHelper[T, S, R](
    initialState: S,
    provider: () => T,
    f: (T, S) => (Boolean, S, Option[R])
  ): Option[R] = {

    @tailrec
    def loop(state: S): Option[R] = {
      val currentOffset = charOffset
      consumeCommentOrWhitespace()
      val (continue, newState, res) = f(provider(), state)
      if (!continue) res
      else if (atEnd())
        throw new Exception("End of file")
      else if (currentOffset == charOffset)
        throw new Exception("No Progress")
      else
        loop(newState)
    }

    loop(initialState)
  }

  private def tokenParseHelper[R](
    discardCommentsAndWhitespace: Boolean,
    f: Option[Token] => (Boolean, Option[R])
  ): Option[R] = {
    parseHelper[Option[Token], R](discardCommentsAndWhitespace, () => consumeToken(), f)
  }

  private def tokenParseHelper[R, S](
    initialState: S,
    f: (Option[Token], S) => (Boolean, S, Option[R])
  ): Option[R] = {
    statefulParseHelper[Option[Token], S, R](initialState, () => consumeToken(), f)
  }

  private def parseTypeDeclaration(): Option[TypeDecl] = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: NonIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: LocatableIdToken) =>
          t.lowerCaseContents match {
            case Tokens.ClassStr =>
              tokens.append(t); (false, consumeClassDeclaration())
            case Tokens.InterfaceStr =>
              tokens.append(t); (false, consumeInterfaceDeclaration())
            case Tokens.EnumStr =>
              tokens.append(t); (false, consumeEnumDeclaration())
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeClassDeclaration(): Option[TypeDecl] = {

    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: LocatableIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LBraceStr =>
              val classTypeDeclaration = factory.create(ctx, CLASS_NATURE, path, None)
              Parse.parseClassType(classTypeDeclaration, tokens)
              typeDeclaration = Some(classTypeDeclaration)
              val startPosition = tokens.head.location.startPosition
              tokens.clear()
              consumeClassBody(classTypeDeclaration)
              classTypeDeclaration.setLocation(
                Location(startPosition, Position(line, lineOffset, byteOffset))
              )
              classTypeDeclaration.onComplete()
              (false, Some(classTypeDeclaration))
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeClassBody(classTypeDeclaration: TypeDecl): Unit = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: LocatableIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LParenStr =>
              tokens.append(t); collectParenthesisFragment(); (true, None)
            case Tokens.LBrackStr =>
              tokens.append(t); collectSquareBracketFragment(); (true, None)
            case Tokens.SemicolonStr =>
              if (!tokens.isEmpty) {
                Parse.parseClassMember(classTypeDeclaration, tokens, t) match {
                  case (_, cms) =>
                    cms.foreach(
                      _.setLocation(tokens.head.location.startPosition, t.location.endPosition)
                    )
                }
              }
              tokens.clear()
              (true, None)
            case Tokens.LBraceStr =>
              val innerType = Parse.getInnerType(tokens)
              innerType match {
                case None =>
                  consumeClassMember(classTypeDeclaration, t)
                case Some(t) =>
                  consumeInnerType(classTypeDeclaration, t)
              }
              (true, None)
            case Tokens.RBraceStr =>
              if (!tokens.isEmpty)
                throw new Exception("Unexpected '}'")
              (false, None)
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeClassMember(classTypeDeclaration: TypeDecl, t: Token): Unit = {
    Parse.parseClassMember(classTypeDeclaration, tokens, t) match {
      case (true, classMembers) if classMembers.nonEmpty =>
        val startBlockPosition = Position(line, lineOffset, byteOffset)
        val startPosition      = tokens(0).map(_.location.startPosition).getOrElse(startBlockPosition)

        tokens.clear()
        if (classMembers.length == 1 && classMembers.head.isInstanceOf[PropertyDeclaration])
          classMembers.head.asInstanceOf[PropertyDeclaration].propertyBlocks =
            consumePropertyDeclaration()
        else
          consumeBlock()
        classMembers.foreach(
          m =>
            m.setLocations(
              startPosition,
              startBlockPosition,
              Position(line, lineOffset, byteOffset)
            )
        )
      case (true, _) =>
        tokens.clear()
        consumeBlock()
      case (finished, _) if !finished =>
        consumeBlock()
    }
  }

  private def consumePropertyDeclaration(): Array[PropertyBlock] = {
    val propertyBlocks = mutable.ArrayBuffer[PropertyBlock]()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: LocatableIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.RBraceStr => (false, None)
            case Tokens.LBraceStr =>
              val start = tokens.head.location.startPosition
              tokens.clear()
              consumeBlock()
              val end = Position(line, lineOffset, byteOffset)
              propertyBlocks.append(PropertyBlock(Location(start, end)))
              (true, None)
            case Tokens.SemicolonStr =>
              val start = tokens.head.location.startPosition
              val end   = Position(line, lineOffset, byteOffset)
              propertyBlocks.append(PropertyBlock(Location(start, end)))
              tokens.clear()
              (true, None)
          }
      }
    )
    propertyBlocks.toArray
  }

  private def consumeInnerType(ctd: TypeDecl, token: Token): Unit = {
    if (token.matches(Tokens.ClassStr)) {
      val innerClass = factory.create(ctx, CLASS_NATURE, ctd.paths.head, Some(ctd))
      Parse.parseClassType(innerClass, tokens)
      val startPosition = token.location.startPosition
      tokens.clear()
      consumeClassBody(innerClass)
      innerClass.setLocation(Location(startPosition, Position(line, lineOffset, byteOffset)))
      innerClass.onComplete()
      ctd.appendInnerType(innerClass)
    } else if (token.matches(Tokens.InterfaceStr)) {
      val innerInterface = factory.create(ctx, INTERFACE_NATURE, ctd.paths.head, Some(ctd))
      Parse.parseInterfaceType(innerInterface, tokens)
      val startPosition = token.location.startPosition
      tokens.clear()
      consumeInterfaceBody(innerInterface)
      innerInterface.setLocation(Location(startPosition, Position(line, lineOffset, byteOffset)))
      innerInterface.onComplete()
      ctd.appendInnerType(innerInterface)
    } else if (token.matches(Tokens.EnumStr)) {
      val innerEnum = factory.create(ctx, ENUM_NATURE, ctd.paths.head, Some(ctd))
      Parse.parseEnumType(innerEnum, tokens)
      val startPosition = token.location.startPosition
      tokens.clear()
      consumeEnumBody(innerEnum)
      innerEnum.setLocation(Location(startPosition, Position(line, lineOffset, byteOffset)))
      innerEnum.onComplete()
      ctd.appendInnerType(innerEnum)
    }
  }

  private def collectParenthesisFragment(): Unit = {

    tokenParseHelper(
      1: Int,
      (token: Option[Token], nestedParenthesis: Int) => {
        token match {
          case None => (false, nestedParenthesis, None)
          case Some(t: LocatableIdToken) =>
            tokens.append(t); (true, nestedParenthesis, None)
          case Some(t: NonIdToken) =>
            t.lowerCaseContents match {
              case Tokens.LParenStr =>
                tokens.append(t); (true, nestedParenthesis + 1, None)
              case Tokens.RParenStr =>
                tokens.append(t); (nestedParenthesis > 1, nestedParenthesis - 1, None)
              case _ =>
                tokens.append(t); (true, nestedParenthesis, None)
            }
        }
      }
    )
  }

  private def collectSquareBracketFragment(): Unit = {

    tokenParseHelper(
      1: Int,
      (token: Option[Token], nestedSquareBrackets: Int) => {
        token match {
          case None => (false, nestedSquareBrackets, None)
          case Some(t: LocatableIdToken) =>
            tokens.append(t); (true, nestedSquareBrackets, None)
          case Some(t: NonIdToken) =>
            t.lowerCaseContents match {
              case Tokens.LBrackStr =>
                tokens.append(t); (true, nestedSquareBrackets + 1, None)
              case Tokens.RBrackStr =>
                tokens.append(t); (nestedSquareBrackets > 1, nestedSquareBrackets - 1, None)
              case _ =>
                tokens.append(t); (true, nestedSquareBrackets, None)
            }
        }
      }
    )
  }

  private def consumeBlock(): Unit = {
    var continue = true
    var indent   = 1

    while (continue) {
      val currentOffset = charOffset

      currentChar match {
        case Tokens.ForwardSlash =>
          if (!consumeComment())
            consumeCharacter(capture = false)
        case Tokens.SingleQuote =>
          consumeStringLiteral()
        case Tokens.Newline | Tokens.CarriageReturn =>
          consumeNewline()
        case Tokens.LBrace =>
          consumeCharacter(capture = false)
          indent += 1
        case Tokens.RBrace =>
          consumeCharacter(capture = false)
          continue = indent > 1
          indent -= 1
        case _ =>
          consumeCharacter(capture = false)
      }

      if (continue) {
        if (atEnd())
          throw new Exception("End of file")
        else if (currentOffset == charOffset)
          throw new Exception("No Progress")
      }
    }
  }

  private def consumeInterfaceDeclaration(): Option[TypeDecl] = {

    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: LocatableIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LBraceStr =>
              val interfaceTypeDeclaration = factory.create(ctx, INTERFACE_NATURE, path, None)
              Parse.parseInterfaceType(interfaceTypeDeclaration, tokens)
              typeDeclaration = Some(interfaceTypeDeclaration)
              val startPosition = tokens.head.location.startPosition
              tokens.clear()
              consumeInterfaceBody(interfaceTypeDeclaration)
              interfaceTypeDeclaration.setLocation(
                Location(startPosition, Position(line, lineOffset, byteOffset))
              )
              interfaceTypeDeclaration.onComplete()
              (false, Some(interfaceTypeDeclaration))
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeInterfaceBody(interfaceTypeDeclaration: TypeDecl): Unit = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: LocatableIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LParenStr =>
              tokens.append(t); collectParenthesisFragment(); (true, None)
            case Tokens.SemicolonStr =>
              Parse
                .parseInterfaceMember(interfaceTypeDeclaration, tokens)
                .foreach(
                  _.setLocation(
                    tokens.head.location.startPosition,
                    Position(line, lineOffset, byteOffset)
                  )
                )
              tokens.clear()
              (true, None)
            case Tokens.RBraceStr =>
              (false, None)
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeEnumDeclaration(): Option[TypeDecl] = {
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: LocatableIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LBraceStr =>
              val enumTypeDeclaration = factory.create(ctx, ENUM_NATURE, path, None)
              Parse.parseEnumType(enumTypeDeclaration, tokens)
              typeDeclaration = Some(enumTypeDeclaration)
              val startPosition = tokens.head.location.startPosition
              tokens.clear()
              consumeEnumBody(enumTypeDeclaration)
              enumTypeDeclaration.setLocation(
                Location(startPosition, Position(line, lineOffset, byteOffset))
              )
              enumTypeDeclaration.onComplete()
              (false, Some(enumTypeDeclaration))
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeEnumBody(enumTypeDeclaration: TypeDecl): Unit = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: LocatableIdToken) =>
          tokens.append(t)
          Parse.parseEnumMember(enumTypeDeclaration, tokens)
          tokens.clear()
          (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.CommaStr =>
              tokens.clear(); (true, None)
            case Tokens.RBraceStr =>
              tokens.clear(); (false, None)
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def atEnd(): Boolean = {
    finished
  }

  private def reset(): Unit = {
    finished = false
    charOffset = 0
    byteOffset = 0
    lineOffset = 1
    currentChar = contents(charOffset)
    line = 1
    buffer.clear()
    tokens.clear()
  }

  private def peekMatch(peek: Char): Boolean = {
    charOffset + 1 < length && contents(charOffset + 1) == peek
  }

  private val byteBuffer = new mutable.StringBuilder(4)
  private def consumeCharacter(capture: Boolean = true): Unit = {
    if (capture)
      buffer.append(byteOffset, charOffset, line, lineOffset)

    if (charOffset + 1 == length)
      finished = true
    else {
      charOffset += 1
      currentChar = contents(charOffset)

      val byteLength =
        if (currentChar.toInt < 128) 1
        else {
          byteBuffer.setLength(0)
          byteBuffer
            .append(currentChar)
            .toString()
            .getBytes("UTF-8")
            .length
        }

      byteOffset += byteLength
      lineOffset += 1
    }
  }

  private def isAtComment: Boolean = {
    currentChar == Tokens.ForwardSlash &&
    (peekMatch(Tokens.Asterisk) || peekMatch(Tokens.ForwardSlash))
  }

  private def consumeComment(): Boolean = {

    def consumeSingleLineComment(): Unit = {
      @tailrec
      def loop(): Unit = {
        if (!atEnd()) {
          currentChar match {
            case Tokens.Newline | Tokens.CarriageReturn => if (!consumeNewline()) loop()
            case _ =>
              consumeCharacter(false)
              loop()
          }
        }
      }

      loop()
    }

    def consumeMultilineComment(): Unit = {
      @tailrec
      def loop(): Unit = {
        if (!atEnd()) {
          currentChar match {
            case Tokens.Asterisk =>
              consumeCharacter(false)
              if (currentChar == Tokens.ForwardSlash) {
                consumeCharacter(false)
              } else {
                loop()
              }
            case Tokens.Newline | Tokens.CarriageReturn =>
              consumeNewline()
              loop()
            case _ =>
              consumeCharacter(false)
              loop()
          }
        }
      }

      loop()
    }

    // Current Char is expected to be ForwardSlash
    if (peekMatch(Tokens.ForwardSlash)) {
      buffer.clear()
      consumeCharacter(false)
      consumeCharacter(false)
      consumeSingleLineComment()
      buffer.clear()
      true
    } else if (peekMatch(Tokens.Asterisk)) {
      buffer.clear()
      consumeCharacter(false)
      consumeCharacter(false)
      consumeMultilineComment()
      buffer.clear()
      true
    } else {
      false
    }
  }

  private def consumeNewline(): Boolean = {
    if (currentChar == Tokens.CarriageReturn && peekMatch(Tokens.Newline)) {
      consumeCharacter(false)
      consumeCharacter(false)
      line += 1
      lineOffset = 1
      true
    } else if (currentChar == Tokens.Newline) {
      consumeCharacter(false)
      line += 1
      lineOffset = 1
      true
    } else {
      consumeCharacter(false)
      false
    }
  }

  private def consumeCommentOrWhitespace(): Unit = {
    @tailrec
    def loop(): Unit = {
      if (!atEnd()) {
        currentChar match {
          case Tokens.ForwardSlash => if (consumeComment()) loop()
          case Tokens.Space | Tokens.Tab | Tokens.FormFeed | Tokens.Newline |
              Tokens.CarriageReturn =>
            consumeWhitespace()
            loop()
          case _ =>
        }
      }
    }

    loop()
  }

  private def isWhiteSpace(c: Char): Boolean = {
    c == Tokens.Space || c == Tokens.Tab || c == Tokens.FormFeed || c == Tokens.CarriageReturn || c == Tokens.Newline
  }

  private def consumeWhitespace(): Unit = {

    @tailrec
    def loop(): Unit = {
      if (!atEnd()) {
        currentChar match {
          case Tokens.Space | Tokens.Tab | Tokens.FormFeed =>
            consumeCharacter(false)
            loop()
          case Tokens.CarriageReturn | Tokens.Newline =>
            consumeNewline()
            loop()
          case _ =>
        }
      }
    }

    loop()
  }

  private def consumeStringLiteral(capture: Boolean = false): Unit = {
    buffer.clear()
    consumeCharacter(capture)

    var continue = true
    while (continue) {
      val currentOffset = charOffset

      currentChar match {
        case Tokens.SingleQuote =>
          consumeCharacter(capture)
          continue = false
        case Tokens.BackSlash =>
          consumeCharacter(capture)
          if (currentChar == Tokens.SingleQuote || currentChar == Tokens.BackSlash) {
            consumeCharacter(capture)
          }
        case _ =>
          consumeCharacter(capture);
      }

      if (continue) {
        if (atEnd()) {
          throw new Exception("End of file")
        } else if (currentOffset == charOffset)
          throw new Exception("No Progress")
      }
    }
  }

  /**
    * Read a token and return it
    *
    * @return The token read
    */
  private def consumeToken(): Option[Token] = {

    def consumeIdToken(): LocatableIdToken = {
      buffer.clear()
      while (!atEnd() && Tokens.isIdChar(currentChar)) {
        consumeCharacter()
      }
      val b = buffer.captured()
      LocatableIdToken(b._1, b._2)
    }

    def consumeNonIdToken(): NonIdToken = {
      buffer.clear()
      val token = if (currentChar == Tokens.SingleQuote) {
        consumeStringLiteral(capture = true)
        val b = buffer.captured()
        NonIdToken(b._1, b._2)
      } else if (singleCharacterTokens.contains(currentChar)) {
        consumeCharacter()
        val b = buffer.captured()
        NonIdToken(b._1, b._2)
      } else {
        while (
          !atEnd() &&
          !Tokens.isIdChar(currentChar) &&
          !isWhiteSpace(currentChar) &&
          !isAtComment &&
          currentChar != Tokens.SingleQuote &&
          currentChar != Tokens.Semicolon
        ) {
          consumeCharacter()
        }
        val b = buffer.captured()
        NonIdToken(b._1, b._2)
      }
      token
    }

    consumeCommentOrWhitespace()
    if (atEnd())
      None
    else if (Tokens.isIdChar(currentChar))
      Some(consumeIdToken())
    else
      Some(consumeNonIdToken())
  }
}

object OutlineParser {
  val singleCharacterTokens: BitSet = {
    BitSet(
      Tokens.Semicolon,
      Tokens.Comma,
      Tokens.LParen,
      Tokens.RParen,
      Tokens.LessThan,
      Tokens.GreaterThan,
      Tokens.LBrace,
      Tokens.RBrace,
      Tokens.LBrack,
      Tokens.RBrack
    )
  }

  def parse[TypeDecl <: IMutableTypeDeclaration, Ctx](
    path: String,
    contents: String,
    factory: TypeDeclFactory[TypeDecl, Ctx],
    ctx: Ctx
  ): (Boolean, Option[String], Option[TypeDecl]) = {
    new OutlineParser(path, contents, factory, ctx).parse()
  }
}
