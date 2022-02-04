/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.annotation.tailrec

class OutlineParser(path: String, contents: String) {

  private var charOffset = 0
  private var byteOffset = 0
  private var currentChar: Char = 0
  private var line = 0
  private var lineOffset = 0
  private val length = contents.length
  private var finished = false
  private var typeDeclaration: Option[TypeDeclaration] = None

  private val buffer = new Buffer(contents)
  private val tokens = new Tokens()

  def parse(): (Boolean, Option[String], Option[TypeDeclaration]) = {
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
    f: (T) => (Boolean, Option[R])
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
    discardCommentsAndWhitespace: Boolean,
    initialState: S,
    provider: () => T,
    f: (T, S) => (Boolean, S, Option[R])
  ): Option[R] = {

    @tailrec
    def loop(state: S): Option[R] = {
      val currentOffset = charOffset
      if (discardCommentsAndWhitespace)
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

  private def characterParseHelper[R](
    discardCommentsAndWhitespace: Boolean,
    f: (Char) => (Boolean, Option[R])
  ): Option[R] = {
    parseHelper[Char, R](discardCommentsAndWhitespace, () => currentChar, f)
  }

  private def characterParseHelper[R, S](
    discardCommentsAndWhitespace: Boolean,
    initialState: S,
    f: (Char, S) => (Boolean, S, Option[R])
  ): Option[R] = {
    statefulParseHelper[Char, S, R](
      discardCommentsAndWhitespace,
      initialState,
      () => currentChar,
      f
    )
  }

  private def tokenParseHelper[R](
    discardCommentsAndWhitespace: Boolean,
    f: (Option[Token]) => (Boolean, Option[R])
  ): Option[R] = {
    parseHelper[Option[Token], R](discardCommentsAndWhitespace, () => consumeToken(), f)
  }

  private def tokenParseHelper[R, S](
    discardCommentsAndWhitespace: Boolean,
    initialState: S,
    f: (Option[Token], S) => (Boolean, S, Option[R])
  ): Option[R] = {
    statefulParseHelper[Option[Token], S, R](
      discardCommentsAndWhitespace,
      initialState,
      () => consumeToken(),
      f
    )
  }

  private def parseTypeDeclaration(): Option[TypeDeclaration] = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: NonIdToken) =>
          tokens.append(t); (true, None)
        case Some(t: IdToken) =>
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

  private def consumeClassDeclaration(): Option[ClassTypeDeclaration] = {

    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: IdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LBraceStr =>
              val classTypeDeclaration = Parse.parseClassType(tokens, path)
              typeDeclaration = Some(classTypeDeclaration)
              val startLocation = Location.fromStart(tokens(0).get.location)
              tokens.clear()
              consumeClassBody(classTypeDeclaration)
              classTypeDeclaration.location =
                Some(Location.updateEnd(startLocation, line, lineOffset, byteOffset))
              (false, Some(classTypeDeclaration))
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeClassBody(classTypeDeclaration: ClassTypeDeclaration): Unit = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: IdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LParenStr =>
              tokens.append(t); collectParenthesisFragment(); (true, None)
            case Tokens.LBrackStr =>
              tokens.append(t); collectSquareBracketFragment(); (true, None)
            case Tokens.SemicolonStr =>
              if (!tokens.isEmpty()) {
                Parse.parseClassMember(classTypeDeclaration, tokens, t) match {
                  case (_, cms) =>
                    if (cms.nonEmpty) {
                      val location = Location.from(tokens(0).get.location, t.location)
                      cms.foreach(m => m.location = Some(location))
                    }
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
              (false, None)
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeClassMember(classTypeDeclaration: ClassTypeDeclaration, t: Token): Unit = {
    Parse.parseClassMember(classTypeDeclaration, tokens, t) match {
      case (finished, cms) if finished =>
        val startBlockLocation = Location(line, lineOffset, byteOffset, 0, 0, 0)
        val startLocation = {
          if (cms.nonEmpty)
            Some(
              Location
                .fromStart(tokens(0).map(_.location).getOrElse(startBlockLocation))
            )
          else None
        }
        tokens.clear()
        if (cms.length == 1 && cms.head.isInstanceOf[PropertyDeclaration])
          consumePropertyDeclaration(cms.head.asInstanceOf[PropertyDeclaration])
        else
          consumeBlock()
        if (cms.nonEmpty) {
          val location =
            Location.updateEnd(startLocation.get, line, lineOffset, byteOffset)
          val blockLocation =
            Location.updateEnd(startBlockLocation, line, lineOffset, byteOffset)
          cms.foreach(m => m.location = Some(location))
          cms.foreach(m => m.blockLocation = Some(blockLocation))
        }
      case (finished, _) if !finished =>
        consumeBlock()
    }
  }

  private def consumePropertyDeclaration(pd: PropertyDeclaration): Unit = {
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: IdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.RBraceStr => (false, None)
            case Tokens.LBraceStr =>
              val pb = Parse.parsePropertyBlock(pd, tokens)
              if (pb.isDefined) {
                pb.get.blockLocation = Some(tokens(0).get.location)
              }
              tokens.clear()
              consumeBlock()
              if (pb.isDefined) {
                pb.get.blockLocation =
                  Some(Location.updateEnd(pb.get.blockLocation.get, line, lineOffset, byteOffset))
              }
              (true, None)
            case Tokens.SemicolonStr =>
              val pb = Parse.parsePropertyBlock(pd, tokens)
              if (pb.isDefined) {
                pb.get.blockLocation =
                  Some(Location.updateEnd(tokens(0).get.location, line, lineOffset, byteOffset))
              }
              tokens.clear()
              (true, None)
          }
      }
    )
  }

  private def consumeInnerType(ctd: ClassTypeDeclaration, token: Token): Unit = {
    if (token.matches(Tokens.ClassStr)) {
      val innerClass = Parse.parseClassType(tokens, ctd.path)
      val startLocation = Location.fromStart(token.location)
      tokens.clear()
      consumeClassBody(innerClass)
      innerClass.location = Some(Location.updateEnd(startLocation, line, lineOffset, byteOffset))
      ctd.innerTypes.append(innerClass)
    } else if (token.matches(Tokens.InterfaceStr)) {
      val innerInterface = Parse.parseInterfaceType(tokens, ctd.path)
      val startLocation = Location.fromStart(token.location)
      tokens.clear()
      consumeInterfaceBody(innerInterface)
      innerInterface.location = Some(
        Location.updateEnd(startLocation, line, lineOffset, byteOffset)
      )
      ctd.innerTypes.append(innerInterface)
    } else if (token.matches(Tokens.EnumStr)) {
      val innerEnum = Parse.parseEnumType(tokens, ctd.path)
      val startLocation = Location.fromStart(token.location)
      tokens.clear()
      consumeEnumBody(innerEnum)
      innerEnum.location = Some(Location.updateEnd(startLocation, line, lineOffset, byteOffset))
      ctd.innerTypes.append(innerEnum)
    }
  }

  private def collectParenthesisFragment(): Unit = {

    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      1: Int,
      (token: Option[Token], nestedParenthesis: Int) => {
        token match {
          case None => (false, nestedParenthesis, None)
          case Some(t: IdToken) =>
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
      discardCommentsAndWhitespace = true,
      1: Int,
      (token: Option[Token], nestedSquareBrackets: Int) => {
        token match {
          case None => (false, nestedSquareBrackets, None)
          case Some(t: IdToken) =>
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
    // opening brace consumed
    characterParseHelper(
      discardCommentsAndWhitespace = true,
      1: Int,
      (char: Char, nesting: Int) => {
        char match {
          case Tokens.SingleQuote =>
            consumeStringLiteral(); (true, nesting, None)
          case Tokens.LBrace =>
            consumeCharacter(false); (true, nesting + 1, None)
          case Tokens.RBrace =>
            consumeCharacter(false); (nesting > 1, nesting - 1, None)
          case _ =>
            consumeCharacter(false); (true, nesting, None)
        }
      }
    )
  }

  private def consumeInterfaceDeclaration(): Option[InterfaceTypeDeclaration] = {

    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: IdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LBraceStr =>
              val interfaceTypeDeclaration = Parse.parseInterfaceType(tokens, path)
              typeDeclaration = Some(interfaceTypeDeclaration)
              val startLocation = Location.fromStart(tokens(0).get.location)
              tokens.clear()
              consumeInterfaceBody(interfaceTypeDeclaration)
              interfaceTypeDeclaration.location =
                Some(Location.updateEnd(startLocation, line, lineOffset, byteOffset))
              (false, Some(interfaceTypeDeclaration))
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeInterfaceBody(interfaceTypeDeclaration: InterfaceTypeDeclaration): Unit = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: IdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LParenStr =>
              tokens.append(t); collectParenthesisFragment(); (true, None)
            case Tokens.SemicolonStr =>
              val ims = Parse.parseInterfaceMember(interfaceTypeDeclaration, tokens)
              if (ims.nonEmpty) {
                val sl = tokens(0).get.location
                val location = Location.updateEnd(sl, line, lineOffset, byteOffset)
                ims.foreach(im => im.location = Some(location))
              }
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

  private def consumeEnumDeclaration(): Option[EnumTypeDeclaration] = {
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None => (false, None)
        case Some(t: IdToken) =>
          tokens.append(t); (true, None)
        case Some(t: NonIdToken) =>
          t.lowerCaseContents match {
            case Tokens.LBraceStr =>
              val enumTypeDeclaration = Parse.parseEnumType(tokens, path)
              typeDeclaration = Some(enumTypeDeclaration)
              val startLocation = Location.fromStart(tokens(0).get.location)
              tokens.clear()
              consumeEnumBody(enumTypeDeclaration)
              enumTypeDeclaration.location =
                Some(Location.updateEnd(startLocation, line, lineOffset, byteOffset))
              (false, Some(enumTypeDeclaration))
            case _ =>
              tokens.append(t); (true, None)
          }
      }
    )
  }

  private def consumeEnumBody(enumTypeDeclaration: EnumTypeDeclaration): Unit = {
    tokens.clear()
    tokenParseHelper(
      discardCommentsAndWhitespace = true,
      {
        case None =>
          throw new Exception("End of file")
        case Some(t: IdToken) =>
          tokens.append(t); Parse.parseEnumMember(enumTypeDeclaration, tokens); tokens.clear()
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

  private val byteBuffer = new StringBuilder(4)
  private def consumeCharacter(capture: Boolean = true): Unit = {
    if (capture)
      buffer.append(byteOffset, charOffset, line, lineOffset)

    if (charOffset + 1 == length)
      finished = true
    else {
      charOffset += 1
      currentChar = contents(charOffset)
      byteBuffer.setLength(0)

      // Hmmm
      val byteLength =
        if (currentChar.toInt <= 128) 1
        else
          byteBuffer
            .append(currentChar)
            .toString()
            .getBytes("UTF-8")
            .length

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

    characterParseHelper(
      discardCommentsAndWhitespace = false,
      {
        case Tokens.SingleQuote =>
          consumeCharacter(capture); (false, None)
        case Tokens.BackSlash =>
          consumeCharacter(capture)
          if (currentChar == Tokens.SingleQuote || currentChar == Tokens.BackSlash) {
            consumeCharacter(capture)
          }
          (true, None)
        case _ =>
          consumeCharacter(capture); (true, None)
      }
    )
  }

  /**
    * Read a token and return it
    *
    * @return The token read
    */
  private def consumeToken(): Option[Token] = {

    def consumeIdToken(): IdToken = {
      buffer.clear()
      while (!atEnd() && Tokens.isIdChar(currentChar)) {
        consumeCharacter()
      }
      val b = buffer.captured()
      IdToken(b._1, b._2)
    }

    val singleCharacterTokens = Set(
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
  def parse(path: String, contents: String): (Boolean, Option[String], Option[TypeDeclaration]) = {
    new OutlineParser(path, contents).parse()
  }
}
