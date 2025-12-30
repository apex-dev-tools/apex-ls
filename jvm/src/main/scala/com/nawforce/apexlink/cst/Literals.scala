/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.org.Referenceable
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLocation
import com.nawforce.runtime.parsers.CodeParser
import io.github.apexdevtools.apexparser.ApexParser.LiteralContext

sealed abstract class Literal {
  def getType: TypeDeclaration

  def verify(location: PathLocation, context: ExpressionVerifyContext): Unit = {}
}

case object IntegerLiteral extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.integerType
}

case class OversizeIntegerLiteral(location: PathLocation) extends Literal {

  override def getType: TypeDeclaration = PlatformTypes.integerType

  override def verify(location: PathLocation, context: ExpressionVerifyContext): Unit = {
    context.logError(location, "Integer literals can only be up to 10 characters")
  }
}

case object LongLiteral extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.longType
}

case class OversizeLongLiteral(location: PathLocation) extends Literal {

  override def getType: TypeDeclaration = PlatformTypes.longType

  override def verify(location: PathLocation, context: ExpressionVerifyContext): Unit = {
    context.logError(location, "Long literals can only be up to 19 characters")
  }
}

case object DoubleLiteral extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.doubleType
}

case object DecimalLiteral extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.decimalType
}

case class OversizeDecimalLiteral(location: PathLocation) extends Literal {

  override def getType: TypeDeclaration = PlatformTypes.decimalType

  override def verify(location: PathLocation, context: ExpressionVerifyContext): Unit = {
    context.logError(location, "Decimal literals can only be up to 50 characters")
  }
}

case object StringLiteral extends Literal {
  private val boundMatch  = ":\\s*[0-9a-zA-Z_]*".r
  private val boundPrefix = ":\\s*".r

  override def getType: TypeDeclaration = PlatformTypes.stringType

  def apply(value: String): Literal = {
    val bound = boundMatch
      .findAllIn(value)
      .map(v => Name(boundPrefix.replaceFirstIn(v, "")))
      .toSet
    if (bound.nonEmpty)
      BoundStringLiteral(bound)
    else
      StringLiteral
  }
}

case class InvalidStringLiteral(location: PathLocation) extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.stringType

  override def verify(location: PathLocation, context: ExpressionVerifyContext): Unit = {
    context.logError(location, "Invalid escape sequence in string literal")
  }
}

final case class BoundStringLiteral(bound: Set[Name]) extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.stringType

  override def verify(location: PathLocation, context: ExpressionVerifyContext): Unit = {
    bound.foreach(bound => {
      if (context.isVar(bound, markUsed = true).isEmpty) {
        context.thisType
          .findField(bound, None)
          .map(field => {
            Referenceable
              .addReferencingLocation(context.thisType, field, location, context.thisType)
            context.addDependency(field)
          })
      }
    })
  }
}

case object BooleanLiteral extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.booleanType
}

case object NullLiteral extends Literal {
  override def getType: TypeDeclaration = PlatformTypes.nullType
}

object IntegerOrLongLiteral {
  def apply(context: LiteralContext): Literal = {
    val value = CodeParser.getText(context)
    if (value.last.toLower == 'l') {
      if (value.length > 20) // 19 + 1 for 'l'
        OversizeLongLiteral(CST.sourceContext.value.get.getLocation(context))
      else
        LongLiteral
    } else {
      if (value.length > 10)
        OversizeIntegerLiteral(CST.sourceContext.value.get.getLocation(context))
      else
        IntegerLiteral
    }
  }
}

object DoubleOrDecimalLiteral {
  def apply(context: LiteralContext): Literal = {
    val value = CodeParser.getText(context)
    if (value.last.toLower == 'd') {
      DoubleLiteral
    } else if (value.length > 50) {
      OversizeDecimalLiteral(CST.sourceContext.value.get.getLocation(context))
    } else {
      DecimalLiteral
    }
  }
}

object Literal {
  private val validEscapes = Set('\\', '\'', '"', 'n', 'r', 't', 'b', 'f', 'u')

  private def hasInvalidEscape(source: String): Boolean = {
    var i = 0
    while (i < source.length) {
      if (source(i) == '\\') {
        if (i + 1 >= source.length) {
          return true
        }
        val next = source(i + 1)
        if (next == 'u') {
          if (i + 5 >= source.length) {
            return true
          }
          val hexStart = i + 2
          val hexEnd   = hexStart + 4
          if (
            !source
              .substring(hexStart, hexEnd)
              .forall(c =>
                (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
              )
          ) {
            return true
          }
          i = hexEnd
        } else if (!validEscapes.contains(next)) {
          return true
        } else {
          i += 2
        }
      } else {
        i += 1
      }
    }
    false
  }

  def construct(from: LiteralContext): Literal = {
    CodeParser
      .toScala(from.IntegerLiteral())
      .map(_ => IntegerOrLongLiteral(from))
      .orElse(
        CodeParser
          .toScala(from.LongLiteral())
          .map(_ => IntegerOrLongLiteral(from))
      )
      .orElse(
        CodeParser
          .toScala(from.NumberLiteral())
          .map(_ => DoubleOrDecimalLiteral(from))
      )
      .orElse(
        CodeParser
          .toScala(from.StringLiteral())
          .map(x => {
            val source = CST.sourceContext.value.get.extractSource(from)
            val text   = source.code.asString
            val content =
              if (text.length >= 2 && text(0) == '\'' && text(text.length - 1) == '\'') {
                text.substring(1, text.length - 1)
              } else {
                text
              }
            if (hasInvalidEscape(content)) {
              InvalidStringLiteral(CST.sourceContext.value.get.getLocation(from))
            } else {
              StringLiteral(CodeParser.getText(x))
            }
          })
      )
      .orElse(CodeParser.toScala(from.BooleanLiteral()).map(_ => BooleanLiteral))
      .getOrElse(NullLiteral)
  }
}
