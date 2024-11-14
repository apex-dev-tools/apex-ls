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

package com.nawforce.apexlink.cst.stmts

import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.parsers.ENUM_NATURE
import com.nawforce.runtime.parsers.CodeParser
import io.github.apexdevtools.apexparser.ApexParser.{
  SwitchStatementContext,
  WhenControlContext,
  WhenLiteralContext,
  WhenValueContext
}

sealed abstract class WhenLiteral extends CST {
  def isComparableTo(typeName: TypeName): Boolean
}

final class WhenNullLiteral extends WhenLiteral {
  override def isComparableTo(typeName: TypeName): Boolean = true
}
final case class WhenIdLiteral(id: Id) extends WhenLiteral {
  override def isComparableTo(typeName: TypeName): Boolean = false // Not used
  override def toString: String                            = id.name.value.toLowerCase
}
final case class WhenStringLiteral(value: String) extends WhenLiteral {
  override def isComparableTo(typeName: TypeName): Boolean = typeName == TypeNames.String
  override def toString: String                            = value
}
final case class WhenIntegerLiteral(negate: Boolean, value: String) extends WhenLiteral {
  override def isComparableTo(typeName: TypeName): Boolean =
    typeName == TypeNames.Integer || typeName == TypeNames.Long
  override def toString: String = (if (negate) "-" else "") + value
}
final case class WhenLongLiteral(negate: Boolean, value: String) extends WhenLiteral {
  override def isComparableTo(typeName: TypeName): Boolean = typeName == TypeNames.Long
  override def toString: String                            = (if (negate) "-" else "") + value
}

object WhenLiteral {
  def construct(context: WhenLiteralContext): Option[WhenLiteral] = {
    val literal = flattenLiteral(context)
    CodeParser
      .toScala(literal.NULL())
      .map(_ => new WhenNullLiteral())
      .orElse(
        CodeParser
          .toScala(literal.IntegerLiteral())
          .map(l => {
            val negate = CodeParser.toScala(literal.SUB()).size % 2 != 0
            WhenIntegerLiteral(negate, CodeParser.getText(l))
          })
      )
      .orElse(
        CodeParser
          .toScala(literal.LongLiteral())
          .map(l => {
            val negate = CodeParser.toScala(literal.SUB()).size % 2 != 0
            val text   = CodeParser.getText(l)
            WhenLongLiteral(negate, text.substring(0, text.length - 1))
          })
      )
      .orElse(
        CodeParser
          .toScala(literal.StringLiteral())
          .map(l => WhenStringLiteral(CodeParser.getText(l)))
      )
      .orElse(
        CodeParser
          .toScala(literal.id())
          .map(l => WhenIdLiteral(Id.construct(l)))
      )
      .map(_.withContext(literal))
  }

  private def flattenLiteral(value: WhenLiteralContext): WhenLiteralContext = {
    // Remove nesting when a literal is wrapped in brackets
    CodeParser.toScala(value.whenLiteral()).map(flattenLiteral).getOrElse(value)
  }
}

sealed abstract class WhenValue extends CST {
  def checkMatchableTo(context: BlockVerifyContext, typeName: TypeName): Seq[String]
  def checkIsSObject(context: BlockVerifyContext): Seq[String]
  def checkEnumValue(context: BlockVerifyContext, typeDeclaration: TypeDeclaration): Seq[String]
  def verify(context: BlockVerifyContext): Unit = {}
}

final class WhenElseValue extends WhenValue {
  def checkMatchableTo(context: BlockVerifyContext, typeName: TypeName): Seq[String] = Seq()
  def checkIsSObject(context: BlockVerifyContext): Seq[String]                       = Seq()
  def checkEnumValue(context: BlockVerifyContext, typeDeclaration: TypeDeclaration): Seq[String] =
    Seq()
}

final case class WhenLiteralsValue(literals: Seq[WhenLiteral]) extends WhenValue {
  override def checkMatchableTo(context: BlockVerifyContext, typeName: TypeName): Seq[String] = {
    literals.flatMap(literal => {
      if (!literal.isComparableTo(typeName)) {
        context.logError(literal.location, s"A $typeName literal is required for this value")
        None
      } else {
        Some(literal.toString)
      }
    })
  }

  override def checkIsSObject(context: BlockVerifyContext): Seq[String] = {
    val nonNull = literals.filterNot(_.isInstanceOf[WhenNullLiteral])
    if (nonNull.nonEmpty)
      context.logError(
        nonNull.head.location,
        "An SObject name and variable name are required for this value"
      )
    Seq()
  }

  override def checkEnumValue(
    context: BlockVerifyContext,
    typeDeclaration: TypeDeclaration
  ): Seq[String] = {
    val nonNull = literals.filterNot(_.isInstanceOf[WhenNullLiteral])
    val notEnum = nonNull.filter(!_.isInstanceOf[WhenIdLiteral])
    if (notEnum.nonEmpty) {
      context.logError(notEnum.head.location, "An Enum value is required for this value")
      return Seq()
    }

    nonNull.foreach {
      case iv: WhenIdLiteral =>
        val field = typeDeclaration.findField(iv.id.name, Some(true))
        field.foreach(context.addDependency)
        if (field.isEmpty) {
          context.logError(iv.id.location, "Value must be a enum constant")
          return Seq()
        }
      case _ =>
    }
    nonNull.map(_.toString)
  }
}

final case class WhenSObjectValue(typeName: TypeName, id: Id) extends WhenValue {
  def checkMatchableTo(context: BlockVerifyContext, typeName: TypeName): Seq[String] = {
    context.logError(id.location, s"A $typeName literal is required for this value")
    Seq()
  }

  def checkIsSObject(context: BlockVerifyContext): Seq[String] = {
    context.getTypeFor(typeName, context.thisType) match {
      case Right(td) if td.isSObject => Seq(typeName.name.value.toLowerCase())
      case Right(_) =>
        context.logError(id.location, "An SObject type is required for this value")
        Seq()
      case Left(_) =>
        // defer to verify for missing type
        Seq()
    }
  }

  def checkEnumValue(context: BlockVerifyContext, typeDeclaration: TypeDeclaration): Seq[String] = {
    context.logError(id.location, "Expecting an enum constant value")
    Seq()
  }

  override def verify(context: BlockVerifyContext): Unit = {
    context.addVar(id.name, id, isReadOnly = false, typeName)
  }
}

object WhenValue {
  def construct(value: WhenValueContext): WhenValue = {
    CodeParser
      .toScala(value.ELSE())
      .map(_ => new WhenElseValue())
      .getOrElse(if (!value.whenLiteral().isEmpty) {
        WhenLiteralsValue(
          CodeParser
            .toScala(value.whenLiteral())
            .flatMap(l => WhenLiteral.construct(l))
        )
      } else {
        WhenSObjectValue(TypeReference.construct(value.typeRef()), Id.construct(value.id()))
      })
  }
}

final case class WhenControl(whenValue: WhenValue, block: Block) extends CST {
  def verify(context: BlockVerifyContext): Unit = {
    val blockContext = new InnerBlockVerifyContext(context).setControlRoot(context)
    whenValue.verify(blockContext)
    block.verify(blockContext)
    context.typePlugin.foreach(_.onBlockValidated(block, context.isStatic, blockContext))
  }
}

object WhenControl {
  def construct(parser: CodeParser, whenControl: WhenControlContext): WhenControl = {
    WhenControl(
      CodeParser.toScala(whenControl.whenValue()).map(v => WhenValue.construct(v)).get,
      Block.constructInner(parser, whenControl.block())
    ).withContext(whenControl)
  }
}

final case class SwitchStatement(expression: Expression, whenControls: List[WhenControl])
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    val result = expression.verify(context)
    if (result.isDefined) {
      val values = result.typeName match {
        case TypeNames.Integer | TypeNames.Long | TypeNames.String =>
          checkMatchableTo(context, result.typeName)
        case TypeNames.SObject =>
          checkIsSObject(context)
        case _ if result.typeDeclaration.nature == ENUM_NATURE =>
          checkEnumValue(context, result.typeDeclaration)
        case _ =>
          context.logError(
            expression.location,
            s"Switch expression must be a Integer, Long, String, SObject record or enum value, not '${result.typeName}'"
          )
          return;
      }
      checkWhenElseIsLast(context)
      checkForDoubleNull(context)
      val duplicates = values.groupBy(identity).collect { case (_, Seq(_, y, _*)) => y }
      duplicates.headOption.foreach(dup =>
        context.logError(expression.location, s"Duplicate when case for $dup")
      )
    }

    val switchContext = new InnerBlockVerifyContext(context).withBranchingControl()
    whenControls.foreach(_.verify(switchContext))
    verifyControlPath(
      switchContext,
      BranchControlPattern(
        Option
          .when(whenControls.isEmpty) { Array(x = true) }
          .getOrElse(whenControls.map(_ => true).toArray)
      )
    )
  }

  private def checkMatchableTo(context: BlockVerifyContext, typeName: TypeName): Seq[String] = {
    whenControls.flatMap(_.whenValue.checkMatchableTo(context, typeName))
  }

  private def checkIsSObject(context: BlockVerifyContext): Seq[String] = {
    whenControls.flatMap(_.whenValue.checkIsSObject(context))
  }

  private def checkEnumValue(
    context: BlockVerifyContext,
    typeDeclaration: TypeDeclaration
  ): Seq[String] = {
    whenControls.flatMap(_.whenValue.checkEnumValue(context, typeDeclaration))
  }

  private def checkWhenElseIsLast(context: BlockVerifyContext): Unit = {
    val notLastElse = whenControls.zipWithIndex
      .filter(_._1.whenValue.isInstanceOf[WhenElseValue])
      .find(_._2 != whenControls.length - 1)
    if (notLastElse.nonEmpty)
      context.logError(notLastElse.get._1.location, "'when else' must be the last when block")
  }

  private def checkForDoubleNull(context: BlockVerifyContext): Unit = {
    val literals = whenControls.flatMap(wc => {
      wc.whenValue match {
        case l: WhenLiteralsValue => l.literals
        case _                    => Seq()
      }
    })
    if (literals.count(_.isInstanceOf[WhenNullLiteral]) > 1)
      context.logError(
        literals.last.location,
        "There should only be one 'when null' block in a switch"
      )
  }
}

object SwitchStatement {
  def construct(parser: CodeParser, switchStatement: SwitchStatementContext): SwitchStatement = {
    SwitchStatement(
      Expression.construct(switchStatement.expression()),
      CodeParser
        .toScala(switchStatement.whenControl())
        .map(wc => WhenControl.construct(parser, wc).withContext(wc))
        .toList
    ).withContext(switchStatement)
  }
}
