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

import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexFieldLike}
import com.nawforce.apexlink.types.core.{FieldDeclaration, TypeDeclaration}
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.apexparser.ApexParser._
import com.nawforce.apexparser.ApexParserBaseVisitor
import com.nawforce.pkgforce.names.{DotName, EncodedName, Name, Names, TypeName}
import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.parsers.CodeParser

import scala.collection.compat.immutable.ArraySeq

sealed abstract class Primary extends CST {
  def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext
}

final case class ThisPrimary() extends Primary {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {

    if (input.declaration.isEmpty) {
      context.logError(location, "")
    }

    if (input.isStatic.contains(true)) {
      context.logError(location, s"'this' can not be used in a static context")
      ExprContext.empty
    } else {
      // Allow this to reference statics platform bug
      ExprContext(isStatic = None, context.thisType)
    }
  }
}

final case class SuperPrimary() extends Primary {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    if (input.isStatic.contains(true)) {
      context.logError(location, s"'super' can not be used in a static context")
      ExprContext.empty
    } else {
      ExprContext(isStatic = Some(false), context.superType)
    }
  }
}

final case class LiteralPrimary(literal: Literal) extends Primary {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    literal.verify(context)
    ExprContext(isStatic = Some(false), Some(literal.getType))
  }
}

final case class TypeReferencePrimary(typeName: TypeName) extends Primary {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {

    // Workaround miss parsing of Foo__c.SObjectType.class as a typeRef
    val targetTypeName =
      if (typeName.outer.nonEmpty && typeName.name == Names.SObjectType) {
        TypeNames.sObjectType$(typeName.outer.get)
      } else {
        typeName
      }

    if (targetTypeName != TypeNames.Void) {
      val td = context.getTypeAndAddDependency(targetTypeName, context.thisType).toOption
      if (td.isEmpty)
        context.missingType(location, typeName)
    }
    ExprContext(isStatic = Some(false), Some(PlatformTypes.typeType))
  }
}

final case class IdPrimary(id: Id) extends Primary {
  // NOTE: cachedClassFieldDeclaration stores the field declaration if there is a class field that shares a name with
  // this IdPrimary. This means if this IdPrimary belongs to a local field declaration with the same name as a
  // class field, then cachedClassFieldDeclaration will store the class field declaration, even though it is shadowed.
  private var cachedClassFieldDeclaration: Option[FieldDeclaration] = None
  var typeName: Option[TypeName]                                    = None
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    cachedClassFieldDeclaration = input.typeDeclaration.findField(id.name, input.isStatic)
    isVarReference(context)
      .getOrElse(
        isFieldReference(input, context)
          .getOrElse(isTypeReference(context).getOrElse({
            if (input.typeDeclaration.isComplete)
              context.missingIdentifier(location, input.typeName, id.name)
            ExprContext.empty
          }))
      )
  }

  /** Will return the location of this IdPrimary if the ApexFieldDeclaration is a class variable with the same name as the
    * IdPrimary.
    * This method does not handle shadowed variables - it only checks a class field declaration matches the provided
    * declaration.
    */
  def getLocationForClassFieldUsage(fd: ApexFieldDeclaration): Option[Location] = {
    cachedClassFieldDeclaration match {
      case Some(fdFromCallout: ApexFieldDeclaration) =>
        if (fdFromCallout.idPathLocation == fd.idPathLocation) Some(id.location.location)
        else None
      case _ => None
    }
  }

  def isCachedFieldEmpty: Boolean = {
    cachedClassFieldDeclaration.isEmpty
  }

  private def isVarReference(context: ExpressionVerifyContext): Option[ExprContext] = {
    context
      .isVar(id.name, markUsed = true)
      .map(varTypeAndDefinition => {
        typeName = Some(varTypeAndDefinition.declaration.typeName)
        ExprContext(
          isStatic = Some(false),
          Some(varTypeAndDefinition.declaration),
          varTypeAndDefinition.definition
        )
      })
  }

  private def isFieldReference(
    input: ExprContext,
    context: ExpressionVerifyContext
  ): Option[ExprContext] = {
    val td            = input.typeDeclaration
    val staticContext = Some(true).filter(input.isStatic.contains)

    val field = findField(id.name, td, staticContext)

    if (field.nonEmpty && isAccessible(td, field.get, staticContext)) {
      context.addDependency(field.get)
      Some(
        context
          .getTypeAndAddDependency(field.get.typeName, td)
          .toOption
          .map(target => {
            ExprContext(isStatic = Some(false), Some(target), field.get)
          })
          .getOrElse({
            context.missingType(location, field.get.typeName)
            ExprContext.empty
          })
      )
    } else {
      None
    }
  }

  private def isTypeReference(context: ExpressionVerifyContext): Option[ExprContext] = {
    context.getTypeAndAddDependency(TypeName(id.name), context.thisType) match {
      case Right(td) => Some(ExprContext(isStatic = Some(true), Some(td), td))
      case _         => None
    }
  }

  private def findField(
    name: Name,
    td: TypeDeclaration,
    staticContext: Option[Boolean]
  ): Option[FieldDeclaration] = {
    val encodedName   = EncodedName(name)
    val namespaceName = encodedName.defaultNamespace(td.moduleDeclaration.flatMap(_.namespace))
    td.findField(namespaceName.fullName, staticContext)
      .orElse({
        if (encodedName != namespaceName)
          td.findField(encodedName.fullName, staticContext)
        else None
      })
  }

  private def isAccessible(
    td: TypeDeclaration,
    field: FieldDeclaration,
    staticContext: Option[Boolean]
  ): Boolean = {
    // From static context, we can only use locally defined static fields, but can only test this with Apex
    // defined fields & types.
    (staticContext.contains(true), td, field) match {
      case (true, ad: ApexClassDeclaration, af: ApexFieldLike) => ad.localFields.contains(af)
      case _                                                   => true
    }
  }
}

/** Type of return from SOQL queries, based on functions used in the query */
sealed trait QueryResultType
case object LIST_RESULT_QUERY      extends QueryResultType
case object COUNT_RESULT_QUERY     extends QueryResultType
case object AGGREGATE_RESULT_QUERY extends QueryResultType

/** Inline SOQL primary, captures just enough detail to enable downstream processing
  * @param queryResultType type of results expected from query
  * @param fromNames dot names used in FROM
  * @param boundExpressions bound expressions used anywhere in the query
  */
final case class SOQL(
  queryResultType: QueryResultType,
  fromNames: Array[DotName],
  boundExpressions: ArraySeq[Expression]
) extends Primary {

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {

    boundExpressions.foreach(expr => {
      expr.verify(input, context)
    })

    if (fromNames.length != 1 || fromNames.head.names.size != 1) {
      context.logError(
        location,
        s"Expecting SOQL to query only a single SObject, found '${fromNames.mkString(", ")}'"
      )
    }
    val sobjectType = TypeName(fromNames.head.names.head, Nil, Some(TypeNames.Schema))

    if (queryResultType == COUNT_RESULT_QUERY) {
      ExprContext(isStatic = Some(false), PlatformTypes.integerType)
    } else {
      val recordSetType =
        if (queryResultType == AGGREGATE_RESULT_QUERY) TypeNames.AggregateResult else sobjectType
      context.getTypeAndAddDependency(
        TypeNames.recordSetOf(recordSetType),
        context.thisType
      ) match {
        case Left(_) =>
          if (!context.module.isGhostedType(sobjectType))
            context.missingType(location, sobjectType)
          ExprContext(isStatic = Some(false), context.module.any)
        case Right(td) =>
          ExprContext(isStatic = Some(false), td)
      }
    }
  }
}

object SOQL {
  def apply(query: QueryContext): SOQL = {
    val entries = CodeParser
      .toScala(query.selectList().selectEntry())

    val aggregateFunctions = entries
      .flatMap(se => CodeParser.toScala(se.soqlFunction()))
      .filter(fn =>
        CodeParser.toScala(fn.AVG()).nonEmpty ||
          CodeParser.toScala(fn.COUNT()).nonEmpty ||
          CodeParser.toScala(fn.COUNT_DISTINCT()).nonEmpty ||
          CodeParser.toScala(fn.MIN()).nonEmpty ||
          CodeParser.toScala(fn.MAX()).nonEmpty ||
          CodeParser.toScala(fn.SUM()).nonEmpty
      )
    val countFunctions = aggregateFunctions.filter(fn => CodeParser.toScala(fn.COUNT()).nonEmpty)
    val emptyCountFunctions =
      countFunctions.filter(fn => CodeParser.toScala(fn.fieldName()).isEmpty)

    val resultType =
      if (entries.size == 1 && emptyCountFunctions.size == 1) {
        // Count queries are only valid for 'Select Count() From...', otherwise assume is aggregate
        COUNT_RESULT_QUERY
      } else if (
        CodeParser.toScala(query.groupByClause()).nonEmpty || aggregateFunctions.nonEmpty
      ) {
        AGGREGATE_RESULT_QUERY
      } else {
        LIST_RESULT_QUERY
      }

    // NOTE: Bound variables don't support all expressions but we have not worked out what
    // is available so currently model as an expression
    val boundedExpressions = new BoundExprVisitor().visit(query).map(ec => Expression.construct(ec))
    val fromNames =
      CodeParser
        .toScala(query.fromNameList().fieldName())
        .map(nameList =>
          DotName(CodeParser.toScala(nameList.soqlId()).map(name => Name(CodeParser.getText(name))))
        )
    new SOQL(resultType, fromNames.toArray, boundedExpressions)
  }
}

/** Inline SOSL primary, captures just enough detail to enable downstream processing
  * @param boundExpressions bound expressions used anywhere in the query
  */
final case class SOSL(boundExpressions: ArraySeq[Expression]) extends Primary {

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {

    boundExpressions.foreach(expr => {
      expr.verify(input, context)
    })

    ExprContext(isStatic = Some(false), context.module.any)
  }
}

object SOSL {
  def apply(query: SoslLiteralContext): SOSL = {
    val boundedExpressions = new BoundExprVisitor().visit(query).map(ec => Expression.construct(ec))
    new SOSL(boundedExpressions)
  }
}

/** ANTLR visitor for extracting bound expressions from SOQL or SOSL queries, e.g. WHERE Id in :Ids */
class BoundExprVisitor extends ApexParserBaseVisitor[ArraySeq[ExpressionContext]] {

  override def defaultResult(): ArraySeq[ExpressionContext] = ArraySeq[ExpressionContext]()

  override protected def aggregateResult(
    aggregate: ArraySeq[ExpressionContext],
    nextResult: ArraySeq[ExpressionContext]
  ): ArraySeq[ExpressionContext] = {
    aggregate ++ nextResult
  }

  override def visitBoundExpression(
    boundExpressionContext: BoundExpressionContext
  ): ArraySeq[ExpressionContext] = {
    ArraySeq(boundExpressionContext.expression())
  }
}

object Primary {
  def construct(from: PrimaryContext): Primary = {
    val cst =
      from match {
        case _: ThisPrimaryContext =>
          ThisPrimary()
        case _: SuperPrimaryContext =>
          SuperPrimary()
        case ctx: LiteralPrimaryContext =>
          LiteralPrimary(Literal.construct(ctx.literal()))
        case ctx: TypeRefPrimaryContext =>
          TypeReferencePrimary(TypeReference.construct(ctx.typeRef()))
        case _: VoidPrimaryContext =>
          TypeReferencePrimary(TypeName.Void)
        case id: IdPrimaryContext =>
          IdPrimary(Id.construct(id.id()))
        case ctx: SoqlPrimaryContext =>
          SOQL(ctx.soqlLiteral().query())
        case ctx: SoslPrimaryContext =>
          SOSL(ctx.soslLiteral())
      }
    cst.withContext(from)
  }
}
