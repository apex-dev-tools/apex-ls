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

import com.nawforce.apexlink.cst.AssignableSupport.{
  AssignableOptions,
  couldBeEqual,
  isAssignableDeclaration
}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames._
import com.nawforce.apexlink.org.OrgInfo
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.apexparser.ApexParser._
import com.nawforce.pkgforce.modifiers.ABSTRACT_MODIFIER
import com.nawforce.pkgforce.names._
import com.nawforce.runtime.parsers.CodeParser

import scala.collection.immutable.ArraySeq

/** New expression type name, similar to a typeRef */
final case class CreatedName(idPairs: List[IdCreatedNamePair]) extends CST {
  def typeNameLocation: Location = Location(
    location.location.startLine,
    location.location.startPosition,
    location.location.endLine,
    location.location.startPosition + typeName.name.value.length
  )

  lazy val typeName: TypeName = idPairs.tail.map(_.typeName).foldLeft(idPairs.head.typeName) {
    (acc: TypeName, typeName: TypeName) =>
      typeName.withTail(acc)
  }

  def verify(context: ExpressionVerifyContext): ExprContext = {

    val newType = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (newType.nonEmpty) {
      ExprContext(isStatic = Some(false), newType, newType.get)
    } else {
      context.missingType(location, typeName)
      ExprContext.empty
    }
  }
}

object CreatedName {
  def construct(from: CreatedNameContext): CreatedName = {
    val pairs = CodeParser.toScala(from.idCreatedNamePair())
    CreatedName(IdCreatedNamePair.construct(pairs.toList)).withContext(from)
  }
}

final case class IdCreatedNamePair(id: Id, types: ArraySeq[TypeName]) extends CST {
  val typeName: TypeName = {
    val encName = EncodedName(id.name)
    if (encName.ext.nonEmpty)
      TypeName(encName.fullName, types, Some(TypeNames.Schema)).intern
    else
      TypeName(encName.fullName, types, None).intern
  }
}

object IdCreatedNamePair {
  def construct(aList: List[IdCreatedNamePairContext]): List[IdCreatedNamePair] = {
    aList.map(x => IdCreatedNamePair.construct(x))
  }

  def construct(from: IdCreatedNamePairContext): IdCreatedNamePair = {
    IdCreatedNamePair(
      Id.constructAny(from.anyId()),
      CodeParser
        .toScala(from.typeList())
        .map(tl => TypeList.construct(tl))
        .getOrElse(TypeNames.emptyTypeNames)
    ).withContext(from)
  }
}

/** New expression node, these always start 'new TYPE' but there a few form alternatives */
final case class Creator(createdName: CreatedName, creatorRest: Option[CreatorRest]) extends CST {
  def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)
    creatorRest
      .map(_.verify(createdName, input, context))
      .getOrElse(ExprContext.empty)
  }
}

object Creator {
  def construct(from: CreatorContext): Creator = {
    val rest: Option[CreatorRest] =
      CodeParser
        .toScala(from.noRest())
        .map(NoRest.construct)
        .orElse(CodeParser.toScala(from.classCreatorRest()).map(ClassCreatorRest.construct))
        .orElse(CodeParser.toScala(from.arrayCreatorRest()).map(ArrayCreatorRest.construct))
        .orElse(CodeParser.toScala(from.mapCreatorRest()).map(MapCreatorRest.construct))
        .orElse(CodeParser.toScala(from.setCreatorRest()).map(SetOrListCreatorRest.construct))

    Creator(CreatedName.construct(from.createdName()), rest).withContext(from)
  }
}

/** Base for things that appear after 'new TYPE' expressions */
sealed abstract class CreatorRest extends CST {
  def verify(
    createdName: CreatedName,
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext
}

/** Object creation without arguments, e.g. new Foo() */
final class NoRest extends CreatorRest {
  override def verify(
    createdName: CreatedName,
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext = {
    createdName.verify(context)
  }
}

object NoRest {
  def construct(from: NoRestContext): NoRest = {
    new NoRest().withContext(from)
  }
}

/** Object creation with arguments, e.g. new Foo(a, b)
  * @param arguments constructor argument expressions
  */
final case class ClassCreatorRest(arguments: ArraySeq[Expression]) extends CreatorRest {
  override def verify(
    createdName: CreatedName,
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext = {

    val creating = createdName.verify(context)

    val isFieldConstructed =
      creating.declaration
        .map(_.isFieldConstructed)
        .getOrElse({
          context.module.isGhostedType(createdName.typeName) && EncodedName(
            createdName.typeName.name
          ).ext.exists(ClassCreatorRest.isFieldConstructedExt)

        })

    if (isFieldConstructed && creating.declaration.isEmpty) {
      validateFieldConstructorArgumentsGhosted(createdName.typeName, input, arguments, context)
      ExprContext.empty
    } else if (isFieldConstructed) {
      creating.typeDeclaration.validateFieldConstructorArguments(input, arguments, context)
      creating
    } else {
      val args = arguments.map(_.verify(input, context))
      if (args.forall(_.isDefined))
        validateConstructor(creating.declaration, args.map(arg => arg.typeName), context)
      else
        creating
    }
  }

  private def validateConstructor(
    input: Option[TypeDeclaration],
    arguments: ArraySeq[TypeName],
    context: ExpressionVerifyContext
  ): ExprContext = {
    input match {
      case Some(td) =>
        if (td.modifiers.contains(ABSTRACT_MODIFIER)) {
          OrgInfo.logError(location, s"Abstract classes cannot be constructed: ${td.typeName}")
          return ExprContext.empty
        }
        td.findConstructor(arguments, context) match {
          case Left(error) =>
            OrgInfo.logError(location, error)
            ExprContext.empty
          case Right(ctor) => ExprContext(Some(false), input, ctor)
        }
      case _ => ExprContext.empty
    }
  }

  private def validateFieldConstructorArgumentsGhosted(
    typeName: TypeName,
    input: ExprContext,
    arguments: ArraySeq[Expression],
    context: ExpressionVerifyContext
  ): Unit = {

    val validArgs = arguments.flatMap {
      case BinaryExpression(PrimaryExpression(IdPrimary(id)), rhs, "=") =>
        rhs.verify(input, context)
        Some(id)
      case argument =>
        OrgInfo.logError(
          argument.location,
          s"SObject type '$typeName' construction needs '<field name> = <value>' arguments"
        )
        None
    }

    if (validArgs.length == arguments.length) {
      val duplicates = validArgs.groupBy(_.name).collect { case (_, ArraySeq(_, y, _*)) => y }
      if (duplicates.nonEmpty) {
        OrgInfo.logError(
          duplicates.head.location,
          s"Duplicate assignment to field '${duplicates.head.name}' on SObject type '$typeName'"
        )
      }
    }
  }

}

object ClassCreatorRest {
  private val fieldConstructedExt = Set(Name("c"), Name("e"), Name("b"), Name("mdt"))

  def construct(from: ClassCreatorRestContext): ClassCreatorRest = {
    ClassCreatorRest(Arguments.construct(from.arguments())).withContext(from)
  }

  private def isFieldConstructedExt(ext: Name): Boolean = fieldConstructedExt.contains(ext)
}

/** Array creation arguments, e.g. either new String[3] or new Account[]{accountA, accountB}
  * @param indexExpression expression for size of array
  * @param arrayInitializer an list of initializer expressions
  */
final case class ArrayCreatorRest(
  indexExpression: Option[Expression],
  arrayInitializer: Option[ArrayInitializer]
) extends CreatorRest {
  override def verify(
    createdName: CreatedName,
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext = {

    // Validate type of Array we are creating
    val creating = createdName.verify(context)
    if (!creating.isDefined) {
      return ExprContext.empty
    }

    // If we a size that must be an integer
    indexExpression.foreach(expr => {
      val indexType = expr.verify(input, context)
      indexType.declaration.foreach(indexType => {
        if (indexType.typeName != TypeNames.Integer) {
          OrgInfo.logError(
            expr.location,
            s"Index for array construction must be an Integer, not '${indexType.typeName}'"
          )
        }
      })
    })

    // If we have initializer expressions the type must be assignable
    arrayInitializer.foreach(_.expressions.foreach(expr => {
      val exprType = expr.verify(input, context)
      exprType.declaration.foreach(exprType => {
        if (!isAssignableDeclaration(creating.typeName, exprType, context)) {
          OrgInfo.logError(
            expr.location,
            s"Expression of type '${exprType.typeName}' can not be assigned to ${creating.typeName}'"
          )
        }
      })
    }))

    // Return the array type
    val listType        = creating.typeName.asListOf
    val listDeclaration = context.getTypeAndAddDependency(listType, context.thisType).toOption.get
    ExprContext(isStatic = Some(false), listDeclaration)
  }
}

object ArrayCreatorRest {
  def construct(from: ArrayCreatorRestContext): ArrayCreatorRest = {
    ArrayCreatorRest(
      CodeParser.toScala(from.expression()).map(Expression.construct),
      CodeParser.toScala(from.arrayInitializer()).map(ArrayInitializer.construct)
    )
  }
}

final case class ArrayInitializer(expressions: ArraySeq[Expression]) extends CST {
  def verify(input: ExprContext, context: ExpressionVerifyContext): Unit = {
    expressions.foreach(_.verify(input, context))
  }
}

object ArrayInitializer {
  def construct(from: ArrayInitializerContext): ArrayInitializer = {
    val initializers = CodeParser.toScala(from.expression())
    ArrayInitializer(Expression.construct(initializers)).withContext(from)
  }
}

final case class MapCreatorRest(pairs: List[MapCreatorRestPair]) extends CreatorRest {
  override def verify(
    createdName: CreatedName,
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext = {
    val creating = createdName.verify(context)
    if (creating.declaration.isEmpty)
      return ExprContext.empty

    val td            = creating.declaration.get
    val enclosedTypes = td.typeName.getMapType

    if (enclosedTypes.isEmpty) {
      OrgInfo.logError(
        location,
        s"Expression pair list construction is only supported for Map types, not '${td.typeName}'"
      )
      return ExprContext.empty
    }

    val keyType = context.getTypeAndAddDependency(enclosedTypes.get._1, context.thisType)
    if (keyType.isLeft) {
      if (!context.module.isGhostedType(enclosedTypes.get._1))
        OrgInfo.log(keyType.swap.getOrElse(throw new NoSuchElementException).asIssue(location))
      return ExprContext.empty
    }

    val valueType = context.getTypeAndAddDependency(enclosedTypes.get._2, context.thisType)
    if (valueType.isLeft) {
      if (!context.module.isGhostedType(enclosedTypes.get._2))
        OrgInfo.log(valueType.swap.getOrElse(throw new NoSuchElementException).asIssue(location))
      return ExprContext.empty
    }

    pairs.foreach(pair => {
      val pairContext = pair.verify(input, context)
      if (pairContext._1.isDefined) {
        val isKeyAssignable =
          couldBeEqual(pairContext._1.typeDeclaration, keyType.toOption.get, context)
        if (!isKeyAssignable) {
          OrgInfo.logError(
            location,
            s"Incompatible key type '${pairContext._1.typeName}' for '${keyType.toOption.get.typeName}'"
          )
          return ExprContext.empty
        }
      }
      if (pairContext._2.isDefined) {
        val isValueAssignable =
          couldBeEqual(pairContext._2.typeDeclaration, valueType.toOption.get, context)
        if (!isValueAssignable) {
          OrgInfo.logError(
            location,
            s"Incompatible value type '${pairContext._2.typeName}' for '${valueType.toOption.get.typeName}'"
          )
          return ExprContext.empty
        }
      }
    })

    creating
  }
}

object MapCreatorRest {
  def construct(from: MapCreatorRestContext): MapCreatorRest = {
    val pairs = CodeParser.toScala(from.mapCreatorRestPair())
    MapCreatorRest(MapCreatorRestPair.construct(pairs.toList)).withContext(from)
  }
}

final case class MapCreatorRestPair(from: Expression, to: Expression) extends CST {
  def verify(input: ExprContext, context: ExpressionVerifyContext): (ExprContext, ExprContext) = {
    val fromContext = from.verify(input, context)
    val toContext   = to.verify(input, context)
    (fromContext, toContext)
  }
}

object MapCreatorRestPair {
  def construct(aList: List[MapCreatorRestPairContext]): List[MapCreatorRestPair] = {
    aList.flatMap(x => MapCreatorRestPair.construct(x))
  }

  def construct(from: MapCreatorRestPairContext): Option[MapCreatorRestPair] = {
    val expressions = CodeParser.toScala(from.expression())
    if (expressions.length == 2) {
      Some(
        MapCreatorRestPair(
          Expression.construct(expressions.head),
          Expression.construct(expressions(1))
        )
          .withContext(from)
      )
    } else {
      None
    }
  }
}

/** Set or List creation arguments, e.g. new List<Account>{accountA, accountB}
  * @param parts expressions for each argument
  */
final case class SetOrListCreatorRest(parts: ArraySeq[Expression]) extends CreatorRest {
  override def verify(
    createdName: CreatedName,
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext = {

    // Validate the basic types are OK
    val creating = createdName.verify(context)
    if (creating.declaration.isEmpty)
      return ExprContext.empty

    val td           = creating.declaration.get
    val enclosedType = td.typeName.getSetOrListType
    if (enclosedType.isEmpty) {
      OrgInfo.logError(
        location,
        s"Expression list construction is only supported for Set or List types, not '${td.typeName}'"
      )
      return ExprContext.empty
    }

    // Check generic type is available
    context.getTypeAndAddDependency(enclosedType.get, context.thisType) match {
      case Left(error) =>
        if (!context.module.isGhostedType(enclosedType.get))
          OrgInfo.log(error.asIssue(location))
        ExprContext.empty
      case Right(toType) =>
        // For each expression check we can assign to the generic type
        parts.foreach(part => {
          val exprType = part.verify(input, context)
          exprType.declaration.foreach(exprType => {
            if (!isAssignableDeclaration(toType.typeName, exprType, context)) {
              OrgInfo.logError(
                location,
                s"Expression of type '${exprType.typeName}' can not be assigned to ${toType.typeName}'"
              )
            }
          })
        })
        creating
    }
  }
}

object SetOrListCreatorRest {
  def construct(from: SetCreatorRestContext): SetOrListCreatorRest = {
    val parts = CodeParser.toScala(from.expression())
    SetOrListCreatorRest(Expression.construct(parts)).withContext(from)
  }
}
