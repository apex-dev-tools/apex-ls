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

import com.nawforce.apexlink.cst.AssignableSupport.{couldBeEqual, isAssignableDeclaration}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.pkgforce.names.TypeName

abstract class Operation {

  def isAssignmentOperation: Boolean = false

  def verify(
    leftType: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext]

  def getCommonBase(
    toType: TypeDeclaration,
    fromType: TypeDeclaration,
    context: VerifyContext
  ): Option[TypeDeclaration] = {
    val toSupertypes   = toType.superTypes()
    val fromSupertypes = fromType.superTypes()
    val common         = toSupertypes.intersect(fromSupertypes)
    common.headOption.flatMap(context.getTypeFor(_, context.thisType).toOption)
  }

  def isNumericKind(typeName: TypeName): Boolean = {
    typeName == TypeNames.Integer ||
    typeName == TypeNames.Long ||
    typeName == TypeNames.Decimal ||
    typeName == TypeNames.Double
  }

  def isStringKind(typeName: TypeName): Boolean = {
    typeName == TypeNames.String ||
    typeName == TypeNames.IdType
  }

  def isDateKind(typeName: TypeName): Boolean = {
    typeName == TypeNames.Date ||
    typeName == TypeNames.Datetime
  }

  def isTimeKind(typeName: TypeName): Boolean = {
    typeName == TypeNames.Time
  }

  def isPrimitiveKind(typeName: TypeName): Boolean = {
    Operation.primitiveTypes.contains(typeName)
  }

  def getArithmeticResult(leftType: TypeName, rightType: TypeName): Option[TypeDeclaration] = {
    Operation.arithmeticOps.get((leftType, rightType))
  }

  def getArithmeticAddSubtractAssigmentResult(
    leftType: TypeName,
    rightType: TypeName
  ): Option[TypeDeclaration] = {
    Operation.arithmeticAddSubtractAssigmentOps.get((leftType, rightType))
  }

  def getArithmeticMultiplyDivideAssigmentResult(
    leftType: TypeName,
    rightType: TypeName
  ): Option[TypeDeclaration] = {
    Operation.arithmeticMultiplyDivideAssigmentOps.get((leftType, rightType))
  }

  def getBitwiseResult(leftType: TypeName, rightType: TypeName): Option[TypeDeclaration] = {
    Operation.bitwiseOps.get((leftType, rightType))
  }

  def getBitwiseAssignmentResult(
    leftType: TypeName,
    rightType: TypeName
  ): Option[TypeDeclaration] = {
    Operation.bitwiseAssignmentOps.get((leftType, rightType))
  }

  def getReadOnlyError(
    exprContext: ExprContext,
    verifyContext: ExpressionVerifyContext
  ): Option[String] = {
    exprContext.locatable match {
      case Some(variable: VariableDeclarator) if variable.isReadOnly =>
        Some(s"Variable '${variable.id.name.value}' can not be assigned to, it is final")
      case Some(field: ApexFieldDeclaration)
          if field.isReadOnly && !isFieldFinalInitialisationContext(field, verifyContext) =>
        // Final fields can be set in ctor & init blocks
        Some(s"Field '${field.name.value}' can not be assigned to, it is final")
      case Some(id: Id) if verifyContext.isVar(id.name).exists(_.isReadOnly) =>
        // For params the locatable only gives Id currently, so we need extra verification
        verifyContext
          .isVar(id.name)
          .flatMap(varDetails =>
            if (varDetails.isReadOnly && varDetails.definition.contains(id))
              Some(s"Parameter '${id.name.value}' can not be assigned to, it is final")
            else None
          )
      case _ => None
    }
  }

  private def isFieldFinalInitialisationContext(
    field: ApexFieldDeclaration,
    context: VerifyContext
  ): Boolean = {
    context match {
      case bodyContext: BodyDeclarationVerifyContext =>
        bodyContext.isFieldFinalInitialisationContext(field)
      case _ => context.parent().exists(parent => isFieldFinalInitialisationContext(field, parent))
    }
  }
}

object Operation {
  private lazy val primitiveTypes: Set[TypeName] = Set(
    TypeNames.Integer,
    TypeNames.Long,
    TypeNames.Double,
    TypeNames.Decimal,
    TypeNames.String,
    TypeNames.Boolean,
    TypeNames.Date,
    TypeNames.Datetime,
    TypeNames.Time,
    TypeNames.Blob,
    TypeNames.IdType
  )

  private lazy val arithmeticOps: Map[(TypeName, TypeName), TypeDeclaration] = Map(
    (TypeNames.Integer, TypeNames.Integer)  -> PlatformTypes.integerType,
    (TypeNames.Integer, TypeNames.Long)     -> PlatformTypes.longType,
    (TypeNames.Integer, TypeNames.Decimal)  -> PlatformTypes.decimalType,
    (TypeNames.Integer, TypeNames.Double)   -> PlatformTypes.doubleType,
    (TypeNames.Long, TypeNames.Integer)     -> PlatformTypes.longType,
    (TypeNames.Long, TypeNames.Long)        -> PlatformTypes.longType,
    (TypeNames.Long, TypeNames.Decimal)     -> PlatformTypes.decimalType,
    (TypeNames.Long, TypeNames.Double)      -> PlatformTypes.doubleType,
    (TypeNames.Decimal, TypeNames.Integer)  -> PlatformTypes.decimalType,
    (TypeNames.Decimal, TypeNames.Long)     -> PlatformTypes.decimalType,
    (TypeNames.Decimal, TypeNames.Decimal)  -> PlatformTypes.decimalType,
    (TypeNames.Decimal, TypeNames.Double)   -> PlatformTypes.decimalType,
    (TypeNames.Double, TypeNames.Integer)   -> PlatformTypes.doubleType,
    (TypeNames.Double, TypeNames.Long)      -> PlatformTypes.doubleType,
    (TypeNames.Double, TypeNames.Decimal)   -> PlatformTypes.decimalType,
    (TypeNames.Double, TypeNames.Double)    -> PlatformTypes.doubleType,
    (TypeNames.Date, TypeNames.Integer)     -> PlatformTypes.dateType,
    (TypeNames.Date, TypeNames.Long)        -> PlatformTypes.dateType,
    (TypeNames.Datetime, TypeNames.Integer) -> PlatformTypes.datetimeType,
    (TypeNames.Datetime, TypeNames.Long)    -> PlatformTypes.datetimeType,
    (TypeNames.Datetime, TypeNames.Decimal) -> PlatformTypes.datetimeType,
    (TypeNames.Datetime, TypeNames.Double)  -> PlatformTypes.datetimeType,
    (TypeNames.Time, TypeNames.Integer)     -> PlatformTypes.timeType,
    (TypeNames.Time, TypeNames.Long)        -> PlatformTypes.timeType
  )

  private lazy val arithmeticAddSubtractAssigmentOps: Map[(TypeName, TypeName), TypeDeclaration] =
    Map(
      (TypeNames.Integer, TypeNames.Integer)  -> PlatformTypes.integerType,
      (TypeNames.Long, TypeNames.Integer)     -> PlatformTypes.longType,
      (TypeNames.Long, TypeNames.Long)        -> PlatformTypes.longType,
      (TypeNames.Decimal, TypeNames.Integer)  -> PlatformTypes.decimalType,
      (TypeNames.Decimal, TypeNames.Long)     -> PlatformTypes.decimalType,
      (TypeNames.Decimal, TypeNames.Double)   -> PlatformTypes.decimalType,
      (TypeNames.Decimal, TypeNames.Decimal)  -> PlatformTypes.decimalType,
      (TypeNames.Double, TypeNames.Integer)   -> PlatformTypes.doubleType,
      (TypeNames.Double, TypeNames.Long)      -> PlatformTypes.doubleType,
      (TypeNames.Double, TypeNames.Decimal)   -> PlatformTypes.decimalType,
      (TypeNames.Double, TypeNames.Double)    -> PlatformTypes.doubleType,
      (TypeNames.Date, TypeNames.Integer)     -> PlatformTypes.dateType,
      (TypeNames.Date, TypeNames.Long)        -> PlatformTypes.dateType,
      (TypeNames.Datetime, TypeNames.Integer) -> PlatformTypes.datetimeType,
      (TypeNames.Datetime, TypeNames.Long)    -> PlatformTypes.datetimeType,
      (TypeNames.Time, TypeNames.Integer)     -> PlatformTypes.timeType,
      (TypeNames.Time, TypeNames.Long)        -> PlatformTypes.timeType
    )

  private lazy val arithmeticMultiplyDivideAssigmentOps
    : Map[(TypeName, TypeName), TypeDeclaration] = Map(
    (TypeNames.Integer, TypeNames.Integer) -> PlatformTypes.integerType,
    (TypeNames.Long, TypeNames.Integer)    -> PlatformTypes.longType,
    (TypeNames.Long, TypeNames.Long)       -> PlatformTypes.longType,
    (TypeNames.Decimal, TypeNames.Integer) -> PlatformTypes.decimalType,
    (TypeNames.Decimal, TypeNames.Long)    -> PlatformTypes.decimalType,
    (TypeNames.Decimal, TypeNames.Double)  -> PlatformTypes.decimalType,
    (TypeNames.Decimal, TypeNames.Decimal) -> PlatformTypes.decimalType,
    (TypeNames.Double, TypeNames.Integer)  -> PlatformTypes.doubleType,
    (TypeNames.Double, TypeNames.Long)     -> PlatformTypes.doubleType,
    (TypeNames.Double, TypeNames.Decimal)  -> PlatformTypes.decimalType,
    (TypeNames.Double, TypeNames.Double)   -> PlatformTypes.doubleType
  )

  private lazy val bitwiseOps: Map[(TypeName, TypeName), TypeDeclaration] = Map(
    (TypeNames.Integer, TypeNames.Integer) -> PlatformTypes.integerType,
    (TypeNames.Integer, TypeNames.Long)    -> PlatformTypes.longType,
    (TypeNames.Long, TypeNames.Integer)    -> PlatformTypes.longType,
    (TypeNames.Long, TypeNames.Long)       -> PlatformTypes.longType,
    (TypeNames.Boolean, TypeNames.Boolean) -> PlatformTypes.booleanType
  )

  private lazy val bitwiseAssignmentOps: Map[(TypeName, TypeName), TypeDeclaration] = Map(
    (TypeNames.Integer, TypeNames.Integer) -> PlatformTypes.integerType,
    (TypeNames.Long, TypeNames.Integer)    -> PlatformTypes.longType,
    (TypeNames.Long, TypeNames.Long)       -> PlatformTypes.longType,
    (TypeNames.Boolean, TypeNames.Boolean) -> PlatformTypes.longType
  )
}

case object AssignmentOperation extends Operation {
  override def isAssignmentOperation: Boolean = true

  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    if (rightContext.typeName == TypeNames.Null) {
      Right(leftContext)
    } else if (
      isAssignableDeclaration(leftContext.typeName, rightContext.typeDeclaration, context)
    ) {
      Right(leftContext)
    } else {
      Left(
        s"Incompatible types in assignment, from '${rightContext.typeName}' to '${leftContext.typeName}'"
      )
    }
  }
}

case object LogicalOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    if (!isAssignableDeclaration(TypeNames.Boolean, rightContext.typeDeclaration, context)) {
      Left(s"Right expression of logical $op must a boolean, not '${rightContext.typeName}'")
    } else if (!isAssignableDeclaration(TypeNames.Boolean, leftContext.typeDeclaration, context)) {
      Left(s"Left expression of logical $op must a boolean, not '${leftContext.typeName}'")
    } else {
      Right(leftContext)
    }
  }
}

case object CompareOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {

    if (isNumericKind(leftContext.typeName)) {
      if (!isNumericKind(rightContext.typeName)) {
        return Left(
          s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'"
        )
      }
    } else if (isStringKind(leftContext.typeName)) {
      if (!isStringKind(rightContext.typeName))
        return Left(
          s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'"
        )
    } else if (isDateKind(leftContext.typeName)) {
      if (!isDateKind(rightContext.typeName)) {
        return Left(
          s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'"
        )
      }
    } else if (isTimeKind(leftContext.typeName)) {
      if (!isTimeKind(rightContext.typeName)) {
        return Left(
          s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'"
        )
      }
    } else {
      return Left(
        s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'"
      )
    }
    Right(ExprContext(isStatic = Some(false), PlatformTypes.booleanType))
  }
}

case object ExactEqualityOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {

    if (!couldBeEqual(leftContext.typeDeclaration, rightContext.typeDeclaration, context))
      Left(s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'")
    else if (isPrimitiveKind(leftContext.typeName) && isPrimitiveKind(rightContext.typeName))
      Left(
        "Exact equality/inequality is not supported between primitive types " +
          s"'${leftContext.typeName}' & '${rightContext.typeName}'"
      )
    else
      Right(ExprContext(isStatic = Some(false), PlatformTypes.booleanType))
  }
}

case object EqualityOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    if (couldBeEqual(leftContext.typeDeclaration, rightContext.typeDeclaration, context))
      Right(ExprContext(isStatic = Some(false), PlatformTypes.booleanType))
    else
      Left(s"Comparing incompatible types '${leftContext.typeName}' and '${rightContext.typeName}'")
  }
}

case object PlusOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    if (leftContext.typeName == TypeNames.String || rightContext.typeName == TypeNames.String) {
      Right(ExprContext(isStatic = Some(false), PlatformTypes.stringType))
    } else {
      val td = getArithmeticResult(leftContext.typeName, rightContext.typeName)
      if (td.isEmpty) {
        return Left(
          s"Addition operation not allowed between types '${leftContext.typeName}' and '${rightContext.typeName}'"
        )
      }
      Right(ExprContext(isStatic = Some(false), td.get))
    }
  }
}

case object ArithmeticOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    val td = getArithmeticResult(leftContext.typeName, rightContext.typeName)
    if (td.isEmpty) {
      return Left(
        s"Arithmetic operation not allowed between types '${leftContext.typeName}' and '${rightContext.typeName}'"
      )
    }
    Right(ExprContext(isStatic = Some(false), td.get))
  }
}

case object ArithmeticAddSubtractAssignmentOperation extends Operation {
  override def isAssignmentOperation: Boolean = true

  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    if (leftContext.typeName == TypeNames.String && op == "+=") {
      Right(ExprContext(isStatic = Some(false), PlatformTypes.stringType))
    } else {
      val td =
        getArithmeticAddSubtractAssigmentResult(leftContext.typeName, rightContext.typeName)
      if (td.isEmpty) {
        return Left(
          s"Arithmetic operation not allowed between types '${leftContext.typeName}' and '${rightContext.typeName}'"
        )
      }
      Right(ExprContext(isStatic = Some(false), td.get))
    }
  }
}

case object ArithmeticMultiplyDivideAssignmentOperation extends Operation {
  override def isAssignmentOperation: Boolean = true

  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    val td =
      getArithmeticMultiplyDivideAssigmentResult(leftContext.typeName, rightContext.typeName)
    if (td.isEmpty) {
      return Left(
        s"Arithmetic operation not allowed between types '${leftContext.typeName}' and '${rightContext.typeName}'"
      )
    }
    Right(ExprContext(isStatic = Some(false), td.get))
  }
}

case object BitwiseOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    val td = getBitwiseResult(leftContext.typeName, rightContext.typeName)
    if (td.isEmpty) {
      return Left(
        s"Bitwise operation only allowed between Integer, Long & Boolean types, not '${leftContext.typeName}' and '${rightContext.typeName}'"
      )
    }
    Right(ExprContext(isStatic = Some(false), td.get))
  }
}

case object BitwiseAssignmentOperation extends Operation {
  override def isAssignmentOperation: Boolean = true

  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {
    val td = getBitwiseAssignmentResult(leftContext.typeName, rightContext.typeName)
    if (td.isEmpty) {
      return Left(
        s"Bitwise operation only allowed between Integer, Long & Boolean types, not '${leftContext.typeName}' and '${rightContext.typeName}'"
      )
    }
    Right(ExprContext(isStatic = Some(false), td.get))
  }
}

case object ConditionalOperation extends Operation {
  override def verify(
    leftContext: ExprContext,
    rightContext: ExprContext,
    op: String,
    context: ExpressionVerifyContext
  ): Either[String, ExprContext] = {

    // Future: How does this really function, Java mechanics are very complex
    if (isAssignableDeclaration(leftContext.typeName, rightContext.typeDeclaration, context)) {
      Right(leftContext)
    } else if (
      isAssignableDeclaration(rightContext.typeName, leftContext.typeDeclaration, context)
    ) {
      Right(rightContext)
    } else {
      getCommonBase(leftContext.typeDeclaration, rightContext.typeDeclaration, context)
        .map(td => Right(ExprContext(isStatic = Some(false), td)))
        .getOrElse({
          Left(
            s"Incompatible types in ternary operation '${leftContext.typeName}' and '${rightContext.typeName}'"
          )
        })
    }
  }
}
