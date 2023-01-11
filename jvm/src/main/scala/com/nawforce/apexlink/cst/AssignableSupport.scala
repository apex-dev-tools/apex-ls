package com.nawforce.apexlink.cst

import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.{Names, TypeName}

object AssignableSupport {

  implicit class AssignableName(fromType: TypeName) {

    def isAssignableTo(toType: TypeName, strict: Boolean, context: VerifyContext): Boolean =
      isAssignable(
        toType,
        fromType,
        AssignableOptions(strictConversions = strict, disableSObjectNarrowing = false),
        context
      )

  }

  implicit class AssignableType(fromType: TypeDeclaration) {

    def isAssignableTo(toType: TypeName, strict: Boolean, context: VerifyContext): Boolean =
      isAssignable(
        toType,
        fromType,
        AssignableOptions(strictConversions = strict, disableSObjectNarrowing = false),
        context
      )

    def couldBeEqualTo(toType: TypeDeclaration, context: VerifyContext): Boolean =
      couldBeEqual(toType, fromType, context)

  }

  case class AssignableOptions(
    strictConversions: Boolean, // When enabled limits allowed implicit type conversions
    disableSObjectNarrowing: Boolean // When enabled disallows narrowing of SObject conversions, i.e. Account->SObject
  )

  def couldBeEqual(
    toType: TypeDeclaration,
    fromType: TypeDeclaration,
    context: VerifyContext
  ): Boolean = {
    val options = AssignableOptions(strictConversions = false, disableSObjectNarrowing = false)
    isAssignable(toType.typeName, fromType, options, context) ||
    isAssignable(fromType.typeName, toType, options, context)
  }

  def isAssignable(
    toType: TypeName,
    fromType: TypeName,
    strictConversions: Boolean,
    context: VerifyContext
  ): Boolean = {
    isAssignable(
      toType,
      fromType,
      AssignableOptions(strictConversions, disableSObjectNarrowing = false),
      context
    )
  }

  def isAssignable(
    toType: TypeName,
    fromType: TypeName,
    options: AssignableOptions,
    context: VerifyContext
  ): Boolean = {
    context.getTypeFor(fromType, context.thisType) match {
      case Left(_) =>
        // Allow some ghosted assignments to support Lists
        (toType == TypeNames.SObject && context.module.isGhostedType(fromType) && fromType.outer
          .contains(TypeNames.Schema)) ||
        (toType == TypeNames.InternalObject && context.module.isGhostedType(fromType))
      case Right(fromDeclaration) =>
        isAssignable(toType, fromDeclaration, options, context)
    }
  }

  def isAssignable(
    toType: TypeName,
    fromType: TypeDeclaration,
    strictConversions: Boolean,
    context: VerifyContext
  ): Boolean = {
    isAssignable(
      toType,
      fromType,
      AssignableOptions(strictConversions, disableSObjectNarrowing = false),
      context
    )
  }

  def isAssignable(
    toType: TypeName,
    fromType: TypeDeclaration,
    options: AssignableOptions,
    context: VerifyContext
  ): Boolean = {
    if (
      fromType.typeName == TypeNames.Null ||
      fromType.typeName == TypeNames.Any ||
      fromType.typeName == toType ||
      (!options.strictConversions && toType == TypeNames.InternalObject) ||
      context.module.isGhostedType(toType)
    ) {
      true
    } else if (!options.strictConversions && fromType.typeName.isRecordSet) {
      isRecordSetAssignable(toType, context)
    } else if (toType.params.nonEmpty || fromType.typeName.params.nonEmpty) {
      isAssignableGeneric(toType, fromType, context)
    } else {
      (if (options.strictConversions)
         strictAssignable.contains(toType, fromType.typeName)
       else
         looseAssignable.contains(toType, fromType.typeName)) ||
      isSObjectNarrowable(toType, fromType.typeName, options, context) ||
      fromType.extendsOrImplements(toType)
    }
  }

  private def isAssignableGeneric(
    toType: TypeName,
    fromType: TypeDeclaration,
    context: VerifyContext
  ): Boolean = {
    if (toType.params.size == fromType.typeName.params.size) {
      isAssignableName(toType, fromType) && hasAssignableParams(toType, fromType.typeName, context)
    } else if (toType.params.isEmpty || fromType.typeName.params.isEmpty) {
      // e.g. Object a = List<A> | Iterable<A> a = new CustomIterator() | Iterable<A> a = QueryLocator
      fromType.extendsOrImplements(toType) ||
      isQueryLocatorAssignable(toType, fromType.typeName, context)
    } else {
      false
    }
  }

  private def isAssignableName(toType: TypeName, fromType: TypeDeclaration): Boolean = {
    val sameParams = matchGenericType(toType, fromType.typeName)
    fromType.typeName == sameParams || fromType.extendsOrImplements(sameParams)
  }

  private def matchGenericType(toType: TypeName, fromType: TypeName): TypeName = {
    val likeType = toType.withParams(fromType.params)
    if (toType.isIterable && fromType.isList) {
      // Workaround for Iterable i = List
      likeType.withName(Names.List$)
    } else {
      likeType
    }
  }

  private def hasAssignableParams(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext
  ): Boolean = {
    val toParams   = toType.params
    val fromParams = fromType.params
    val options    = AssignableOptions(strictConversions = false, disableSObjectNarrowing = false)

    (fromType.name match {
      case Names.List$ | Names.Set$ =>
        isSObjectNarrowable(toParams.head, fromParams.head, options, context)
      case Names.Map$ => isSObjectNarrowable(toParams(1), fromParams(1), options, context)
      case _          => false
    }) ||
    toParams
      .zip(fromParams)
      .map(p => isAssignable(p._1, p._2, options, context))
      .forall(b => b)
  }

  /* Test if an System.SObject can be cast to a specific SObject type. This conversion is generally unsafe but is
   * supported in various (but not all) places in Apex. */
  private def isSObjectNarrowable(
    toType: TypeName,
    fromType: TypeName,
    options: AssignableOptions,
    context: VerifyContext
  ): Boolean = {
    if (
      !options.disableSObjectNarrowing &&
      fromType == TypeNames.SObject &&
      toType != TypeNames.SObject
    ) {
      context.getTypeFor(toType, context.thisType) match {
        case Left(_)              => false
        case Right(toDeclaration) => toDeclaration.isSObject
      }
    } else {
      false
    }
  }

  private def isQueryLocatorAssignable(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext
  ): Boolean = {
    if (fromType == TypeNames.QueryLocator && toType.isIterable && toType.params.nonEmpty) {
      isAssignable(
        toType.params.head,
        TypeNames.SObject,
        AssignableOptions(strictConversions = false, disableSObjectNarrowing = false),
        context
      )
    } else {
      false
    }
  }

  @scala.annotation.tailrec
  private def isRecordSetAssignable(toType: TypeName, context: VerifyContext): Boolean = {
    if (toType == TypeNames.SObject || toType.isSObjectList || toType.isObjectList) {
      true
    } else if (toType.isList) {
      isRecordSetAssignable(toType.params.head, context)
    } else {
      context.getTypeFor(toType, context.thisType) match {
        case Left(_)              => false
        case Right(toDeclaration) => toDeclaration.isSObject
      }
    }
  }

  private lazy val strictAssignable: Set[(TypeName, TypeName)] =
    Set(
      (TypeNames.Long, TypeNames.Integer),
      (TypeNames.Decimal, TypeNames.Integer),
      (TypeNames.Decimal, TypeNames.Long),
      (TypeNames.String, TypeNames.IdType),
      (TypeNames.Datetime, TypeNames.Date)
    )

  private lazy val looseAssignable: Set[(TypeName, TypeName)] =
    Set(
      (TypeNames.Long, TypeNames.Integer),
      (TypeNames.Decimal, TypeNames.Integer),
      (TypeNames.Double, TypeNames.Integer),
      (TypeNames.Decimal, TypeNames.Long),
      (TypeNames.Double, TypeNames.Long),
      (TypeNames.Double, TypeNames.Decimal),
      (TypeNames.Decimal, TypeNames.Double),
      (TypeNames.IdType, TypeNames.String),
      (TypeNames.String, TypeNames.IdType),
      (TypeNames.Datetime, TypeNames.Date)
    )

}
