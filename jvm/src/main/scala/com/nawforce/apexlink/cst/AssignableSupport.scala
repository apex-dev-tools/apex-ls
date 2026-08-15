/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.{Names, TypeName}

/** Rules for determining if one type is assignable to another */
object AssignableSupport {

  /** How Apex converts a RecordSet argument to a selected parameter. The overload rank is lower
    * for conversions Apex prefers when multiple overloads are otherwise compatible.
    */
  sealed trait RecordSetConversion {
    def overloadRank: Int
  }

  final case class PreservedRecordSet(overloadRank: Int)    extends RecordSetConversion
  final case class PreservedCollection(overloadRank: Int)   extends RecordSetConversion
  final case class ScalarSObjectCoercion(overloadRank: Int) extends RecordSetConversion
  final case class PreservedObject(overloadRank: Int)       extends RecordSetConversion

  /** Options for determining assignability
    * @param strictConversions limit implicit type conversions
    * @param narrowSObjects narrowing of SObject conversions, i.e. SObject cast to Account
    * @param invariantSet require identical Set type parameters
    */
  case class AssignableOptions(
    strictConversions: Boolean,
    narrowSObjects: Boolean,
    invariantSet: Boolean = false
  )

  object AssignableOptions {

    /** Most commonly used options */
    val default: AssignableOptions =
      AssignableOptions(strictConversions = false, narrowSObjects = true)

    /** Options for assignment and return validation, where Apex treats Set type parameters as
      * invariant.
      */
    val assignment: AssignableOptions =
      AssignableOptions(strictConversions = false, narrowSObjects = true, invariantSet = true)
  }

  /** Determine if two values could be equal based on type
    *
    * Where possible prefer [[isAssignableDeclaration]] to avoid type resolution
    * @param aType one of the types
    * @param bType the other one
    * @param context context of evaluation
    */
  def couldBeEqual(
    aType: TypeDeclaration,
    bType: TypeDeclaration,
    context: VerifyContext
  ): Boolean = {
    isAssignableDeclaration(aType.typeName, bType, context) ||
    isAssignableDeclaration(bType.typeName, aType, context)
  }

  /** Determine if value of a type can be assigned to another type
    *
    * Where possible prefer [[isAssignableDeclaration]] to avoid type resolution
    * @param toType type to assign to
    * @param fromType type to assign from
    * @param context context of assignment
    * @param options options for type of assignment
    */
  def isAssignable(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext,
    options: AssignableOptions = AssignableOptions.default
  ): Boolean = {
    context.getTypeFor(fromType, context.thisType) match {
      case Left(_) =>
        // Allow some ghosted assignments to support Lists
        // Exact match, assigning to Object or to SObject given a Schema type
        context.module.isGhostedType(fromType) && (toType == fromType ||
          toType == TypeNames.InternalObject || (
            toType == TypeNames.SObject && fromType.outer.contains(TypeNames.Schema)
          ))
      case Right(fromDeclaration) =>
        isAssignableDeclaration(toType, fromDeclaration, context, options)
    }
  }

  /** Determine if value of a type declaration can be assigned to another type
    * @param toType   type to assign to
    * @param fromType type declaration to assign from
    * @param context  context of assignment
    * @param options  options for type of assignment
    */
  def isAssignableDeclaration(
    toType: TypeName,
    fromType: TypeDeclaration,
    context: VerifyContext,
    options: AssignableOptions = AssignableOptions.default
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
      isRecordSetAssignable(toType, fromType.typeName, context)
    } else if (toType.params.nonEmpty || fromType.typeName.params.nonEmpty) {
      isAssignableGeneric(toType, fromType, context, options)
    } else {
      (if (options.strictConversions)
         strictAssignable.contains(toType, fromType.typeName)
       else
         looseAssignable.contains(toType, fromType.typeName)) ||
      canNarrowSObject(toType, fromType.typeName, context, options) ||
      fromType.extendsOrImplements(toType)
    }
  }

  private def isAssignableGeneric(
    toType: TypeName,
    fromType: TypeDeclaration,
    context: VerifyContext,
    options: AssignableOptions
  ): Boolean = {
    if (toType.params.size == fromType.typeName.params.size) {
      isAssignableName(toType, fromType) && hasAssignableGenericParams(
        toType,
        fromType.typeName,
        context,
        options
      )
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

  /** Check if generic type parameters are assignable with collection-specific rules. */
  private def hasAssignableGenericParams(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext,
    options: AssignableOptions
  ): Boolean = {
    if (options.invariantSet && toType.name == Names.Set$ && fromType.name == Names.Set$)
      return toType.params == fromType.params

    // SObject narrowing is supported on List & Set but not Map
    checkGenericParameterAssignability(
      toType.params,
      fromType.params,
      context,
      narrowSObjects = fromType.name != Names.Map$
    )
  }

  /** Check generic type parameter assignability with default rules. */
  private def checkGenericParameterAssignability(
    toParams: Seq[TypeName],
    fromParams: Seq[TypeName],
    context: VerifyContext,
    narrowSObjects: Boolean = true
  ): Boolean = {
    toParams
      .zip(fromParams)
      .map { case (toParam, fromParam) =>
        isAssignable(
          toParam,
          fromParam,
          context,
          AssignableOptions(strictConversions = false, narrowSObjects)
        )
      }
      .forall(identity)
  }

  /* Test if a System.SObject can be cast to a specific SObject type. This conversion is generally unsafe but is
   * supported in various (but not all) places in Apex. */
  private def canNarrowSObject(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext,
    options: AssignableOptions = AssignableOptions.default
  ): Boolean = {
    if (
      options.narrowSObjects &&
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
      isAssignable(toType.params.head, TypeNames.SObject, context, AssignableOptions.default)
    } else {
      false
    }
  }

  /** Classify conversion of a RecordSet to a target type.
    * @param toType the type we are trying to assign to
    * @param fromType the RecordSet typeName, maybe over SObject or a specific SObject
    * @param context context used to identify scalar SObject targets
    */
  def recordSetConversion(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext
  ): Option[RecordSetConversion] = {
    if (!fromType.isRecordSet || fromType.params.isEmpty)
      return None

    // Where we don't know specific RecordSet SObject we need some flex in rules
    val fromSObjectType    = fromType.params.head
    val isSObjectRecordSet = fromSObjectType == TypeNames.SObject
    if (toType.isList || toType.isRecordSet) {
      val toObject = toType.params.head
      val rank =
        if (toObject == fromSObjectType || isSObjectRecordSet) 0
        else if (toObject == TypeNames.SObject) 1
        else if (toObject == TypeNames.InternalObject) 4
        else return None
      if (toType.isRecordSet) Some(PreservedRecordSet(rank))
      else Some(PreservedCollection(rank))
    } else if (toType == TypeNames.InternalObject) {
      Some(PreservedObject(5))
    } else {
      val isScalarSObject =
        toType.params.isEmpty && (toType == TypeNames.SObject || context
          .getTypeFor(toType, context.thisType)
          .toOption
          .exists(_.isSObject))
      if (
        !isScalarSObject || (!isSObjectRecordSet && toType != fromSObjectType && toType != TypeNames.SObject)
      )
        None
      else if (toType == TypeNames.SObject)
        Some(ScalarSObjectCoercion(3))
      else
        Some(ScalarSObjectCoercion(2))
    }
  }

  private def isRecordSetAssignable(
    toType: TypeName,
    fromType: TypeName,
    context: VerifyContext
  ): Boolean = recordSetConversion(toType, fromType, context).nonEmpty

  private val strictAssignable: Set[(TypeName, TypeName)] =
    Set(
      (TypeNames.Long, TypeNames.Integer),
      (TypeNames.Decimal, TypeNames.Integer),
      (TypeNames.Decimal, TypeNames.Long),
      (TypeNames.String, TypeNames.IdType),
      (TypeNames.Datetime, TypeNames.Date)
    )

  private val looseAssignable: Set[(TypeName, TypeName)] = {
    strictAssignable ++
      Set(
        (TypeNames.Double, TypeNames.Integer),
        (TypeNames.Double, TypeNames.Long),
        (TypeNames.Double, TypeNames.Decimal),
        (TypeNames.Decimal, TypeNames.Double),
        (TypeNames.IdType, TypeNames.String)
      )
  }

}
