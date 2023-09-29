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

  /** Options for determining assignability
    * @param strictConversions limit implicit type conversions
    * @param narrowSObjects narrowing of SObject conversions, i.e. SObject cast to Account
    */
  case class AssignableOptions(strictConversions: Boolean, narrowSObjects: Boolean)

  object AssignableOptions {

    /** Most commonly used options */
    val default: AssignableOptions =
      AssignableOptions(strictConversions = false, narrowSObjects = true)
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
        (toType == TypeNames.SObject && context.module.isGhostedType(fromType) && fromType.outer
          .contains(TypeNames.Schema)) ||
        (toType == TypeNames.InternalObject && context.module.isGhostedType(fromType))
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
      isRecordSetAssignable(toType, fromType.typeName)
    } else if (toType.params.nonEmpty || fromType.typeName.params.nonEmpty) {
      isAssignableGeneric(toType, fromType, context)
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

    (fromType.name match {
      case Names.List$ | Names.Set$ =>
        canNarrowSObject(toParams.head, fromParams.head, context)
      case Names.Map$ =>
        canNarrowSObject(toParams(1), fromParams(1), context)
      case _ => false
    }) ||
    toParams
      .zip(fromParams)
      .map(p => isAssignable(p._1, p._2, context))
      .forall(b => b)
  }

  /* Test if an System.SObject can be cast to a specific SObject type. This conversion is generally unsafe but is
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

  /** Test if RecordSet in fromType is assignable to toType.
    * @param toType the type we are trying to assign to
    * @param fromType the RecordSet typeName, maybe over SObject or a specific SObject
    */
  private def isRecordSetAssignable(toType: TypeName, fromType: TypeName): Boolean = {
    // Where we don't know specific RecordSet SObject we need some flex in rules
    val fromSObjectType    = fromType.params.head
    val isSObjectRecordSet = fromSObjectType == TypeNames.SObject
    if (toType.isList || toType.isRecordSet) {
      // Assignment to List or RecordSet must be same type or SObject/Object
      val toObject = toType.params.head
      if (toObject == TypeNames.SObject || toObject == TypeNames.InternalObject)
        true
      else
        isSObjectRecordSet || toObject == fromSObjectType
    } else {
      // Assignment non-list/Recordset must be same type or SObject/Object
      if (toType == TypeNames.SObject || toType == TypeNames.InternalObject)
        true
      else
        isSObjectRecordSet || toType == fromSObjectType
    }
  }

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
