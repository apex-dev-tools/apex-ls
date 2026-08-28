/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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

import com.nawforce.apexlink.types.apex.ApexDeclaration
import com.nawforce.apexlink.types.core.{MethodDeclaration, TypeDeclaration}
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.PathLocation

import scala.collection.immutable.ArraySeq

/** A type explicitly written in Apex source along with the location of the final identifier of the
  * written name. A single type reference such as 'Map<String, Outer.Hidden[]>' yields an occurrence
  * for each written component that could denote a nested type, so that a diagnostic can be reported
  * against the component at fault rather than against the normalized collection wrapper.
  *
  * Only qualified names are recorded, see TypeReference; an unqualified name is either a top level
  * type or resolves to a nested type that is visible from where it is written.
  */
final case class SourceTypeOccurrence(typeName: TypeName, location: PathLocation)

object SourceTypeOccurrence {
  val empty: ArraySeq[SourceTypeOccurrence] = ArraySeq.empty

  /** Join two occurrence lists, keeping the shared empty instance when neither recorded anything. */
  def concat(
    left: ArraySeq[SourceTypeOccurrence],
    right: ArraySeq[SourceTypeOccurrence]
  ): ArraySeq[SourceTypeOccurrence] = {
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else left ++ right
  }

  /** Join occurrence lists, keeping the shared empty instance when none recorded anything. */
  def concat(parts: Seq[ArraySeq[SourceTypeOccurrence]]): ArraySeq[SourceTypeOccurrence] = {
    if (parts.forall(_.isEmpty)) empty else ArraySeq.from(parts.flatten)
  }
}

/** Validation of the accessibility of types that are explicitly written in Apex source.
  *
  * Lookup and dependency registration are deliberately left alone; this is a pure post-resolution
  * check so that a visibility failure does not disturb downstream analysis. Only nested Apex
  * declarations are examined, other declaration kinds (platform, SObject, Component, Page, ghosted,
  * synthetic) have their own visibility rules or none at all.
  */
object SourceTypeAccess {

  def validate(occurrences: ArraySeq[SourceTypeOccurrence], context: VerifyContext): Unit = {
    if (occurrences.nonEmpty && !context.suppressIssues)
      occurrences.foreach(occurrence => validate(occurrence.typeName, occurrence.location, context))
  }

  /** Validate a written type name, resolving it via the normal (cached) lookup. */
  def validate(typeName: TypeName, location: PathLocation, context: VerifyContext): Unit = {
    if (!context.suppressIssues)
      context
        .getTypeFor(typeName, context.thisType)
        .foreach(td => validate(td, location, context))
  }

  /** Validate the return type of a method resolved at a call site.
    *
    * The return type is not written in the calling source, so this is not a written occurrence; it
    * is checked against the resolved method in the way the org does. Only the return type itself is
    * examined and never its type arguments, matching the org, which accepts a 'List<Hidden>' return
    * where it rejects a bare 'Hidden' one.
    */
  def validateMethodReturnType(
    returnType: TypeDeclaration,
    method: MethodDeclaration,
    location: PathLocation,
    context: VerifyContext
  ): Unit = {
    returnType match {
      case nested: ApexDeclaration if nested.outerTypeName.nonEmpty && !context.suppressIssues =>
        if (
          !TestVisibleAccess.access(nested, Some(context.thisType)).isAccessible &&
          context.recordTypeVisibilityIssue(location, nested.typeId)
        )
          context.logError(
            location,
            s"Method return type ${nested.typeName} is not visible for: ${signatureOf(method)}"
          )
      case _ => ()
    }
  }

  /** The org qualifies the method name with its owning type in this diagnostic. */
  private def signatureOf(method: MethodDeclaration): String = {
    val owner = method.thisTypeIdOpt.map(typeId => s"${typeId.typeName}.").getOrElse("")
    s"${method.typeName} $owner${method.nameAndParameterTypes}"
  }

  /** Validate an already resolved type that was explicitly written at the given location. */
  def validate(td: TypeDeclaration, location: PathLocation, context: VerifyContext): Unit = {
    td match {
      case nested: ApexDeclaration if nested.outerTypeName.nonEmpty && !context.suppressIssues =>
        TestVisibleAccess
          .access(nested, Some(context.thisType))
          .errorMessage
          .foreach(message =>
            if (context.recordTypeVisibilityIssue(location, nested.typeId))
              context.logError(location, message)
          )
      case _ => ()
    }
  }
}
