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
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.PathLocation

import scala.collection.immutable.ArraySeq

/** A type explicitly written in Apex source along with the location of the final identifier of the
  * written name. A single type reference such as 'Map<String, Outer.Hidden[]>' yields an occurrence
  * for each written component so that a diagnostic can be reported against the component at fault
  * rather than against the normalized collection wrapper.
  */
final case class SourceTypeOccurrence(typeName: TypeName, location: PathLocation)

object SourceTypeOccurrence {
  val empty: ArraySeq[SourceTypeOccurrence] = ArraySeq.empty
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
