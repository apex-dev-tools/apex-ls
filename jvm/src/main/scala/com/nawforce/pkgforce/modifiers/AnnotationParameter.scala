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
package com.nawforce.pkgforce.modifiers

import com.nawforce.pkgforce.path.Location

import scala.collection.compat.immutable.ArraySeq

/** Separator written between two annotation parameters.
  *
  * Apex accepts whitespace alone here, a comma is recorded rather than rejected so that it can be
  * diagnosed against the parameter that follows it.
  */
sealed abstract class AnnotationParameterSeparator(val text: String) {
  override def toString: String = text
}

object AnnotationParameterSeparator {

  /** Whitespace, the only separator the platform compiler accepts. */
  case object Whitespace extends AnnotationParameterSeparator(" ")

  /** A comma, which the platform compiler rejects. */
  case object Comma extends AnnotationParameterSeparator(",")
}

/** A single annotation parameter, as it was written.
  *
  * `name` is set only for the `name = value` form, it is empty for a bare value such as the
  * argument of `@SuppressWarnings('PMD')`. Values are left uninterpreted, quotes and all, nothing
  * here establishes whether the name or the value is legal for the annotation.
  *
  * `precedingSeparator` is the separator written between this parameter and the one before it, it
  * is empty for the first parameter of a list.
  */
final case class AnnotationParameter(
  name: Option[String],
  value: String,
  precedingSeparator: Option[AnnotationParameterSeparator] = None,
  nameLocation: Option[Location] = None,
  valueLocation: Option[Location] = None,
  location: Option[Location] = None
) {
  override def toString: String = name.map(n => s"$n=$value").getOrElse(value)
}

object AnnotationParameter {
  final val emptyArraySeq: ArraySeq[AnnotationParameter] = ArraySeq()
}
