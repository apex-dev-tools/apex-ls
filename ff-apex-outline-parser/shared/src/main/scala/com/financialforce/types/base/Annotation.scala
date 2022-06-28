/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.types.base

import com.financialforce.types.ArrayInternCache

/** Annotation element, name is case-insensitive, parameters are unparsed. */
case class Annotation(name: String, parameters: Option[String]) {
  override def toString: String = {
    if (parameters.isDefined) s"@$name(${parameters.get})" else s"@$name"
  }
}

/** Caching support for Arrays of annotations. */
object Annotation {
  final val emptyArray = Array[Annotation]()

  private val cache = new ArrayInternCache[Annotation]()

  def intern(annotations: Array[Annotation]): Array[Annotation] = {
    cache.intern(annotations)
  }
}
