/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types.base

import com.financialforce.types.ArrayInternCache

/** Modifier element, text is case-insensitive. */
case class Modifier(text: String) {
  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Modifier]
    text.equalsIgnoreCase(other.text)
  }

  override def toString: String = text
}

/** Caching support for Arrays of modifiers. */
object Modifier {
  final val emptyArray = Array[Modifier]()

  private val cache = new ArrayInternCache[Modifier]()

  def intern(modifiers: Array[Modifier]): Array[Modifier] = {
    cache.intern(modifiers)
  }
}
