/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types.base

/** Dot seperated name without type arguments. */
case class QualifiedName(parts: Array[IdWithLocation]) {
  def location: Location = {
    val start = parts.head.location
    val end   = parts.last.location
    Location.span(start, end)
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[QualifiedName]
    parts.sameElements(other.parts)
  }

  override def toString: String = {
    parts.map(_.toString).mkString(".")
  }
}
