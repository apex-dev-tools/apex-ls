/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Simple cache for interning values. For Arrays see ArrayInternCache. */
class InternCache[T] {
  private var cache = mutable.HashMap[T, T]()

  def intern(value: T): T = {
    cache.getOrElseUpdate(value, value)
  }

  def clean(): Unit = {
    cache = new mutable.HashMap()
  }
}

/** Simple cache for interning Arrays of values. This needs special handling as Array uses reference equality. */
class ArrayInternCache[T] {
  private var cache = mutable.HashMap[ArraySeq[T], Array[T]]()

  def intern(value: Array[T]): Array[T] = {
    val wrapped = ArraySeq.unsafeWrapArray(value)
    cache.getOrElseUpdate(wrapped, value)
  }

  def clean(): Unit = {
    cache = new mutable.HashMap()
  }
}
