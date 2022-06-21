/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

package object types {

  private[types] implicit class StringOps(value: String) {
    /* Improve a formatted string by removing leading/trailing & duplicate whitespaces. */
    def tidyWhitespace: String = value.trim.replaceAll(" +", " ")
  }

  /** Simple cache for interning Arrays of values. This needs special handling as Array uses reference equality. */
  private[types] class ArrayInternCache[T] {
    private var cache = mutable.HashMap[ArraySeq[T], Array[T]]()

    def intern(value: Array[T]): Array[T] = {
      val wrapped = ArraySeq.unsafeWrapArray(value)
      cache.getOrElseUpdate(wrapped, value)
    }

    def clean(): Unit = {
      cache = new mutable.HashMap()
    }
  }
}
