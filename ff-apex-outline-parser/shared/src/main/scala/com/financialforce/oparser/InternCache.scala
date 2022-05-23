/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.collection.mutable

/** Simple cache for interning values */
class InternCache[T] {
  private var cache = mutable.HashMap[T, T]()

  def intern(value: T): T = {
    cache.getOrElseUpdate(value, value)
  }

  def clean(): Unit = {
    cache = new mutable.HashMap[T, T]()
  }
}
