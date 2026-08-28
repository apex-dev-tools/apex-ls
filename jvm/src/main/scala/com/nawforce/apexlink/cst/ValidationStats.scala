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

import java.util.concurrent.atomic.LongAdder

/** Counts of the type resolution performed while validating.
  *
  * TypeVerifyContext caches resolutions in a map scoped to the single type being validated, so the
  * same type resolved from two types is resolved twice. These counters size that: how often the
  * cache answers a lookup, and how many contexts the work is spread over.
  *
  * Counting is always on. It is a pair of LongAdder increments on a path that already allocates a
  * tuple key, and it records nothing derived from source, so there is nothing to gate. Callers that
  * want a figure for one load call reset() first, which is not safe while a load is running.
  */
object ValidationStats {
  private val typeCacheHits   = new LongAdder()
  private val typeCacheMisses = new LongAdder()
  private val typeContexts    = new LongAdder()

  def recordTypeCacheHit(): Unit = typeCacheHits.increment()

  def recordTypeCacheMiss(): Unit = typeCacheMisses.increment()

  def recordTypeContext(): Unit = typeContexts.increment()

  def reset(): Unit = {
    typeCacheHits.reset()
    typeCacheMisses.reset()
    typeContexts.reset()
  }

  def snapshot(): ValidationStatsSnapshot =
    ValidationStatsSnapshot(typeCacheHits.sum(), typeCacheMisses.sum(), typeContexts.sum())
}

/** Type resolution counts taken at a point in time. */
final case class ValidationStatsSnapshot(
  typeCacheHits: Long,
  typeCacheMisses: Long,
  typeContexts: Long
) {

  /** Lookups served, a miss being one that reached TypeResolver. */
  def typeCacheLookups: Long = typeCacheHits + typeCacheMisses

  /** Proportion of lookups the per type cache answered, None when nothing was resolved. */
  def typeCacheHitRate: Option[Double] =
    Option.when(typeCacheLookups > 0)(typeCacheHits.toDouble / typeCacheLookups.toDouble)
}
