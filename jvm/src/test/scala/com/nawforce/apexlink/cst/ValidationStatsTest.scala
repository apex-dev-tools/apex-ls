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

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ValidationStatsTest extends AnyFunSuite with TestHelper {

  test("snapshot with no lookups has no hit rate") {
    val snapshot = ValidationStatsSnapshot(0, 0, 0)
    assert(snapshot.typeCacheLookups == 0)
    assert(snapshot.typeCacheHitRate.isEmpty)
  }

  test("hit rate is hits over lookups") {
    val snapshot = ValidationStatsSnapshot(typeCacheHits = 3, typeCacheMisses = 1, typeContexts = 2)
    assert(snapshot.typeCacheLookups == 4)
    assert(snapshot.typeCacheHitRate.contains(0.75))
  }

  test("reset clears the counters") {
    ValidationStats.recordTypeCacheHit()
    ValidationStats.recordTypeCacheMiss()
    ValidationStats.recordTypeContext()
    ValidationStats.reset()
    assert(ValidationStats.snapshot() == ValidationStatsSnapshot(0, 0, 0))
  }

  test("validating records contexts and resolutions") {
    ValidationStats.reset()
    happyTypeDeclaration("public class Dummy { void func() {String a; String b; a = b;} }")

    val snapshot = ValidationStats.snapshot()
    assert(snapshot.typeContexts > 0)
    assert(snapshot.typeCacheMisses > 0)
    assert(snapshot.typeCacheHits > 0)
    assert(snapshot.typeCacheLookups == snapshot.typeCacheHits + snapshot.typeCacheMisses)
    assert(snapshot.typeCacheHitRate.exists(rate => rate > 0 && rate < 1))
  }
}
