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

package io.github.apexdevtools.apexls

import com.nawforce.apexlink.types.apex.FullDeclaration
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.diagnostics.LoggerOps
import org.scalatest.funsuite.AnyFunSuite

class LoadPhaseCollectorTest extends AnyFunSuite {

  test("known spans map to phases and unknown ones are dropped") {
    assert(LoadPhaseCollector.phaseOf("Org created").contains("orgCreate"))
    assert(LoadPhaseCollector.phaseOf("Workspace scanned").contains("workspaceScan"))
    assert(LoadPhaseCollector.phaseOf("Modules deployed").contains("moduleDeploy"))
    assert(LoadPhaseCollector.phaseOf("Parsed 12 classes").contains("classParseBatch"))
    assert(LoadPhaseCollector.phaseOf("Parsed 3 triggers").contains("triggerParseBatch"))
    assert(LoadPhaseCollector.phaseOf("Loaded summary classes").contains("summaryCacheLoad"))
    assert(
      LoadPhaseCollector.phaseOf("Closed plugins (unused analysis)").contains("unusedAnalysis")
    )
    assert(LoadPhaseCollector.phaseOf("Refreshed something").isEmpty)
  }

  test("validation decomposition spans map to their own phases") {
    assert(LoadPhaseCollector.phaseOf(TypeDeclaration.MethodMapSpan).contains("validateMethodMap"))
    assert(
      LoadPhaseCollector
        .phaseOf(TypeDeclaration.ConstructorMapSpan)
        .contains("validateConstructorMap")
    )
    assert(
      LoadPhaseCollector
        .phaseOf(FullDeclaration.BodyDeclarationSpan)
        .contains("validateBodyDeclarations")
    )
    assert(
      LoadPhaseCollector
        .phaseOf(FullDeclaration.OuterDependencySpan)
        .contains("validateOuterDependencies")
    )
  }

  test("decomposition spans are not swallowed by the per type validate span") {
    // "Validated " prefixed labels map to validateFile, so a decomposition label must not use it
    assert(!FullDeclaration.BodyDeclarationSpan.startsWith("Validated "))
    assert(!FullDeclaration.OuterDependencySpan.startsWith("Validated "))
    assert(!TypeDeclaration.MethodMapSpan.startsWith("Validated "))
    assert(!TypeDeclaration.ConstructorMapSpan.startsWith("Validated "))
  }

  test("spans naming a file are aggregated without keeping the path") {
    assert(LoadPhaseCollector.phaseOf("Parsed /private/src/Secret.cls").contains("classParseFile"))
    assert(LoadPhaseCollector.phaseOf("Validated /private/src/Secret.cls").contains("validateFile"))
    assert(LoadPhaseCollector.phaseOf("Indexer scanned /private/src").contains("indexerScan"))
  }

  test("timings accumulate per phase") {
    val collector = new LoadPhaseCollector
    collector.record("Parsed /a/A.cls", 1000000)
    collector.record("Parsed /a/B.cls", 3000000)
    collector.record("Ignoring directory /a/hidden", 9000000)

    val phases = collector.phases
    assert(phases.map(_.phase) == Seq("classParseFile"))
    assert(phases.head.count == 2)
    assert(phases.head.totalNanos == 4000000)
    assert(phases.head.maxNanos == 3000000)
    assert(collector.threadsFor("classParseFile") == 1)
    assert(collector.threadsFor("orgCreate") == 0)
  }

  test("an installed collector observes timed spans and is removable") {
    val collector = new LoadPhaseCollector
    val previous  = LoggerOps.setTimingCollector(Some(collector))
    try {
      LoggerOps.debugTime("Workspace scanned", show = false) { () }
    } finally {
      LoggerOps.setTimingCollector(previous)
    }
    LoggerOps.debugTime("Modules deployed", show = false) { () }

    assert(collector.phases.map(_.phase) == Seq("workspaceScan"))
  }
}
