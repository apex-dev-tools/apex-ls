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

import com.nawforce.apexlink.api.{Org, ServerOps}
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.apexlink.types.apex.{FullDeclaration, SummaryDeclaration}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Environment
import org.scalatest.funsuite.AnyFunSuite

/** Nested type visibility must behave the same whether the target declaration is parsed (Full) or
  * loaded from the parsed cache (Summary), and must track refreshes of either file.
  */
class NestedTypeVisibilityCacheTest extends AnyFunSuite {

  private val hiddenTarget = "global class Target { private class Hidden {} }"
  private val shownTarget  = "global class Target { public class Shown {} private class Hidden {} }"
  private val visibleTarget = "global class Target { @TestVisible private class Hidden {} }"

  private val hiddenCaller = "global class Dummy { Target.Hidden a; }"
  private val testCaller   = "@isTest global class Dummy { Target.Hidden a; }"
  private val shownCaller  = "global class Dummy { Target.Shown a; }"

  private val sources = Map("Target.cls" -> hiddenTarget, "Dummy.cls" -> hiddenCaller)

  private def withIsolatedRuntime[T](op: => T): T = {
    val originalCache     = Environment.getCacheDirOverride
    val originalAutoFlush = ServerOps.isAutoFlushEnabled
    Environment.setCacheDirOverride(Some(None))
    try op
    finally {
      Environment.setCacheDirOverride(originalCache)
      ServerOps.setAutoFlush(originalAutoFlush)
    }
  }

  private def openOrg(root: PathLike, cacheEnabled: Boolean): OPM.OrgImpl = {
    val options = OpenOptions
      .default()
      .withAutoFlush(enabled = false)
      .withCacheDirectory(if (cacheEnabled) root.join(".cache").toString else "")
      .withCache(cacheEnabled)
    Org.newOrg(root, options).asInstanceOf[OPM.OrgImpl]
  }

  private def visibilityIssues(org: OPM.OrgImpl, root: PathLike): Seq[String] =
    org.issueManager
      .issuesForFileInternal(root.join("Dummy.cls"))
      .map(_.diagnostic.message)
      .filter(_.contains("Type is not visible"))

  private def declaration(org: OPM.OrgImpl, name: String): Any =
    org.unmanaged.orderedModules.flatMap(_.findModuleType(TypeName(Name(name)))).head

  private def refresh(org: OPM.OrgImpl, root: PathLike, file: String, content: String): Unit = {
    root.join(file).write(content)
    org.unmanaged.refreshAll(Array(root.join(file)))
    org.flush()
  }

  test("cache disabled reports against a Full target declaration") {
    withIsolatedRuntime {
      FileSystemHelper.runTempDir(sources) { root: PathLike =>
        val org = openOrg(root, cacheEnabled = false)
        assert(declaration(org, "Target").isInstanceOf[FullDeclaration])
        assert(visibilityIssues(org, root) == Seq("Type is not visible: Target.Hidden"))
      }
    }
  }

  test("cache enabled reports identically against a Summary target declaration") {
    withIsolatedRuntime {
      FileSystemHelper.runTempDir(sources, setupCache = true) { root: PathLike =>
        val cold = openOrg(root, cacheEnabled = true)
        assert(declaration(cold, "Target").isInstanceOf[FullDeclaration])
        assert(visibilityIssues(cold, root) == Seq("Type is not visible: Target.Hidden"))
        cold.flush()

        val warm = openOrg(root, cacheEnabled = true)
        assert(declaration(warm, "Target").isInstanceOf[SummaryDeclaration])
        assert(visibilityIssues(warm, root) == Seq("Type is not visible: Target.Hidden"))
      }
    }
  }

  test("same file access holds across Full and Summary loading") {
    withIsolatedRuntime {
      val sameFile = Map(
        "Dummy.cls" ->
          "global class Dummy { private class Hidden {} class Peer { Dummy.Hidden a; } }"
      )
      FileSystemHelper.runTempDir(sameFile, setupCache = true) { root: PathLike =>
        val cold = openOrg(root, cacheEnabled = true)
        assert(visibilityIssues(cold, root).isEmpty)
        cold.flush()

        val warm = openOrg(root, cacheEnabled = true)
        assert(declaration(warm, "Dummy").isInstanceOf[SummaryDeclaration])
        assert(visibilityIssues(warm, root).isEmpty)
      }
    }
  }

  test("refreshing the target visibility adds and removes the diagnostic") {
    withIsolatedRuntime {
      FileSystemHelper.runTempDir(sources, setupCache = true) { root: PathLike =>
        val org = openOrg(root, cacheEnabled = true)
        assert(visibilityIssues(org, root) == Seq("Type is not visible: Target.Hidden"))

        refresh(org, root, "Target.cls", visibleTarget)
        assert(visibilityIssues(org, root) == Seq("Type is not visible: Target.Hidden"))

        refresh(org, root, "Dummy.cls", testCaller)
        assert(visibilityIssues(org, root).isEmpty)

        refresh(org, root, "Target.cls", hiddenTarget)
        assert(visibilityIssues(org, root) == Seq("Type is not visible: Target.Hidden"))

        refresh(org, root, "Dummy.cls", hiddenCaller)
        assert(visibilityIssues(org, root) == Seq("Type is not visible: Target.Hidden"))
      }
    }
  }

  test("refreshing to an accessible nested type clears the diagnostic") {
    withIsolatedRuntime {
      FileSystemHelper.runTempDir(sources, setupCache = true) { root: PathLike =>
        val org = openOrg(root, cacheEnabled = true)
        assert(visibilityIssues(org, root) == Seq("Type is not visible: Target.Hidden"))

        refresh(org, root, "Target.cls", shownTarget)
        refresh(org, root, "Dummy.cls", shownCaller)
        assert(visibilityIssues(org, root).isEmpty)
      }
    }
  }
}
