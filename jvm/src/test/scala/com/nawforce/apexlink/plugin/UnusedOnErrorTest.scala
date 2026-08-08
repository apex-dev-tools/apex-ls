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
package com.nawforce.apexlink.plugin

import com.nawforce.apexlink.api.{Org, ServerOps}
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.apexlink.types.apex.{FullDeclaration, SummaryDeclaration}
import com.nawforce.pkgforce.diagnostics.{DiagnosticCategory, Issue, UNUSED_CATEGORY}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Environment
import org.scalatest.funsuite.AnyFunSuite

class UnusedOnErrorTest extends AnyFunSuite {

  private val validSources = Map(
    "LocalTarget.cls" ->
      "public class LocalTarget { public void run() { String unusedLocal; Integer value = 1; System.debug(value); } }",
    "MemberTarget.cls" ->
      "public class MemberTarget { public void unusedMember() {} { Integer value = 1; System.debug(value); } }",
    "MemberHolder.cls" ->
      "public class MemberHolder { { Type held = MemberTarget.class; System.debug(held); } }",
    "TypeTarget.cls" ->
      "public class TypeTarget { Object unusedField; { Integer value = 1; System.debug(value); } }"
  )

  private val errorSources = validSources ++ Map(
    "LocalTarget.cls" ->
      "public static class LocalTarget { public void run() { String unusedLocal; Integer value = 1; System.debug(value); } }",
    "MemberTarget.cls" ->
      "public static class MemberTarget { public void unusedMember() {} { Integer value = 1; System.debug(value); } }",
    "TypeTarget.cls" ->
      "public static class TypeTarget { Object unusedField; { Integer value = 1; System.debug(value); } }"
  )

  private val targetFiles = Seq("LocalTarget.cls", "MemberTarget.cls", "TypeTarget.cls")
  private val targetTypes = targetFiles.map(_.stripSuffix(".cls"))

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

  private def openOrg(
    root: PathLike,
    unusedOnError: Option[Boolean],
    cacheEnabled: Boolean,
    unusedEnabled: Boolean = true
  ): OPM.OrgImpl = {
    var options = OpenOptions
      .default()
      .withAutoFlush(enabled = false)
      .withCacheDirectory(if (cacheEnabled) root.join(".cache").toString else "")
      .withCache(cacheEnabled)
      .withUnused(unusedEnabled)
    unusedOnError.foreach(enabled => options = options.withUnusedOnError(enabled))
    Org.newOrg(root, options).asInstanceOf[OPM.OrgImpl]
  }

  private def issuesFor(org: OPM.OrgImpl, root: PathLike, file: String): Seq[Issue] =
    org.issueManager.issuesForFileInternal(root.join(file))

  private def unusedMessages(org: OPM.OrgImpl, root: PathLike, file: String): Seq[String] =
    issuesFor(org, root, file)
      .filter(_.diagnostic.category == UNUSED_CATEGORY)
      .map(_.diagnostic.message)
      .sorted

  private def assertHasErrors(org: OPM.OrgImpl, root: PathLike): Unit =
    targetFiles.foreach(file =>
      assert(
        issuesFor(org, root, file)
          .exists(issue => DiagnosticCategory.isErrorType(issue.diagnostic.category)),
        s"Expected an error in $file"
      )
    )

  private def assertExpectedUnused(org: OPM.OrgImpl, root: PathLike): Unit = {
    assert(unusedMessages(org, root, "LocalTarget.cls").exists(_.contains("Unused local variable")))
    assert(unusedMessages(org, root, "MemberTarget.cls").exists(_.contains("unusedMember")))
    assert(unusedMessages(org, root, "TypeTarget.cls").contains("Unused class 'TypeTarget'"))
  }

  private def assertNoUnused(org: OPM.OrgImpl, root: PathLike): Unit =
    targetFiles.foreach(file => assert(unusedMessages(org, root, file).isEmpty, file))

  private def declaration(org: OPM.OrgImpl, name: String): Any =
    org.unmanaged.orderedModules
      .flatMap(_.findModuleType(TypeName(Name(name))))
      .head

  test("errors suppress local, member, and type unused by default but opt-in retains them") {
    withIsolatedRuntime {
      FileSystemHelper.run(errorSources) { root: PathLike =>
        val defaultOrg = openOrg(root, unusedOnError = None, cacheEnabled = false)
        assertHasErrors(defaultOrg, root)
        assertNoUnused(defaultOrg, root)

        val explicitDefault = openOrg(root, unusedOnError = Some(false), cacheEnabled = false)
        assertHasErrors(explicitDefault, root)
        assertNoUnused(explicitDefault, root)

        val optedIn = openOrg(root, unusedOnError = Some(true), cacheEnabled = false)
        assertHasErrors(optedIn, root)
        assertExpectedUnused(optedIn, root)
      }
    }
  }

  test("without errors default and opt-in unused findings are identical") {
    withIsolatedRuntime {
      FileSystemHelper.run(validSources) { root: PathLike =>
        val defaultOrg = openOrg(root, unusedOnError = None, cacheEnabled = false)
        val optedIn    = openOrg(root, unusedOnError = Some(true), cacheEnabled = false)

        targetFiles.foreach(file =>
          assert(unusedMessages(defaultOrg, root, file) == unusedMessages(optedIn, root, file))
        )
        assertExpectedUnused(defaultOrg, root)
      }
    }
  }

  test("cache summaries preserve default and opt-in behavior from a default-policy cache") {
    withIsolatedRuntime {
      FileSystemHelper.runTempDir(errorSources) { root: PathLike =>
        val uncachedOptIn = openOrg(root, unusedOnError = Some(true), cacheEnabled = false)
        val expected =
          targetFiles.map(file => file -> unusedMessages(uncachedOptIn, root, file)).toMap
        targetTypes.foreach(name =>
          assert(declaration(uncachedOptIn, name).isInstanceOf[FullDeclaration])
        )
        assertExpectedUnused(uncachedOptIn, root)

        val cacheWriter = openOrg(root, unusedOnError = None, cacheEnabled = true)
        assertNoUnused(cacheWriter, root)
        cacheWriter.flush()

        val warmDefault = openOrg(root, unusedOnError = None, cacheEnabled = true)
        targetTypes.foreach(name =>
          assert(declaration(warmDefault, name).isInstanceOf[SummaryDeclaration])
        )
        assertHasErrors(warmDefault, root)
        assertNoUnused(warmDefault, root)

        val warmOptIn = openOrg(root, unusedOnError = Some(true), cacheEnabled = true)
        targetTypes.foreach(name =>
          assert(declaration(warmOptIn, name).isInstanceOf[SummaryDeclaration])
        )
        assertHasErrors(warmOptIn, root)
        targetFiles.foreach(file =>
          assert(unusedMessages(warmOptIn, root, file) == expected(file), file)
        )
        assertExpectedUnused(warmOptIn, root)

        val warmDisabled =
          openOrg(root, unusedOnError = Some(true), cacheEnabled = true, unusedEnabled = false)
        targetTypes.foreach(name =>
          assert(declaration(warmDisabled, name).isInstanceOf[SummaryDeclaration])
        )
        assertHasErrors(warmDisabled, root)
        assertNoUnused(warmDisabled, root)
      }
    }
  }

  test("refresh adds and removes error suppression according to each org policy") {
    Seq(None, Some(true)).foreach(policy =>
      withIsolatedRuntime {
        FileSystemHelper.run(validSources) { root: PathLike =>
          val org = openOrg(root, unusedOnError = policy, cacheEnabled = true)
          assertExpectedUnused(org, root)

          targetFiles.foreach(file => root.join(file).write(errorSources(file)))
          org.unmanaged.refreshAll(targetFiles.map(file => root.join(file)).toArray)
          org.flush()
          assertHasErrors(org, root)
          if (policy.contains(true)) assertExpectedUnused(org, root) else assertNoUnused(org, root)

          targetFiles.foreach(file => root.join(file).write(validSources(file)))
          org.unmanaged.refreshAll(targetFiles.map(file => root.join(file)).toArray)
          org.flush()
          assertExpectedUnused(org, root)
          targetFiles.foreach(file =>
            assert(
              !issuesFor(org, root, file)
                .exists(issue => DiagnosticCategory.isErrorType(issue.diagnostic.category))
            )
          )
        }
      }
    )
  }

  test("sequential orgs isolate plugin enablement and unused-on-error policy") {
    withIsolatedRuntime {
      FileSystemHelper.run(errorSources) { root: PathLike =>
        val disabled =
          openOrg(root, unusedOnError = Some(true), cacheEnabled = false, unusedEnabled = false)
        assertNoUnused(disabled, root)

        val defaultOrg = openOrg(root, unusedOnError = None, cacheEnabled = false)
        assertHasErrors(defaultOrg, root)
        assertNoUnused(defaultOrg, root)

        val optedIn = openOrg(root, unusedOnError = Some(true), cacheEnabled = false)
        assertHasErrors(optedIn, root)
        assertExpectedUnused(optedIn, root)

        val defaultAgain = openOrg(root, unusedOnError = None, cacheEnabled = false)
        assertHasErrors(defaultAgain, root)
        assertNoUnused(defaultAgain, root)
      }
    }
  }
}
