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

import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class LoadBenchmarkCommandTest extends AnyFunSuite with BatchCommandTestSupport {
  private val files = Map(
    "sfdx-project.json"            -> project("", Seq("force-app")),
    "force-app/classes/A.cls"      -> "public class A { {B value;} }",
    "force-app/classes/B.cls"      -> "public class B {}",
    "force-app/triggers/T.trigger" -> "trigger T on Account (before insert) { A value; }",
    "force-app/labels/CustomLabels.labels" ->
      "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
  )

  private val configuration =
    Seq("--parser", "OutlineMulti", "--unused", "false", "--logging", "none")

  test("a load is measured with its full configuration, size profile and environment") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val invocation =
        invoke(
          workspace,
          "benchmark-load",
          cacheEnabled = false,
          (configuration :+ "--label=cold"): _*
        )
      assert(invocation.status == 0)

      val result = invocation.json("result")
      assert(result("schemaVersion").num == 2)
      assert(result("label").str == "cold")
      assert(result("workspace")("identity").str.startsWith("sha256:"))

      val settings = result("configuration")
      assert(settings("parser").str == "Outline Parser - Multithreaded")
      assert(settings("parserOption").str == "OutlineMulti")
      assert(!settings("cache")("enabled").bool)
      assert(!settings("unused").bool)
      assert(!settings("unusedOnError").bool)
      assert(settings("logging").str == "none")
      assert(!settings("autoFlush").bool)
      assert(settings("blockPrefetchThreads").num == 0)

      assert(result("timings")("totalLoadMs").num > 0)
      assert(result("timings")("cacheFlushMs") == ujson.Null)
      val phases = result("timings")("phases").arr.map(_("phase").str)
      assert(phases.contains("orgCreate"))
      assert(phases.contains("workspaceScan"))
      assert(phases.contains("moduleDeploy"))
      assert(phases == phases.sorted)

      val size = result("size")
      assert(size("packageCount").num == 1)
      assert(size("moduleCount").num == 1)
      assert(size("apexTypeCount").num == 3)
      assert(size("apexClassFileCount").num == 2)
      assert(size("triggerFileCount").num == 1)
      assert(size("apexSourceBytes").num > 0)
      assert(size("byNature")("apex").num == 2)

      assert(result("issues")("errors").num == 0)

      val validation = result("validation")
      assert(validation("typeContexts").num > 0)
      assert(validation("typeCacheLookups").num > 0)
      assert(
        validation("typeCacheLookups").num ==
          validation("typeCacheHits").num + validation("typeCacheMisses").num
      )
      assert(validation("typeCacheHitRate").num >= 0)

      assert(result("parallelism")("availableProcessors").num > 0)
      assert(result("environment")("javaVersion").str.nonEmpty)
      assert(result("environment")("maxHeapBytes").num > 0)
    }
  }

  test("a requested block prefetch thread count is applied and reported back") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val invocation =
        invoke(
          workspace,
          "benchmark-load",
          cacheEnabled = false,
          (configuration ++ Seq("--block-prefetch-threads", "2")): _*
        )
      assert(invocation.status == 0)
      assert(invocation.json("result")("configuration")("blockPrefetchThreads").num == 2)
    }
  }

  test("nothing identifying the workspace is reported unless paths are requested") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val hidden = invoke(workspace, "benchmark-load", cacheEnabled = false, configuration: _*)
      assert(hidden.json("result")("workspace")("path") == ujson.Null)
      assert(!hidden.stdout.contains(workspace.toString))

      val shown =
        invoke(
          workspace,
          "benchmark-load",
          cacheEnabled = false,
          (configuration :+ "--include-paths"): _*
        )
      assert(shown.json("result")("workspace")("path").str == workspace.toString)
    }
  }

  test("an isolated cache directory is used and its write time reported separately") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val invocation = invoke(workspace, "benchmark-load", cacheEnabled = true, configuration: _*)
      assert(invocation.status == 0)

      val result = invocation.json("result")
      assert(result("configuration")("cache")("enabled").bool)
      assert(result("configuration")("cache")("directoryProvided").bool)
      assert(result("configuration")("cache")("directory") == ujson.Null)
      assert(result("timings")("cacheFlushMs").num >= 0)
      assert(workspace.join("cache").isDirectory)
    }
  }

  test("the cache setting has to be chosen so the developer cache is never touched") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val implicitCache =
        invokeRaw(("benchmark-load" +: "--workspace" +: workspace.toString +: configuration): _*)
      assert(implicitCache.status == 1)
      assert(implicitCache.json("error")("code").str == "INVALID_ARGUMENT")
      assert(implicitCache.json("error")("message").str.contains("explicit cache setting"))

      val both = invokeRaw(
        ("benchmark-load" +: "--workspace" +: workspace.toString +: "--no-cache" +:
          "--cache-dir" +: workspace.join("cache").toString +: configuration): _*
      )
      assert(both.status == 1)
      assert(both.json("error")("message").str.contains("exclusive"))
    }
  }

  test("an invalid workspace is an argument failure") {
    val invocation = invokeRaw(
      ("benchmark-load" +: "--workspace" +: "/path/that/does/not/exist" +: "--no-cache" +:
        configuration): _*
    )

    assert(invocation.status == 1)
    assert(invocation.json("error")("code").str == "INVALID_SCOPE")
  }

  test("an incomplete configuration is an argument failure") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val invocation =
        invoke(workspace, "benchmark-load", cacheEnabled = false, "--parser", "OutlineMulti")

      assert(invocation.status == 1)
      assert(invocation.json("error")("code").str == "INVALID_ARGUMENT")
      assert(invocation.json("error")("message").str.contains("must be explicit"))
    }
  }
}
