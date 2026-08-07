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

import com.nawforce.apexlink.api.Org
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class DependencyCountsCommandTest extends AnyFunSuite with BatchCommandTestSupport {
  test("dependency-counts supports scopes, packages, triggers, limits, aliases, and tests") {
    val config =
      """{
        |  "packageDirectories": [
        |    {"path": "force-app", "default": true},
        |    {"path": "second"}
        |  ],
        |  "namespace": "",
        |  "plugins": {
        |    "unpackagedMetadata": ["unpackaged"],
        |    "maxDependencyCount": 9,
        |    "dependencyCountAliases": {"medium": 15}
        |  }
        |}""".stripMargin
    val files = Map(
      "sfdx-project.json" -> config,
      "force-app/main/default/classes/A.cls" ->
        "//MaxDependencyCount(medium)\npublic class A { B value; }",
      "force-app/main/default/classes/B.cls" -> "public class B {}",
      "force-app/main/default/classes/Bad.cls" ->
        "//MaxDependencyCount(unknown)\npublic class Bad {}",
      "force-app/main/default/classes/ATest.cls" ->
        "@isTest private class ATest { A value; }",
      "force-app/main/default/triggers/A.trigger" ->
        "trigger A on Account (before insert) { A value; }",
      "second/classes/Second.cls"    -> "public class Second {}",
      "unpackaged/classes/Loose.cls" -> "public class Loose {}",
      "force-app/empty/readme.txt"   -> "empty"
    )

    FileSystemHelper.runTempDir(files) { workspace =>
      val all = invoke(workspace, "dependency-counts", cacheEnabled = false)
      assert(all.status == 0)
      val allCounts = all.json("result")("counts").arr
      val paths     = allCounts.map(_("path").str)
      assert(paths == paths.sorted)
      assert(paths.forall(path => java.nio.file.Paths.get(path).isAbsolute))
      assert(paths.exists(_.endsWith("A.trigger")))
      assert(paths.exists(_.endsWith("Second.cls")))
      assert(paths.exists(_.endsWith("Loose.cls")))
      assert(paths.exists(_.endsWith("ATest.cls")))

      val byName = allCounts.map(value => fileName(value("path").str) -> value).toMap
      assert(byName("A.cls")("maxDependencyCount").num == 15)
      assert(byName("A.cls")("maxDependencyCountError") == ujson.Null)
      assert(byName("B.cls")("maxDependencyCount").num == 9)
      assert(byName("Bad.cls")("maxDependencyCount") == ujson.Null)
      assert(byName("Bad.cls")("maxDependencyCountError").str.contains("unknown"))

      val scoped = invoke(
        workspace,
        "dependency-counts",
        cacheEnabled = false,
        "--scope",
        "force-app/main/default/classes",
        "--exclude-tests"
      )
      assert(scoped.status == 0)
      val scopedNames =
        scoped.json("result")("counts").arr.map(value => fileName(value("path").str))
      assert(scopedNames == Seq("A.cls", "B.cls", "Bad.cls"))

      val absolute = invoke(
        workspace,
        "dependency-counts",
        cacheEnabled = false,
        s"--scope=${workspace.join("second")}"
      )
      assert(absolute.status == 0)
      assert(
        absolute.json("result")("counts").arr.map(value => fileName(value("path").str)) ==
          Seq("Second.cls")
      )

      val empty =
        invoke(workspace, "dependency-counts", cacheEnabled = false, "--scope", "force-app/empty")
      assert(empty.status == 0)
      assert(empty.json("result")("counts").arr.isEmpty)
    }
  }

  test("dependency-counts exposes missing limits as two explicit nulls through Org") {
    FileSystemHelper.runTempDir(Map("NoLimit.cls" -> "public class NoLimit {}")) { workspace =>
      val org: Org  = Org.newOrg(workspace.toString)
      val apiCounts = org.getAllDependencyCounts(workspace.toString, excludeTestClasses = false)
      assert(apiCounts.length == 1)
      assert(apiCounts.head.maxDependencyCount == Left(None))

      val invocation = invoke(workspace, "dependency-counts", cacheEnabled = false)
      val count      = invocation.json("result")("counts")(0)
      assert(count("maxDependencyCount") == ujson.Null)
      assert(count("maxDependencyCountError") == ujson.Null)
    }
  }

  test("dependency-counts rejects invalid and out-of-workspace scopes") {
    FileSystemHelper.runTempDir(Map("Empty.cls" -> "public class Empty {}")) { workspace =>
      val missing =
        invoke(workspace, "dependency-counts", cacheEnabled = false, "--scope", "missing")
      val outside = invoke(
        workspace,
        "dependency-counts",
        cacheEnabled = false,
        "--scope",
        workspace.parent.toString
      )

      assert(missing.status == 1)
      assert(missing.json("error")("code").str == "INVALID_SCOPE")
      assert(outside.status == 1)
      assert(outside.json("error")("code").str == "INVALID_SCOPE")
    }
  }

  test("dependency-counts rejects command option errors before loading a workspace") {
    val invalid = Seq(
      invokeRaw("dependency-counts", "--scope"),
      invokeRaw("dependency-counts", "--exclude-tests", "--exclude-tests")
    )

    assert(invalid.forall(_.status == 1))
    assert(invalid.forall(_.json("error")("code").str == "INVALID_ARGUMENT"))
  }
}
