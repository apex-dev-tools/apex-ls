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

class TestClassesCommandTest extends AnyFunSuite with BatchCommandTestSupport {
  private val config =
    """{
      |  "packageDirectories": [
      |    {"path": "force app", "default": true},
      |    {"path": "second"}
      |  ],
      |  "namespace": "example"
      |}""".stripMargin

  private val files = Map(
    "sfdx-project.json"                          -> config,
    "force app/main/default/classes/Service.cls" -> "public class Service {}",
    "force app/main/default/classes/ServiceImpl.cls" ->
      "public class ServiceImpl { Service service; }",
    "force app/main/default/classes/ServiceTest.cls" ->
      "@isTest public class ServiceTest { ServiceImpl service; }",
    "force app/main/default/classes/Annotated.cls" -> "@IsTeSt private class Annotated {}",
    "force app/main/default/classes/Legacy.cls" ->
      "public class Legacy { testMethod static void verifiesBehavior() {} }",
    "force app/main/default/classes/Ordinary.cls" -> "public class Ordinary {}",
    "force app/main/default/classes/Broken.cls"   -> "@isTest private class Broken {",
    "second/classes/Second.cls"                   -> "public class Second {}",
    "second/classes/SecondTest.cls" ->
      "@isTest private class SecondTest { Second value; }"
  )

  test("impacted mode preserves explanations, namespaces, paths, cache modes, and ordering") {
    FileSystemHelper.runTempDir(files) { workspace =>
      Seq(false, true).foreach { cacheEnabled =>
        val relative = "force app/main/default/classes/Service.cls"
        val absolute = workspace.join(relative).toString
        val invocation = invoke(
          workspace,
          "test-classes",
          cacheEnabled,
          "--mode",
          "impacted",
          "--path",
          relative,
          s"--path=$absolute",
          "--path",
          "second/classes/Second.cls",
          "--path",
          "force app/main/default/classes/missing.cls"
        )

        assert(invocation.status == 0)
        assert(invocation.stderr.isEmpty)
        val classes = invocation.json("result")("testClasses").arr
        assert(classes.map(_("name").str) == Seq("example.SecondTest", "example.ServiceTest"))
        assert(
          classes.head("explanation").arr.map(_.str) == Seq("example.SecondTest", "example.Second")
        )
        assert(
          classes(1)("explanation").arr
            .map(_.str) == Seq("example.ServiceTest", "example.ServiceImpl", "example.Service")
        )

        val repeated = invoke(
          workspace,
          "test-classes",
          cacheEnabled,
          "--mode=impacted",
          s"--path=$relative",
          "--path=second/classes/Second.cls"
        )
        assert(repeated.stdout == invocation.stdout)
      }
    }
  }

  test("all mode discovers declared top-level tests for the workspace or selected paths") {
    FileSystemHelper.runTempDir(files) { workspace =>
      Seq(false, true).foreach { cacheEnabled =>
        val all = invoke(workspace, "test-classes", cacheEnabled, "--mode", "all")
        assert(all.status == 0)
        assert(all.stderr.isEmpty)
        assert(
          all.json("result")("testClasses").arr.map(_("name").str) == Seq(
            "example.Annotated",
            "example.Legacy",
            "example.SecondTest",
            "example.ServiceTest"
          )
        )
        assert(all.json("result")("testClasses").arr.forall(_("explanation").arr.isEmpty))

        val selected = invoke(
          workspace,
          "test-classes",
          cacheEnabled,
          "--mode=all",
          "--path",
          "force app/main/default/classes/Legacy.cls",
          "--path",
          "force app/main/default/classes/Annotated.cls",
          "--path",
          "force app/main/default/classes/Legacy.cls",
          "--path",
          "force app/main/default/classes/Ordinary.cls",
          "--path",
          "force app/main/default/classes/Broken.cls",
          "--path",
          "force app/main/default/classes/missing.cls"
        )
        assert(selected.status == 0)
        assert(
          selected.json("result")("testClasses").arr.map(_("name").str) == Seq(
            "example.Annotated",
            "example.Legacy"
          )
        )
      }
    }
  }

  test("impacted mode returns an empty success for valid no-match paths") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val invocation = invoke(
        workspace,
        "test-classes",
        cacheEnabled = false,
        "--mode",
        "impacted",
        "--path",
        "force app/main/default/classes/Ordinary.cls"
      )

      assert(invocation.status == 0)
      assert(invocation.json("result")("testClasses").arr.isEmpty)
    }
  }

  test("test-classes validates mode and path arguments before loading a workspace") {
    val invalid = Seq(
      invokeRaw("test-classes"),
      invokeRaw("test-classes", "--mode"),
      invokeRaw("test-classes", "--mode", "unknown"),
      invokeRaw("test-classes", "--mode", "all", "--mode", "all"),
      invokeRaw("test-classes", "--mode", "impacted"),
      invokeRaw("test-classes", "--mode", "all", "--path"),
      invokeRaw("test-classes", "--mode", "all", "unexpected")
    )

    assert(invalid.forall(_.status == 1))
    assert(invalid.forall(_.json("error")("code").str == "INVALID_ARGUMENT"))
  }
}
