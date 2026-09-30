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
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

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

  private def writePathsFile(workspace: PathLike, content: String): String = {
    val file = Paths.get(workspace.toString, "paths.txt")
    Files.write(file, content.getBytes(StandardCharsets.UTF_8))
    file.toString
  }

  test("paths file entries are selected in the same way as path arguments") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val service = "force app/main/default/classes/Service.cls"
      val second  = workspace.join("second/classes/Second.cls").toString
      val expected = invoke(
        workspace,
        "test-classes",
        cacheEnabled = false,
        "--mode",
        "impacted",
        "--path",
        service,
        "--path",
        second
      )
      assert(expected.status == 0)

      val fromFile = writePathsFile(workspace, s"$service\r\n\n  \n$second\n")
      val file =
        invoke(
          workspace,
          "test-classes",
          cacheEnabled = false,
          "--mode=impacted",
          "--paths-file",
          fromFile
        )
      assert(file.status == 0)
      assert(file.stdout == expected.stdout)

      val combined = writePathsFile(workspace, second)
      val both = invoke(
        workspace,
        "test-classes",
        cacheEnabled = false,
        "--mode=impacted",
        s"--paths-file=$combined",
        "--path",
        service
      )
      assert(both.status == 0)
      assert(both.stdout == expected.stdout)
    }
  }

  test("all mode selects declared tests from a paths file") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val fromFile = writePathsFile(
        workspace,
        "force app/main/default/classes/Legacy.cls\nforce app/main/default/classes/Ordinary.cls\n"
      )
      val invocation =
        invoke(
          workspace,
          "test-classes",
          cacheEnabled = false,
          "--mode=all",
          "--paths-file",
          fromFile
        )

      assert(invocation.status == 0)
      assert(
        invocation.json("result")("testClasses").arr.map(_("name").str) == Seq("example.Legacy")
      )
    }
  }

  test("paths file must provide impacted paths and may only be given once") {
    FileSystemHelper.runTempDir(files) { workspace =>
      val empty = writePathsFile(workspace, "\n")
      val invocation =
        invoke(
          workspace,
          "test-classes",
          cacheEnabled = false,
          "--mode=impacted",
          "--paths-file",
          empty
        )

      val duplicate = invoke(
        workspace,
        "test-classes",
        cacheEnabled = false,
        "--mode=all",
        "--paths-file",
        empty,
        "--paths-file",
        empty
      )

      assert(Seq(invocation, duplicate).forall(_.status == 1))
      assert(Seq(invocation, duplicate).forall(_.json("error")("code").str == "INVALID_ARGUMENT"))
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
      invokeRaw("test-classes", "--mode", "all", "--paths-file"),
      invokeRaw("test-classes", "--mode", "all", "--paths-file", "/missing/paths.txt"),
      invokeRaw("test-classes", "--mode", "all", "--paths-file=", "--path", "Foo.cls"),
      invokeRaw("test-classes", "--mode", "all", "unexpected")
    )

    assert(invalid.forall(_.status == 1))
    assert(invalid.forall(_.json("error")("code").str == "INVALID_ARGUMENT"))
  }
}
