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

import com.nawforce.apexlink.api.{Org, ServerOps}
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Environment
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class BatchDependencyTest extends AnyFunSuite {

  test("dependency-report returns deterministic flattened nodes with all fields") {
    val files = Map(
      "sfdx-project.json" -> project("pkg", Seq(".")),
      "Base.cls"          -> "public virtual class Base {}",
      "Contract.cls"      -> "public interface Contract {}",
      "Helper.cls"        -> "public class Helper {}",
      "Subject.cls" ->
        "//MaxDependencyCount(12)\nglobal class Subject extends Base implements Contract { Helper helper; }"
    )

    FileSystemHelper.runTempDir(files) { workspace =>
      val first  = invoke(workspace, "dependency-report", cacheEnabled = true)
      val second = invoke(workspace, "dependency-report", cacheEnabled = false)

      assert(first.status == 0)
      assert(second.status == 0)
      assert(first.json == second.json)
      assert(first.stdout.count(_ == '\n') == 1)
      val nodes = first.json("result")("nodes").arr
      assert(nodes.map(_("name").str) == nodes.map(_("name").str).sorted)

      val subject = nodes.find(_("name").str == "pkg.Subject").get
      assert(
        subject.obj.keySet == Set(
          "name",
          "nature",
          "size",
          "transitiveCount",
          "maxDependencyCount",
          "isEntryPoint",
          "extending",
          "implementing",
          "using"
        )
      )
      assert(subject("nature").str == "class")
      assert(subject("size").num > 0)
      assert(subject("transitiveCount").num == 3)
      assert(subject("maxDependencyCount").num == 12)
      assert(subject("isEntryPoint").bool)
      assert(subject("extending").arr.map(_.str) == Seq("pkg.Base"))
      assert(subject("implementing").arr.map(_.str) == Seq("pkg.Contract"))
      assert(subject("using").arr.map(_.str) == Seq("pkg.Helper"))
      assert(first.stderr.isEmpty)
    }
  }

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
      java.nio.file.Files.createSymbolicLink(
        java.nio.file.Paths.get(workspace.toString).resolve("outside-link"),
        java.nio.file.Paths.get(workspace.parent.toString)
      )
      val missing =
        invoke(workspace, "dependency-counts", cacheEnabled = false, "--scope", "missing")
      val outside = invoke(
        workspace,
        "dependency-counts",
        cacheEnabled = false,
        "--scope",
        workspace.parent.toString
      )
      val linkedOutside =
        invoke(workspace, "dependency-counts", cacheEnabled = false, "--scope", "outside-link")

      assert(missing.status == 1)
      assert(missing.json("error")("code").str == "INVALID_SCOPE")
      assert(outside.status == 1)
      assert(outside.json("error")("code").str == "INVALID_SCOPE")
      assert(linkedOutside.status == 1)
      assert(linkedOutside.json("error")("code").str == "INVALID_SCOPE")
    }
  }

  test("dependency-bombs preserves ranking, excludes tests, and supports zero") {
    val files = Map(
      "sfdx-project.json" -> project("test", Seq("pkg")),
      "pkg/Dummy1.cls" ->
        "public class Dummy1 { {Object a = new Dummy2(); Object b = new Dummy3();} }",
      "pkg/Dummy2.cls" ->
        "public class Dummy2 { {Object a = new Dummy3(); Object b = new Dummy1(); Object c = new Dummy4();}}",
      "pkg/Dummy3.cls" -> "public class Dummy3 {}",
      "pkg/Dummy4.cls" -> "public class Dummy4 {}",
      "pkg/BombTest.cls" ->
        "@isTest private class BombTest { {Dummy1 a; Dummy2 b;} }"
    )

    FileSystemHelper.runTempDir(files) { workspace =>
      val expected = Org
        .newOrg(workspace.toString)
        .getDependencyBombs(20)
        .sortBy(bomb => (-bomb.score, bomb.identifier.typeName.toString))
      val invocation =
        invoke(workspace, "dependency-bombs", cacheEnabled = false, "--count", "20")
      assert(invocation.status == 0)
      val bombs = invocation.json("result")("bombs").arr
      assert(bombs.map(_("name").str) == Seq("test.Dummy2", "test.Dummy1"))
      assert(
        bombs.map(value =>
          (
            value("name").str,
            value("usedBy").num.toInt,
            value("uses").num.toInt,
            value("score").num
          )
        ) == expected.toSeq.map(bomb =>
          (bomb.identifier.typeName.toString, bomb.usedBy, bomb.uses, bomb.score)
        )
      )
      assert(!bombs.exists(_("name").str.contains("BombTest")))

      val zero = invoke(workspace, "dependency-bombs", cacheEnabled = false, "--count=0")
      assert(zero.status == 0)
      assert(zero.json("result")("bombs").arr.isEmpty)
    }
  }

  test("dependency command option errors are argument failures without loading a workspace") {
    val invalid = Seq(
      invokeRaw("dependency-bombs", "--count", "-1"),
      invokeRaw("dependency-bombs", "--count", "abc"),
      invokeRaw("dependency-bombs", "--count"),
      invokeRaw("dependency-counts", "--scope"),
      invokeRaw("dependency-counts", "--exclude-tests", "--exclude-tests"),
      invokeRaw("dependency-report", "--scope", ".")
    )

    assert(invalid.forall(_.status == 1))
    assert(invalid.forall(_.json("error")("code").str == "INVALID_ARGUMENT"))
  }

  private def project(namespace: String, directories: Seq[String]): String = {
    val packageDirectories = directories.map(path => s"""{"path":"$path"}""").mkString(",")
    s"""{"packageDirectories":[$packageDirectories],"namespace":"$namespace"}"""
  }

  private def fileName(path: String): String = java.nio.file.Paths.get(path).getFileName.toString

  private def invoke(
    workspace: PathLike,
    command: String,
    cacheEnabled: Boolean,
    commandArguments: String*
  ): Invocation = {
    val cacheArguments =
      if (cacheEnabled) Seq("--cache-dir", workspace.join("cache").toString) else Seq("--no-cache")
    invokeRaw(
      (Seq(command, "--workspace", workspace.toString) ++ cacheArguments ++ commandArguments): _*
    )
  }

  private def invokeRaw(args: String*): Invocation = {
    val stdout                 = new ByteArrayOutputStream()
    val stderr                 = new ByteArrayOutputStream()
    val originalCacheDirectory = Environment.getCacheDirOverride
    val originalAutoFlush      = ServerOps.isAutoFlushEnabled
    val originalParser         = ServerOps.getCurrentParser
    val originalLoggingLevel   = LoggerOps.getLoggingLevel
    val status                 = Batch.run(args.toArray, stdout, stderr)
    val stdoutText             = new String(stdout.toByteArray, StandardCharsets.UTF_8)

    assert(Environment.getCacheDirOverride == originalCacheDirectory)
    assert(ServerOps.isAutoFlushEnabled == originalAutoFlush)
    assert(ServerOps.getCurrentParser == originalParser)
    assert(LoggerOps.getLoggingLevel == originalLoggingLevel)
    assert(stdoutText.count(_ == '\n') == 1)
    new Invocation(
      status,
      stdoutText,
      new String(stderr.toByteArray, StandardCharsets.UTF_8),
      ujson.read(stdoutText)
    )
  }

  private final class Invocation(
    val status: Int,
    val stdout: String,
    val stderr: String,
    val json: ujson.Value
  )
}
