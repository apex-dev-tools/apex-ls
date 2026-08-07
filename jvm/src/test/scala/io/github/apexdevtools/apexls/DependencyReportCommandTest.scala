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

class DependencyReportCommandTest extends AnyFunSuite with BatchCommandTestSupport {
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

  test("dependency-report rejects command options before loading a workspace") {
    val invocation = invokeRaw("dependency-report", "--scope", ".")
    assert(invocation.status == 1)
    assert(invocation.json("error")("code").str == "INVALID_ARGUMENT")
  }
}
