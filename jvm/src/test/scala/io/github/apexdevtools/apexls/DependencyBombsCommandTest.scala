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

class DependencyBombsCommandTest extends AnyFunSuite with BatchCommandTestSupport {
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

  test("dependency-bombs rejects command option errors before loading a workspace") {
    val invalid = Seq(
      invokeRaw("dependency-bombs", "--count", "-1"),
      invokeRaw("dependency-bombs", "--count", "abc"),
      invokeRaw("dependency-bombs", "--count")
    )

    assert(invalid.forall(_.status == 1))
    assert(invalid.forall(_.json("error")("code").str == "INVALID_ARGUMENT"))
  }
}
