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

import org.scalatest.funsuite.AnyFunSuite

class LoadBenchmarkArgumentsTest extends AnyFunSuite {
  private val minimal = Seq("--parser", "OutlineMulti", "--unused", "true", "--logging", "none")

  private def parse(args: String*) = LoadBenchmarkArguments.parse(args)

  private def errorFor(args: String*): String = {
    parse(args: _*) match {
      case Left(error) => error.message
      case Right(_)    => fail(s"Expected a failure for ${args.mkString(" ")}")
    }
  }

  test("a minimal configuration parses with conservative defaults") {
    val arguments = parse(minimal: _*).toOption.get

    assert(arguments.parser == "OutlineMulti")
    assert(arguments.unused)
    assert(!arguments.unusedOnError)
    assert(arguments.logging == "none")
    assert(arguments.parallelism.isEmpty)
    assert(arguments.blockPrefetchThreads.isEmpty)
    assert(arguments.label.isEmpty)
    assert(!arguments.includePaths)
  }

  test("optional settings are read from both argument forms") {
    val arguments = parse(
      (minimal ++ Seq(
        "--unused-on-error=true",
        "--parallelism",
        "4",
        "--block-prefetch-threads=2",
        "--label=cold",
        "--include-paths"
      )): _*
    ).toOption.get

    assert(arguments.unusedOnError)
    assert(arguments.parallelism.contains(4))
    assert(arguments.blockPrefetchThreads.contains(2))
    assert(arguments.label.contains("cold"))
    assert(arguments.includePaths)
  }

  test("parser, unused and logging must be given explicitly") {
    assert(errorFor("--unused", "true", "--logging", "none").contains("'--parser' is required"))
    assert(
      errorFor("--parser", "OutlineMulti", "--logging", "none").contains("'--unused' is required")
    )
    assert(
      errorFor("--parser", "OutlineMulti", "--unused", "true")
        .contains("'--logging' is required")
    )
  }

  test("values are validated") {
    assert(errorFor((minimal.updated(1, "Antlr")): _*).contains("OutlineMulti, OutlineSingle"))
    assert(errorFor((minimal.updated(3, "maybe")): _*).contains("'true' or 'false'"))
    assert(errorFor((minimal.updated(5, "verbose")): _*).contains("none, info, debug, trace"))
    assert(errorFor((minimal :+ "--parallelism" :+ "0"): _*).contains("must be a positive integer"))
    assert(
      errorFor((minimal :+ "--block-prefetch-threads" :+ "3"): _*)
        .contains("must be one of 0, 2, 4")
    )
    assert(
      errorFor((minimal :+ "--block-prefetch-threads" :+ "two"): _*)
        .contains("must be one of 0, 2, 4")
    )
  }

  test("malformed and repeated options are rejected") {
    assert(errorFor((minimal :+ "--label"): _*).contains("'--label' requires a value"))
    assert(errorFor((minimal :+ "--label="): _*).contains("'--label' requires a value"))
    assert(
      errorFor((minimal ++ Seq("--label", "a", "--label", "b")): _*)
        .contains("only be provided once")
    )
    assert(
      errorFor((minimal ++ Seq("--include-paths", "--include-paths")): _*)
        .contains("only be provided once")
    )
    assert(errorFor((minimal :+ "--include-paths=true"): _*).contains("does not take a value"))
    assert(errorFor((minimal :+ "--unknown"): _*).contains("Unexpected argument '--unknown'"))
  }

  test("parser and logging names are case insensitive but normalised") {
    val arguments = parse((minimal.updated(1, "outlinesingle").updated(5, "INFO")): _*).toOption.get

    assert(arguments.parser == "OutlineSingle")
    assert(arguments.logging == "info")
  }
}
