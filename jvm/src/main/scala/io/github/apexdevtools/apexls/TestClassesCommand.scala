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

import com.nawforce.apexlink.api.TestClass
import com.nawforce.runtime.platform.Path

import java.nio.file.Paths
import scala.collection.mutable

private[apexls] final case class TestClassesResult(testClasses: Array[TestClass])

private[apexls] object TestClassesCommand extends BatchCommand {
  override type Result = TestClassesResult

  override val name: String               = "test-classes"
  override val requiresWorkspace: Boolean = true

  override def validate(args: Seq[String]): Either[BatchError, Unit] = {
    TestClassesArguments.parse(args).map(_ => ())
  }

  override def execute(
    context: BatchContext,
    args: Seq[String]
  ): Either[BatchError, TestClassesResult] = {
    TestClassesArguments.parse(args).map { arguments =>
      val workspace = Path(context.options.workspace)
      val paths = arguments.paths
        .map(path => if (Paths.get(path).isAbsolute) Path(path) else workspace.join(path))
        .map(_.toString)
        .distinct
        .toArray
      val testClasses = arguments.mode match {
        case ImpactedMode => context.org.get.getImpactedTestClasses(paths)
        case AllMode      => context.org.get.getDeclaredTestClasses(paths)
      }
      TestClassesResult(testClasses.sortBy(_.name))
    }
  }

  override def writeResult(result: TestClassesResult): ujson.Value = {
    val testClasses = result.testClasses.map { testClass =>
      ujson.Obj(
        "name"        -> testClass.name,
        "explanation" -> ujson.Arr(testClass.explanation.map(ujson.Str(_)).toIndexedSeq: _*)
      )
    }
    ujson.Obj("testClasses" -> ujson.Arr(testClasses.toIndexedSeq: _*))
  }

  private sealed trait TestClassesMode
  private case object ImpactedMode extends TestClassesMode
  private case object AllMode      extends TestClassesMode

  private final case class TestClassesArguments(mode: TestClassesMode, paths: Seq[String])

  private object TestClassesArguments {
    def parse(args: Seq[String]): Either[BatchError, TestClassesArguments] = {
      var mode  = Option.empty[TestClassesMode]
      val paths = mutable.ArrayBuffer[String]()
      var index = 0

      def value(option: String, token: String): Either[BatchError, String] = {
        if (token == option) {
          if (index + 1 >= args.length || args(index + 1).startsWith("--"))
            Left(BatchError("INVALID_ARGUMENT", s"Option '$option' requires a value"))
          else {
            index += 1
            Right(args(index))
          }
        } else {
          val candidate = token.substring(option.length + 1)
          if (candidate.isEmpty)
            Left(BatchError("INVALID_ARGUMENT", s"Option '$option' requires a value"))
          else Right(candidate)
        }
      }

      while (index < args.length) {
        val token = args(index)
        if (token == "--mode" || token.startsWith("--mode=")) {
          if (mode.nonEmpty)
            return Left(BatchError("INVALID_ARGUMENT", "Option '--mode' may only be provided once"))
          value("--mode", token) match {
            case Left(error)       => return Left(error)
            case Right("impacted") => mode = Some(ImpactedMode)
            case Right("all")      => mode = Some(AllMode)
            case Right(candidate) =>
              return Left(
                BatchError(
                  "INVALID_ARGUMENT",
                  s"Unsupported test class mode '$candidate'; expected 'impacted' or 'all'"
                )
              )
          }
        } else if (token == "--path" || token.startsWith("--path=")) {
          value("--path", token) match {
            case Left(error)      => return Left(error)
            case Right(candidate) => paths += candidate
          }
        } else {
          return Left(BatchError("INVALID_ARGUMENT", s"Unexpected argument '$token'"))
        }
        index += 1
      }

      mode match {
        case None => Left(BatchError("INVALID_ARGUMENT", "Option '--mode' is required"))
        case Some(ImpactedMode) if paths.isEmpty =>
          Left(BatchError("INVALID_ARGUMENT", "Mode 'impacted' requires at least one '--path'"))
        case Some(selectedMode) => Right(TestClassesArguments(selectedMode, paths.toSeq))
      }
    }
  }
}
