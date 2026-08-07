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

import com.nawforce.apexlink.api.DependencyCount
import com.nawforce.runtime.platform.Path

import java.nio.file.Paths

private[apexls] object DependencyCountsCommand extends BatchCommand {
  override type Result = Array[DependencyCount]

  override val name: String               = "dependency-counts"
  override val requiresWorkspace: Boolean = true

  override def validate(args: Seq[String]): Either[BatchError, Unit] = {
    DependencyCountsArguments.parse(args).map(_ => ())
  }

  override def execute(
    context: BatchContext,
    args: Seq[String]
  ): Either[BatchError, Array[DependencyCount]] = {
    DependencyCountsArguments.parse(args).flatMap { arguments =>
      val workspace = Path(context.options.workspace)
      val scope = arguments.scope match {
        case Some(value) if Paths.get(value).isAbsolute => Path(value)
        case Some(value)                                => workspace.join(value)
        case None                                       => workspace
      }
      if (!scope.isDirectory) {
        Left(BatchError("INVALID_SCOPE", s"Scope '$scope' is not a directory"))
      } else if (!scope.native.toRealPath().startsWith(workspace.native.toRealPath())) {
        Left(BatchError("INVALID_SCOPE", s"Scope '$scope' is outside workspace '$workspace'"))
      } else {
        Right(
          context.org.get
            .getAllDependencyCounts(scope.toString, arguments.excludeTests)
            .sortBy(_.path)
        )
      }
    }
  }

  override def writeResult(result: Array[DependencyCount]): ujson.Value = {
    val counts = result.map { dependency =>
      val (maximum, maximumError) = dependency.maxDependencyCount match {
        case Right(value)      => (ujson.Num(value), ujson.Null)
        case Left(Some(error)) => (ujson.Null, ujson.Str(error))
        case Left(None)        => (ujson.Null, ujson.Null)
      }
      ujson.Obj(
        "path"                    -> dependency.path,
        "count"                   -> dependency.count,
        "maxDependencyCount"      -> maximum,
        "maxDependencyCountError" -> maximumError
      )
    }
    ujson.Obj("counts" -> ujson.Arr(counts.toIndexedSeq: _*))
  }

  private final case class DependencyCountsArguments(scope: Option[String], excludeTests: Boolean)

  private object DependencyCountsArguments {
    def parse(args: Seq[String]): Either[BatchError, DependencyCountsArguments] = {
      var scope        = Option.empty[String]
      var excludeTests = false
      var index        = 0

      while (index < args.length) {
        val token = args(index)
        if (token == "--exclude-tests") {
          if (excludeTests)
            return Left(
              BatchError("INVALID_ARGUMENT", "Option '--exclude-tests' may only be provided once")
            )
          excludeTests = true
        } else if (token == "--scope" || token.startsWith("--scope=")) {
          if (scope.nonEmpty)
            return Left(
              BatchError("INVALID_ARGUMENT", "Option '--scope' may only be provided once")
            )
          if (token == "--scope") {
            if (index + 1 >= args.length || args(index + 1).startsWith("--"))
              return Left(BatchError("INVALID_ARGUMENT", "Option '--scope' requires a value"))
            index += 1
            scope = Some(args(index))
          } else {
            val value = token.substring("--scope=".length)
            if (value.isEmpty)
              return Left(BatchError("INVALID_ARGUMENT", "Option '--scope' requires a value"))
            scope = Some(value)
          }
        } else {
          return Left(BatchError("INVALID_ARGUMENT", s"Unexpected argument '$token'"))
        }
        index += 1
      }

      Right(DependencyCountsArguments(scope, excludeTests))
    }
  }
}
