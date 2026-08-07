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

import com.nawforce.apexlink.rpc.BombScore
import com.nawforce.pkgforce.names.TypeIdentifier

private[apexls] object DependencyBombsCommand extends BatchCommand {
  override type Result = Array[BombScore]

  override val name: String               = "dependency-bombs"
  override val requiresWorkspace: Boolean = true

  override def validate(args: Seq[String]): Either[BatchError, Unit] = {
    DependencyBombsArguments.parse(args).map(_ => ())
  }

  override def execute(
    context: BatchContext,
    args: Seq[String]
  ): Either[BatchError, Array[BombScore]] = {
    DependencyBombsArguments.parse(args).map { arguments =>
      context.org.get
        .getDependencyBombs(arguments.count)
        .sortBy(bomb => (-bomb.score, identifierName(bomb.identifier)))
    }
  }

  override def writeResult(result: Array[BombScore]): ujson.Value = {
    val bombs = result.map { bomb =>
      ujson.Obj(
        "name"   -> identifierName(bomb.identifier),
        "usedBy" -> bomb.usedBy,
        "uses"   -> bomb.uses,
        "score"  -> bomb.score
      )
    }
    ujson.Obj("bombs" -> ujson.Arr(bombs.toIndexedSeq: _*))
  }

  private def identifierName(identifier: TypeIdentifier): String = identifier.typeName.toString

  private final case class DependencyBombsArguments(count: Int)

  private object DependencyBombsArguments {
    private final val DefaultCount = 20

    def parse(args: Seq[String]): Either[BatchError, DependencyBombsArguments] = {
      args match {
        case Seq()                 => Right(DependencyBombsArguments(DefaultCount))
        case Seq("--count", value) => parseCount(value)
        case Seq(token) if token.startsWith("--count=") =>
          parseCount(token.substring("--count=".length))
        case Seq("--count") =>
          Left(BatchError("INVALID_ARGUMENT", "Option '--count' requires a value"))
        case _ =>
          Left(BatchError("INVALID_ARGUMENT", s"Unexpected argument '${args.head}'"))
      }
    }

    private def parseCount(value: String): Either[BatchError, DependencyBombsArguments] = {
      try {
        val count = value.toInt
        if (count < 0)
          Left(BatchError("INVALID_ARGUMENT", "Option '--count' must be non-negative"))
        else
          Right(DependencyBombsArguments(count))
      } catch {
        case _: NumberFormatException =>
          Left(BatchError("INVALID_ARGUMENT", "Option '--count' must be a non-negative integer"))
      }
    }
  }
}
