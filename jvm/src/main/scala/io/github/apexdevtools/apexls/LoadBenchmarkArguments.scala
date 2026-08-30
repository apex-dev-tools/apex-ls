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

import com.nawforce.apexlink.api.ServerOps

import scala.collection.mutable
import scala.util.Try

/** Arguments of the benchmark-load command.
  *
  * The settings that differ between the CLI and editor load paths are mandatory, an omitted one is
  * an error rather than a default, so that a result can never be misread as measuring a
  * configuration it did not use.
  */
private[apexls] final case class LoadBenchmarkArguments(
  parser: String,
  unused: Boolean,
  unusedOnError: Boolean,
  logging: String,
  parallelism: Option[Int],
  blockPrefetchThreads: Option[Int],
  label: Option[String],
  includePaths: Boolean
)

private[apexls] object LoadBenchmarkArguments {
  final val ContextProperties: Seq[String] = Seq(
    "scala.concurrent.context.numThreads",
    "scala.concurrent.context.minThreads",
    "scala.concurrent.context.maxThreads"
  )

  private final val Parsers  = Seq("OutlineMulti", "OutlineSingle")
  private final val Loggings = Seq("none", "info", "debug", "trace")

  private final val Required = Seq("--parser", "--unused", "--logging")
  private final val Valued =
    Required ++ Seq("--unused-on-error", "--parallelism", "--block-prefetch-threads", "--label")

  def parse(args: Seq[String]): Either[BatchError, LoadBenchmarkArguments] = {
    collect(args).flatMap(collected => build(collected._1, collected._2))
  }

  private def collect(args: Seq[String]): Either[BatchError, (Map[String, String], Boolean)] = {
    val values       = mutable.Map[String, String]()
    var includePaths = false
    var index        = 0

    while (index < args.length) {
      val (option, inline) = splitOption(args(index))
      if (option == "--include-paths") {
        if (inline.nonEmpty) return invalid("Option '--include-paths' does not take a value")
        if (includePaths) return duplicate(option)
        includePaths = true
      } else if (Valued.contains(option)) {
        if (values.contains(option)) return duplicate(option)
        inline match {
          case Some(value) if value.nonEmpty    => values.put(option, value)
          case Some(_)                          => return requiresValue(option)
          case None if index + 1 >= args.length => return requiresValue(option)
          case None =>
            index += 1
            values.put(option, args(index))
        }
      } else {
        return invalid(s"Unexpected argument '${args(index)}'")
      }
      index += 1
    }

    Right((values.toMap, includePaths))
  }

  private def build(
    values: Map[String, String],
    includePaths: Boolean
  ): Either[BatchError, LoadBenchmarkArguments] = {
    for {
      _             <- requireAll(values)
      parser        <- oneOf("--parser", values("--parser"), Parsers)
      unused        <- boolean("--unused", values("--unused"))
      logging       <- oneOf("--logging", values("--logging"), Loggings)
      unusedOnError <- optionalBoolean("--unused-on-error", values)
      parallelism   <- optionalPositive("--parallelism", values)
      prefetch      <- optionalBlockPrefetchThreads(values)
    } yield LoadBenchmarkArguments(
      parser = parser,
      unused = unused,
      unusedOnError = unusedOnError.getOrElse(false),
      logging = logging,
      parallelism = parallelism,
      blockPrefetchThreads = prefetch,
      label = values.get("--label"),
      includePaths = includePaths
    )
  }

  private def requireAll(values: Map[String, String]): Either[BatchError, Unit] = {
    Required.find(option => !values.contains(option)) match {
      case Some(option) =>
        invalid(s"Option '$option' is required, benchmark configuration must be explicit")
      case None => Right(())
    }
  }

  private def oneOf(
    option: String,
    value: String,
    allowed: Seq[String]
  ): Either[BatchError, String] = {
    allowed.find(_.equalsIgnoreCase(value)) match {
      case Some(matched) => Right(matched)
      case None =>
        invalid(s"Option '$option' must be one of ${allowed.mkString(", ")}, not '$value'")
    }
  }

  private def boolean(option: String, value: String): Either[BatchError, Boolean] = {
    value.toLowerCase match {
      case "true"  => Right(true)
      case "false" => Right(false)
      case _       => invalid(s"Option '$option' must be 'true' or 'false', not '$value'")
    }
  }

  private def optionalBoolean(
    option: String,
    values: Map[String, String]
  ): Either[BatchError, Option[Boolean]] = {
    values.get(option) match {
      case None        => Right(None)
      case Some(value) => boolean(option, value).map(Some(_))
    }
  }

  private def optionalPositive(
    option: String,
    values: Map[String, String]
  ): Either[BatchError, Option[Int]] = {
    values.get(option) match {
      case None => Right(None)
      case Some(value) =>
        Try(value.toInt).toOption.filter(_ > 0) match {
          case Some(parsed) => Right(Some(parsed))
          case None         => invalid(s"Option '$option' must be a positive integer, not '$value'")
        }
    }
  }

  /** Only the counts the server accepts, a rejected value would otherwise be silently ignored and
    * the run reported as if it had been applied.
    */
  private def optionalBlockPrefetchThreads(
    values: Map[String, String]
  ): Either[BatchError, Option[Int]] = {
    val option = "--block-prefetch-threads"
    values.get(option) match {
      case None => Right(None)
      case Some(value) =>
        Try(value.toInt).toOption.filter(ServerOps.validBlockPrefetchThreads.contains) match {
          case Some(parsed) => Right(Some(parsed))
          case None =>
            invalid(
              s"Option '$option' must be one of " +
                s"${ServerOps.validBlockPrefetchThreads.mkString(", ")}, not '$value'"
            )
        }
    }
  }

  private def splitOption(token: String): (String, Option[String]) = {
    val index = token.indexOf('=')
    if (index < 0) (token, None) else (token.substring(0, index), Some(token.substring(index + 1)))
  }

  private def invalid(message: String): Left[BatchError, Nothing] = {
    Left(BatchError("INVALID_ARGUMENT", message))
  }

  private def duplicate(option: String): Left[BatchError, Nothing] = {
    invalid(s"Option '$option' may only be provided once")
  }

  private def requiresValue(option: String): Left[BatchError, Nothing] = {
    invalid(s"Option '$option' requires a value")
  }
}
