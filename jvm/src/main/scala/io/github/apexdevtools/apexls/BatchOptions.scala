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

import scala.collection.mutable

private[apexls] final case class BatchOptions(
  workspace: String,
  cacheDirectory: Option[String],
  cacheEnabled: Boolean
)

private[apexls] object BatchOptions {
  def parse(args: IndexedSeq[String]): Either[BatchError, (BatchOptions, Seq[String])] = {
    var workspace      = Option(System.getProperty("user.dir")).getOrElse(".")
    var cacheDirectory = Option.empty[String]
    var cacheEnabled   = true
    var index          = 0
    val commandArgs    = mutable.ArrayBuffer[String]()
    val seen           = mutable.Set[String]()

    def duplicate(option: String): Left[BatchError, Nothing] = {
      Left(BatchError("INVALID_ARGUMENT", s"Option '$option' may only be provided once"))
    }

    def value(option: String, inline: Option[String]): Either[BatchError, String] = {
      inline match {
        case Some(candidate) if candidate.nonEmpty => Right(candidate)
        case Some(_) => Left(BatchError("INVALID_ARGUMENT", s"Option '$option' requires a value"))
        case None if index + 1 >= args.length =>
          Left(BatchError("INVALID_ARGUMENT", s"Option '$option' requires a value"))
        case None =>
          index += 1
          Right(args(index))
      }
    }

    while (index < args.length) {
      val token                 = args(index)
      val (option, inlineValue) = splitOption(token)
      option match {
        case "--workspace" =>
          if (!seen.add(option)) return duplicate(option)
          value(option, inlineValue) match {
            case Left(error)      => return Left(error)
            case Right(candidate) => workspace = candidate
          }
        case "--cache-dir" =>
          if (!seen.add(option)) return duplicate(option)
          value(option, inlineValue) match {
            case Left(error)      => return Left(error)
            case Right(candidate) => cacheDirectory = Some(candidate)
          }
        case "--no-cache" =>
          if (inlineValue.nonEmpty) {
            return Left(BatchError("INVALID_ARGUMENT", "Option '--no-cache' does not take a value"))
          }
          if (!seen.add(option)) return duplicate(option)
          cacheEnabled = false
        case _ => commandArgs += token
      }
      index += 1
    }

    Right((BatchOptions(workspace, cacheDirectory, cacheEnabled), commandArgs.toSeq))
  }

  private def splitOption(token: String): (String, Option[String]) = {
    val index = token.indexOf('=')
    if (index < 0) (token, None) else (token.substring(0, index), Some(token.substring(index + 1)))
  }
}
