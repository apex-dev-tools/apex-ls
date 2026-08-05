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

import upickle.default.{macroRW, ReadWriter}

/** A stable, machine-readable error returned by the JVM batch protocol. */
final case class BatchError(code: String, message: String)

/** Versioned response envelope shared by all JVM batch commands. */
final case class BatchEnvelope(
  protocolVersion: Int,
  command: String,
  ok: Boolean,
  result: Option[ujson.Value],
  error: Option[BatchError]
)

private[apexls] object BatchProtocol {
  final val Version: Int = 1

  implicit private val errorWriter: ReadWriter[BatchError] = macroRW

  def success(command: String, result: ujson.Value): BatchEnvelope = {
    BatchEnvelope(Version, command, ok = true, Some(result), None)
  }

  def failure(command: String, error: BatchError): BatchEnvelope = {
    BatchEnvelope(Version, command, ok = false, None, Some(error))
  }

  def write(envelope: BatchEnvelope): String = {
    val result = envelope.result match {
      case Some(value) => value
      case None        => ujson.Null
    }
    val error = envelope.error match {
      case Some(value) => upickle.default.writeJs(value)
      case None        => ujson.Null
    }

    ujson.write(
      ujson.Obj(
        "protocolVersion" -> envelope.protocolVersion,
        "command"         -> envelope.command,
        "ok"              -> envelope.ok,
        "result"          -> result,
        "error"           -> error
      )
    )
  }
}
