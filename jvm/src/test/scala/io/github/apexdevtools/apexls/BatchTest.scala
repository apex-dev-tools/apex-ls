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
import com.nawforce.runtime.platform.Environment
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class BatchTest extends AnyFunSuite {

  test("ping returns an empty successful result") {
    val invocation = invoke(Array("ping"))

    assert(invocation.status == 0)
    assert(invocation.json("protocolVersion").num == 1)
    assert(invocation.json("command").str == "ping")
    assert(invocation.json("ok").bool)
    assert(invocation.json("result").obj.isEmpty)
    assert(invocation.json("error") == ujson.Null)
    assert(invocation.stdout.endsWith("\n"))
    assert(invocation.stdout.count(_ == '\n') == 1)
  }

  test("a command may return a successful result") {
    val command    = new TestCommand("success", requiresWorkspace = false)
    val invocation = invoke(Array("success"), commands = Seq(command))

    assert(invocation.status == 0)
    assert(invocation.json("ok").bool)
    assert(invocation.json("result").obj.isEmpty)
  }

  test("missing and unknown commands are argument failures") {
    val missing = invoke(Array.empty)
    assert(missing.status == 1)
    assert(missing.json("command").str.isEmpty)
    assert(missing.json("error")("code").str == "INVALID_ARGUMENT")

    val unknown = invoke(Array("not-a-command"))
    assert(unknown.status == 1)
    assert(unknown.json("command").str == "not-a-command")
    assert(unknown.json("error")("code").str == "UNKNOWN_COMMAND")
  }

  test("invalid common and command options are argument failures") {
    val invalidOption = invoke(Array("ping", "--invalid"))
    assert(invalidOption.status == 1)
    assert(invalidOption.json("error")("code").str == "INVALID_ARGUMENT")

    val missingValue = invoke(Array("ping", "--workspace"))
    assert(missingValue.status == 1)
    assert(missingValue.json("error")("code").str == "INVALID_ARGUMENT")
  }

  test("parser selection is not a supported option") {
    val invocation = invoke(Array("ping", "--parser", "OutlineSingle"))

    assert(invocation.status == 1)
    assert(invocation.json("error")("code").str == "INVALID_ARGUMENT")
  }

  test("invalid workspace scope is an argument failure") {
    val command = new TestCommand("workspace", requiresWorkspace = true)
    val invocation = invoke(
      Array("workspace", "--workspace", "/path/that/does/not/exist"),
      commands = Seq(command),
      loader = DefaultBatchWorkspaceLoader
    )

    assert(invocation.status == 1)
    assert(invocation.json("error")("code").str == "INVALID_SCOPE")
  }

  test("workspace loading failure is an execution failure") {
    val command = new TestCommand("workspace", requiresWorkspace = true)
    val loader = new BatchWorkspaceLoader {
      override def load(options: BatchOptions): Either[BatchDispatchFailure, Org] = {
        Left(BatchDispatchFailure(BatchError("WORKSPACE_LOAD_FAILED", "could not load"), 3))
      }
    }
    val invocation = invoke(Array("workspace"), commands = Seq(command), loader = loader)

    assert(invocation.status == 3)
    assert(invocation.json("ok").bool == false)
    assert(invocation.json("error")("code").str == "WORKSPACE_LOAD_FAILED")
  }

  test("malformed workspace configuration is a workspace loading failure") {
    val workspace              = Files.createTempDirectory("batch-workspace")
    val project                = workspace.resolve("sfdx-project.json")
    val originalCacheDirectory = Environment.getCacheDirOverride
    val originalAutoFlush      = ServerOps.isAutoFlushEnabled
    val originalParser         = ServerOps.getCurrentParser
    val originalLoggingLevel   = LoggerOps.getLoggingLevel
    try {
      Files.write(project, "{".getBytes(StandardCharsets.UTF_8))
      val command = new TestCommand("workspace", requiresWorkspace = true)
      val invocation = invoke(
        Array("workspace", "--workspace", workspace.toString, "--no-cache"),
        commands = Seq(command),
        loader = DefaultBatchWorkspaceLoader
      )

      assert(invocation.status == 3)
      assert(invocation.json("error")("code").str == "WORKSPACE_LOAD_FAILED")
      assert(Environment.getCacheDirOverride == originalCacheDirectory)
      assert(ServerOps.isAutoFlushEnabled == originalAutoFlush)
      assert(ServerOps.getCurrentParser == originalParser)
      assert(LoggerOps.getLoggingLevel == originalLoggingLevel)
    } finally {
      Files.deleteIfExists(project)
      Files.deleteIfExists(workspace)
    }
  }

  test("workspace and cache options are passed to workspace commands") {
    val command  = new TestCommand("workspace", requiresWorkspace = true)
    val captures = collection.mutable.ArrayBuffer[BatchOptions]()
    val loader = new BatchWorkspaceLoader {
      override def load(options: BatchOptions): Either[BatchDispatchFailure, Org] = {
        captures += options
        Right(null.asInstanceOf[Org])
      }
    }

    val cacheEnabled = invoke(
      Array("workspace", "--workspace", "/workspace with spaces", "--cache-dir=/cache with spaces"),
      commands = Seq(command),
      loader = loader
    )
    val cacheDisabled = invoke(
      Array("workspace", "--workspace=/workspace with spaces", "--no-cache"),
      commands = Seq(command),
      loader = loader
    )

    assert(cacheEnabled.status == 0)
    assert(cacheDisabled.status == 0)
    assert(captures.head.workspace == "/workspace with spaces")
    assert(captures.head.cacheDirectory.contains("/cache with spaces"))
    assert(captures.head.cacheEnabled)
    assert(captures(1).workspace == "/workspace with spaces")
    assert(!captures(1).cacheEnabled)
  }

  test("thrown command exceptions return analysis failures and keep stdout clean") {
    val command = new TestCommand(
      "throws",
      requiresWorkspace = false,
      action = _ => {
        println("command noise")
        throw new IllegalStateException("analysis broke")
      }
    )
    val invocation = invoke(Array("throws"), commands = Seq(command))

    assert(invocation.status == 3)
    assert(invocation.json("error")("code").str == "ANALYSIS_FAILED")
    assert(!invocation.stdout.contains("command noise"))
    assert(invocation.stderr.contains("command noise"))
    assert(invocation.stderr.contains("analysis broke"))
  }

  test("serialization failures return a valid fallback envelope") {
    val command = new TestCommand("serialize", requiresWorkspace = false)
    val invocation = invoke(
      Array("serialize"),
      commands = Seq(command),
      encoder = _ => throw new IllegalStateException("serialization broke")
    )

    assert(invocation.status == 3)
    assert(invocation.json("error")("code").str == "SERIALIZATION_FAILED")
    assert(invocation.stdout.count(_ == '\n') == 1)
    assert(invocation.stderr.contains("serialization broke"))
  }

  private def invoke(
    args: Array[String],
    commands: Seq[BatchCommand] = Seq.empty,
    loader: BatchWorkspaceLoader = NoWorkspaceLoader,
    encoder: BatchEnvelope => String = BatchProtocol.write
  ): Invocation = {
    val stdout           = new ByteArrayOutputStream()
    val stderr           = new ByteArrayOutputStream()
    val selectedCommands = if (commands.nonEmpty) commands else Seq(PingCommand)
    val status           = Batch.run(args, stdout, stderr, selectedCommands, loader, encoder)
    val stdoutText       = new String(stdout.toByteArray, StandardCharsets.UTF_8)
    Invocation(
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

  private object Invocation {
    def apply(status: Int, stdout: String, stderr: String, json: ujson.Value): Invocation = {
      new Invocation(status, stdout, stderr, json)
    }
  }

  private class TestCommand(
    override val name: String,
    override val requiresWorkspace: Boolean,
    action: BatchContext => BatchResult = _ => BatchPingResult()
  ) extends BatchCommand {
    override def execute(
      context: BatchContext,
      args: Seq[String]
    ): Either[BatchError, BatchResult] = Right(action(context))
  }

  private object PingCommand extends BatchCommand {
    override val name: String               = "ping"
    override val requiresWorkspace: Boolean = false
    override def execute(
      context: BatchContext,
      args: Seq[String]
    ): Either[BatchError, BatchResult] = Right(BatchPingResult())
  }

  private object NoWorkspaceLoader extends BatchWorkspaceLoader {
    override def load(options: BatchOptions): Either[BatchDispatchFailure, Org] = {
      fail("Workspace loader should not have been called")
    }
  }
}
