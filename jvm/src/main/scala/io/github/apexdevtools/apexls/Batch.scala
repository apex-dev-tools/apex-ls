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

import com.nawforce.apexlink.api.{DependencyCount, Org, ServerOps}
import com.nawforce.apexlink.rpc.{BombScore, DependencyNode}
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.runtime.platform.{Environment, Path}

import java.io.{OutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.collection.mutable
import scala.util.control.NonFatal

/** Versioned, machine-readable entry point for one-shot JVM analysis commands. */
object Batch {
  private final val StatusOk       = 0
  private final val StatusArgument = 1
  private final val StatusInternal = 3

  private val commands: Seq[BatchCommand] =
    Seq(PingCommand, DependencyReportCommand, DependencyCountsCommand, DependencyBombsCommand)

  def main(args: Array[String]): Unit = {
    System.exit(run(args, System.out, System.err))
  }

  /** Run a batch invocation, writing exactly one UTF-8 JSON document and newline to stdout. */
  def run(args: Array[String], stdout: OutputStream, stderr: OutputStream): Int = {
    run(args, stdout, stderr, commands, DefaultBatchWorkspaceLoader, BatchProtocol.write)
  }

  private[apexls] def run(
    args: Array[String],
    stdout: OutputStream,
    stderr: OutputStream,
    availableCommands: Seq[BatchCommand],
    workspaceLoader: BatchWorkspaceLoader,
    encoder: BatchEnvelope => String
  ): Int = synchronized {
    val diagnosticStream = stderr match {
      case stream: PrintStream => stream
      case _                   => new PrintStream(stderr, true, StandardCharsets.UTF_8.name())
    }
    val originalStdout         = System.out
    val originalStderr         = System.err
    val originalCacheDirectory = Environment.getCacheDirOverride
    val originalAutoFlush      = ServerOps.isAutoFlushEnabled
    val originalParser         = ServerOps.getCurrentParser
    val originalLoggingLevel   = LoggerOps.getLoggingLevel

    System.setOut(diagnosticStream)
    System.setErr(diagnosticStream)
    val (envelope, status) =
      try {
        Console.withOut(diagnosticStream) {
          Console.withErr(diagnosticStream) {
            dispatch(args.toIndexedSeq, availableCommands, workspaceLoader)
          }
        }
      } catch {
        case NonFatal(exception) =>
          exception.printStackTrace(diagnosticStream)
          failure("", "INTERNAL_ERROR", message(exception), StatusInternal)
      }

    val (json, finalStatus) =
      try {
        (encoder(envelope), status)
      } catch {
        case NonFatal(exception) =>
          exception.printStackTrace(diagnosticStream)
          val serializationFailure = BatchProtocol.failure(
            envelope.command,
            BatchError("SERIALIZATION_FAILED", message(exception))
          )
          (BatchProtocol.write(serializationFailure), StatusInternal)
      } finally {
        Environment.setCacheDirOverride(originalCacheDirectory)
        ServerOps.setAutoFlush(originalAutoFlush)
        ServerOps.setCurrentParser(originalParser)
        LoggerOps.setLoggingLevel(originalLoggingLevel)
        System.setOut(originalStdout)
        System.setErr(originalStderr)
      }

    stdout.write(json.getBytes(StandardCharsets.UTF_8))
    stdout.write('\n')
    stdout.flush()
    finalStatus
  }

  private def dispatch(
    args: IndexedSeq[String],
    availableCommands: Seq[BatchCommand],
    workspaceLoader: BatchWorkspaceLoader
  ): (BatchEnvelope, Int) = {
    val commandName = args.headOption.getOrElse("")
    if (commandName.isEmpty) {
      return failure("", "INVALID_ARGUMENT", "A command is required", StatusArgument)
    }

    val commandByName = availableCommands.map(command => command.name -> command).toMap
    commandByName.get(commandName) match {
      case None =>
        failure(commandName, "UNKNOWN_COMMAND", s"Unknown command '$commandName'", StatusArgument)
      case Some(command) =>
        BatchOptions.parse(args.tail) match {
          case Left(error) => (BatchProtocol.failure(commandName, error), StatusArgument)
          case Right((options, commandArguments)) =>
            command.validate(commandArguments) match {
              case Left(error) => (BatchProtocol.failure(commandName, error), StatusArgument)
              case Right(_) =>
                loadContext(command, options, workspaceLoader) match {
                  case Left(dispatchFailure) =>
                    (
                      BatchProtocol.failure(commandName, dispatchFailure.error),
                      dispatchFailure.status
                    )
                  case Right(context) =>
                    try {
                      command.execute(context, commandArguments) match {
                        case Left(error) =>
                          (BatchProtocol.failure(commandName, error), StatusArgument)
                        case Right(result) =>
                          try {
                            (
                              BatchProtocol.success(commandName, command.writeResult(result)),
                              StatusOk
                            )
                          } catch {
                            case NonFatal(exception) =>
                              exception.printStackTrace(System.err)
                              failure(
                                commandName,
                                "SERIALIZATION_FAILED",
                                message(exception),
                                StatusInternal
                              )
                          }
                      }
                    } catch {
                      case NonFatal(exception) =>
                        exception.printStackTrace(System.err)
                        failure(commandName, "ANALYSIS_FAILED", message(exception), StatusInternal)
                    }
                }
            }
        }
    }
  }

  private def loadContext(
    command: BatchCommand,
    options: BatchOptions,
    workspaceLoader: BatchWorkspaceLoader
  ): Either[BatchDispatchFailure, BatchContext] = {
    if (!command.requiresWorkspace) {
      Right(BatchContext(options, None))
    } else {
      try {
        workspaceLoader.load(options).map(org => BatchContext(options, Some(org)))
      } catch {
        case NonFatal(exception) =>
          exception.printStackTrace(System.err)
          Left(
            BatchDispatchFailure(
              BatchError("WORKSPACE_LOAD_FAILED", message(exception)),
              StatusInternal
            )
          )
      }
    }
  }

  private def failure(
    command: String,
    code: String,
    errorMessage: String,
    status: Int
  ): (BatchEnvelope, Int) = {
    (BatchProtocol.failure(command, BatchError(code, errorMessage)), status)
  }

  private def message(exception: Throwable): String = {
    Option(exception.getMessage).filter(_.nonEmpty).getOrElse(exception.getClass.getSimpleName)
  }

  private object PingCommand extends BatchCommand {
    override type Result = Unit

    override val name: String               = "ping"
    override val requiresWorkspace: Boolean = false
    override def execute(context: BatchContext, args: Seq[String]): Either[BatchError, Unit] =
      Right(())

    override def writeResult(result: Unit): ujson.Value = ujson.Obj()
  }

  private object DependencyReportCommand extends BatchCommand {
    override type Result = Array[DependencyNode]

    override val name: String               = "dependency-report"
    override val requiresWorkspace: Boolean = true

    override def execute(
      context: BatchContext,
      args: Seq[String]
    ): Either[BatchError, Array[DependencyNode]] = {
      val org = context.org.get
      val ids = org.getTypeIdentifiers(apexOnly = true)
      Right(
        org.getDependencyGraph(ids, depth = 1, apexOnly = true, ignoring = Array.empty).nodeData
      )
    }

    override def writeResult(result: Array[DependencyNode]): ujson.Value = {
      val nodes = result.sortBy(node => identifierName(node.identifier)).map { node =>
        ujson.Obj(
          "name"               -> identifierName(node.identifier),
          "nature"             -> node.nature,
          "size"               -> ujson.Num(node.size.toDouble),
          "transitiveCount"    -> node.transitiveCount,
          "maxDependencyCount" -> optionalNumber(node.maxDependencyCount),
          "isEntryPoint"       -> node.isEntryPoint,
          "extending"          -> identifiers(node.extending),
          "implementing"       -> identifiers(node.implementing),
          "using"              -> identifiers(node.using)
        )
      }
      ujson.Obj("nodes" -> ujson.Arr(nodes.toIndexedSeq: _*))
    }

    private def identifiers(
      values: Array[com.nawforce.pkgforce.names.TypeIdentifier]
    ): ujson.Arr = {
      ujson.Arr(values.map(identifierName).sorted.map(ujson.Str).toIndexedSeq: _*)
    }
  }

  private object DependencyCountsCommand extends BatchCommand {
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
  }

  private object DependencyBombsCommand extends BatchCommand {
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
  }

  private def optionalNumber(value: Option[Int]): ujson.Value = {
    value.map(ujson.Num(_)).getOrElse(ujson.Null)
  }

  private def identifierName(identifier: com.nawforce.pkgforce.names.TypeIdentifier): String = {
    identifier.typeName.toString
  }
}

private[apexls] final case class DependencyCountsArguments(
  scope: Option[String],
  excludeTests: Boolean
)

private[apexls] object DependencyCountsArguments {
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
          return Left(BatchError("INVALID_ARGUMENT", "Option '--scope' may only be provided once"))
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

private[apexls] final case class DependencyBombsArguments(count: Int)

private[apexls] object DependencyBombsArguments {
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

private[apexls] final case class BatchContext(options: BatchOptions, org: Option[Org])

private[apexls] trait BatchCommand {
  type Result

  def name: String
  def requiresWorkspace: Boolean
  def validate(args: Seq[String]): Either[BatchError, Unit] = {
    if (args.isEmpty) Right(())
    else Left(BatchError("INVALID_ARGUMENT", s"Unexpected argument '${args.head}'"))
  }
  def execute(context: BatchContext, args: Seq[String]): Either[BatchError, Result]
  def writeResult(result: Result): ujson.Value
}

private[apexls] final case class BatchDispatchFailure(error: BatchError, status: Int)

private[apexls] trait BatchWorkspaceLoader {
  def load(options: BatchOptions): Either[BatchDispatchFailure, Org]
}

private[apexls] object DefaultBatchWorkspaceLoader extends BatchWorkspaceLoader {
  private final val StatusArgument = 1
  private final val StatusInternal = 3

  override def load(options: BatchOptions): Either[BatchDispatchFailure, Org] = {
    val workspace = Path(options.workspace)
    if (!workspace.exists || !workspace.isDirectory) {
      return Left(
        BatchDispatchFailure(
          BatchError("INVALID_SCOPE", s"Workspace '${options.workspace}' is not a directory"),
          StatusArgument
        )
      )
    }
    if (!workspace.join("sfdx-project.json").isFile) {
      return Left(
        BatchDispatchFailure(
          BatchError(
            "INVALID_SCOPE",
            s"Workspace '${options.workspace}' does not contain sfdx-project.json"
          ),
          StatusArgument
        )
      )
    }

    try {
      val openOptions = OpenOptions
        .default()
        .withLoggingLevel("none")
        .withAutoFlush(enabled = false)
        .withCache(options.cacheEnabled)
        .withCacheDirectory(options.cacheDirectory.getOrElse(""))
      val org = Org.newOrg(workspace, openOptions)
      if (org.getProjectConfig().isEmpty) {
        return Left(
          BatchDispatchFailure(
            BatchError("WORKSPACE_LOAD_FAILED", s"Unable to load workspace '${options.workspace}'"),
            StatusInternal
          )
        )
      }
      if (options.cacheEnabled) {
        org.flush()
      }
      Right(org)
    } catch {
      case NonFatal(exception) =>
        Left(
          BatchDispatchFailure(
            BatchError(
              "WORKSPACE_LOAD_FAILED",
              Option(exception.getMessage)
                .filter(_.nonEmpty)
                .getOrElse(exception.getClass.getSimpleName)
            ),
            StatusInternal
          )
        )
    }
  }
}
