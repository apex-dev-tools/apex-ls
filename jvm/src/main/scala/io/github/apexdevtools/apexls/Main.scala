/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.api.IssueLocation
import mainargs.{Flag, Leftover, ParserForClass, arg}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** Command line for running project analysis.
  *
  * Defaults to reporting issue but can also be used to report dependency information.
  */
object Main {
  private final val STATUS_OK: Int        = 0
  private final val STATUS_ARGS: Int      = 1
  private final val STATUS_EXCEPTION: Int = 3
  private final val STATUS_ISSUES: Int    = 4

  private case class ConfigArgs(
    @arg(short = 'd', doc = "Report dependencies rather than issues")
    depends: Flag,
    @arg(short = 'w', doc = "Output warning issues")
    warnings: Flag,
    @arg(short = 'u', doc = "Output unused warning issues, requires --warnings")
    unused: Flag,
    @arg(short = 'i', doc = "Enable info logging")
    info: Flag,
    @arg(short = 'd', doc = "Enable debug logging")
    debug: Flag,
    @arg(short = 'n', doc = "Disable cache use")
    nocache: Flag,
    @arg(short = 'j', doc = "Generate json output, disables info/debug logging")
    json: Flag,
    @arg(doc = "SFDX Workspace directory path")
    dirs: Leftover[String]
  )

  private class Config(args: ConfigArgs) {

    val dirs: Seq[String] = args.dirs.value

    val asOpenOptions: OpenOptions = {
      OpenOptions
        .default()
        .withAutoFlush(enabled = false)
        .withExternalAnalysisMode(LoadAndRefreshAnalysis.shortName)
        .withLoggingLevel(
          if (args.debug.value) "debug"
          else if (args.info.value) "info"
          else "none"
        )
        .withCache(!args.nocache.value)
        .withUnused(args.unused.value)
    }

    val shouldManualFlush: Boolean = !args.nocache.value

    val outputDependencies: Boolean = args.depends.value

    val asJSON: Boolean = args.json.value

    val warnings: Boolean = args.warnings.value
  }

  private object Config {
    def apply(args: Array[String]): Config = {
      new Config(
        ParserForClass[ConfigArgs]
          .constructOrExit(ArraySeq.unsafeWrapArray(args), customName = classOf[Main].getName)
      )
    }
  }

  def main(args: Array[String]): Unit = {
    System.exit(run(args))
  }

  def run(args: Array[String]): Int = {
    try {
      val config = Config(args)

      // Check we have some metadata directories to work with
      val dirs = config.dirs
      if (dirs.isEmpty) {
        System.err.println(s"No workspace directory argument provided.")
        return STATUS_ARGS
      }
      if (dirs.length > 1) {
        System.err.println(
          s"Multiple arguments provided, expected single workspace directory, '${dirs.mkString(", ")}'}"
        )
        return STATUS_ARGS
      }

      // Load org and flush to cache if we are using it
      val workspace = config.dirs.head
      val org       = Org.newOrg(Path(workspace), config.asOpenOptions)
      if (config.shouldManualFlush) {
        org.flush()
      }

      // Output issues
      if (config.outputDependencies) {
        if (config.asJSON) {
          writeDependenciesAsJSON(org)
        } else {
          writeDependenciesAsCSV(org)
        }
        STATUS_OK
      } else {
        writeIssues(org, config.asJSON, config.warnings)
      }

    } catch {
      case ex: Throwable =>
        ex.printStackTrace(System.err)
        STATUS_EXCEPTION
    }
  }

  private def writeDependenciesAsJSON(org: Org): Unit = {
    val buffer = new mutable.StringBuilder()
    var first  = true
    buffer ++= s"""{ "dependencies": [\n"""
    org.getDependencies.asScala.foreach(kv => {
      if (!first)
        buffer ++= ",\n"
      first = false

      buffer ++= s"""{ "name": "${kv._1}", "dependencies": ["""
      buffer ++= kv._2.map("\"" + _ + "\"").mkString(", ")
      buffer ++= s"]}"
    })
    buffer ++= "]}\n"
    print(buffer.mkString)
  }

  private def writeDependenciesAsCSV(org: Org): Unit = {
    org.getDependencies.asScala.foreach(kv => {
      println(s"${kv._1}, ${kv._2.mkString(", ")}")
    })
  }

  private def writeIssues(org: Org, asJSON: Boolean, includeWarnings: Boolean): Int = {

    val issues = org.issues.issuesForFiles(null, includeWarnings, 0)
    val writer = if (asJSON) new JSONMessageWriter() else new TextMessageWriter()
    writer.startOutput()
    var hasErrors = false
    var lastPath  = ""

    issues.foreach(issue => {
      hasErrors |= issue.isError()
      if (includeWarnings || issue.isError) {

        if (issue.filePath() != lastPath) {
          if (lastPath.nonEmpty)
            writer.endDocument()
          lastPath = issue.filePath()
          writer.startDocument(lastPath)
        }

        writer.writeMessage(issue.rule().name(), issue.fileLocation(), issue.message)

      }
    })
    if (lastPath.nonEmpty)
      writer.endDocument()

    print(writer.output)
    System.out.flush()
    if (hasErrors) STATUS_ISSUES else STATUS_OK
  }

  private trait MessageWriter {
    def startOutput(): Unit

    def startDocument(path: String): Unit

    def writeMessage(category: String, location: IssueLocation, message: String): Unit

    def endDocument(): Unit

    def output: String
  }

  private class TextMessageWriter(showPath: Boolean = true) extends MessageWriter {
    private val buffer = new mutable.StringBuilder()

    override def startOutput(): Unit = buffer.clear()

    override def startDocument(path: String): Unit = if (showPath) buffer ++= path + '\n'

    override def writeMessage(category: String, location: IssueLocation, message: String): Unit =
      buffer ++= s"$category: ${location.displayPosition}: $message\n"

    override def endDocument(): Unit = {}

    override def output: String = buffer.toString()
  }

  private class JSONMessageWriter extends MessageWriter {
    private val buffer                 = new mutable.StringBuilder()
    private var firstDocument: Boolean = _
    private var firstMessage: Boolean  = _

    override def startOutput(): Unit = {
      buffer.clear()
      buffer ++= s"""{ "files": [\n"""
      firstDocument = true
    }

    override def startDocument(path: String): Unit = {
      buffer ++= (if (firstDocument) "" else ",\n")
      buffer ++= s"""{ "path": "${JSON.encode(path)}", "messages": [\n"""
      firstDocument = false
      firstMessage = true
    }

    override def writeMessage(category: String, location: IssueLocation, message: String): Unit = {
      buffer ++= (if (firstMessage) "" else ",\n")
      buffer ++= s"""{${locationAsJSON(location)}, "category": "$category", "message": "${JSON
          .encode(message)}"}"""
      firstMessage = false
    }

    override def endDocument(): Unit = buffer ++= "\n]}"

    override def output: String = {
      buffer ++= "]}\n"
      buffer.toString()
    }

    private def locationAsJSON(location: IssueLocation): String =
      s""""start": {"line": ${location.startLineNumber()}, "offset": ${location
          .startCharOffset()} }, "end": {"line": ${location.endLineNumber()}, "offset": ${location
          .endCharOffset()} }"""
  }

  object JSON {
    def encode(value: String): String = {
      val buf = new mutable.StringBuilder()
      value.foreach {
        case '"'                 => buf.append("\\\"")
        case '\\'                => buf.append("\\\\")
        case '\b'                => buf.append("\\b")
        case '\f'                => buf.append("\\f")
        case '\n'                => buf.append("\\n")
        case '\r'                => buf.append("\\r")
        case '\t'                => buf.append("\\t")
        case char if char < 0x20 => buf.append("\\u%04x".format(char: Int))
        case char                => buf.append(char)
      }
      buf.mkString
    }
  }
}

private class Main {
  // To allow getting classOf, see above
}
