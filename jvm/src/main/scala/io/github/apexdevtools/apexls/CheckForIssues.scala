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
import mainargs.{Flag, ParserForMethods, arg, main}

import java.time.Instant
import scala.annotation.unused
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Command line for running project analysis.
  *
  * Defaults to reporting issue but can also be used to report dependency information.
  */
object CheckForIssues {
  private final val STATUS_OK: Int        = 0
  private final val STATUS_ARGS: Int      = 1
  private final val STATUS_EXCEPTION: Int = 3
  private final val STATUS_ISSUES: Int    = 4

  @unused
  @main(name = "io.github.apexdevtools.apexls.CheckForIssues")
  def mainWithArgs(
    @arg(short = 'f', doc = "Output format text (default), json or pmd")
    format: String = "text",
    @arg(short = 'l', doc = "Text output logging level, none (default), info or debug")
    logging: String = "none",
    @arg(short = 'd', doc = "Detail level, errors (default), warnings, unused")
    detail: String = "errors",
    @arg(short = 'n', doc = "Disable cache use")
    nocache: Flag,
    @arg(short = 'w', doc = "Workspace directory path, defaults to current directory")
    workspace: String = ""
  ): Unit = {
    System.exit(run(format, logging, detail, nocache.value, workspace))
  }

  def main(args: Array[String]): Unit = {
    ParserForMethods(this).runOrExit(ArraySeq.unsafeWrapArray(args))
  }

  def run(
    format: String,
    logging: String,
    detail: String,
    nocache: Boolean,
    directory: String
  ): Int = {
    try {
      val workspace = Path(directory)
      val outputFormat = format match {
        case "text" | "json" | "pmd" => format
        case _ =>
          System.err.println(
            s"Unknown output format provided '$format', should be 'text', 'json' or 'pmd'"
          )
          return STATUS_ARGS
      }

      val loggingLevel =
        if (outputFormat != "text")
          "none"
        else
          logging match {
            case "none" | "info" | "debug" => logging
            case _ =>
              System.err.println(
                s"Unknown logging level provided '$logging', should be 'none', 'info' or 'debug'"
              )
              return STATUS_ARGS
          }

      val detailLevel = detail match {
        case "errors" | "warnings" | "unused" => detail
        case _ =>
          System.err.println(
            s"Unknown detail level provided '$detail', should be 'errors', 'warnings' or 'unused'"
          )
          return STATUS_ARGS
      }

      val options = OpenOptions
        .default()
        .withParser("OutlineSingle")
        .withAutoFlush(enabled = false)
        .withExternalAnalysisMode(LoadAndRefreshAnalysis.shortName)
        .withLoggingLevel(loggingLevel)
        .withCache(!nocache)
        .withUnused(detailLevel == "unused")

      // Load org and flush to cache if we are using it
      val org = Org.newOrg(Path(workspace), options)
      if (!nocache) {
        org.flush()
      }

      // Output issues
      val includeWarnings = detailLevel == "warnings" || detailLevel == "unused"
      if (outputFormat == "pmd") {
        writeIssuesPMD(org, includeWarnings)
      } else {
        writeIssues(org, outputFormat == "json", includeWarnings)
      }

    } catch {
      case ex: Throwable =>
        ex.printStackTrace(System.err)
        STATUS_EXCEPTION
    }
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

  private def writeIssuesPMD(org: Org, includeWarnings: Boolean): Int = {

    val issues       = org.issues.issuesForFiles(null, includeWarnings, 0)
    val issuesByFile = issues.groupBy(_.filePath())
    val files = issuesByFile.map(kv => {
      val path   = kv._1
      val issues = kv._2

      val violations = issues.map(issue => {
        <violation beginline={issue.fileLocation().startLineNumber().toString}
                   endline={issue.fileLocation().endLineNumber().toString}
                   begincolumn={issue.fileLocation().startCharOffset().toString}
                   endcolumn={issue.fileLocation().endCharOffset().toString}
                   rule={issue.rule.name()}
                   ruleset={issue.provider()}
                   priority={issue.rule.priority().toString}>
          {issue.message()}
        </violation>
      })
      <file name={path}>
        {violations}
      </file>
    })

    val timestamp = Instant.now().toString
    val pmd = <pmd xmlns="http://pmd.sourceforge.net/report/2.0.0"
                   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                   xsi:schemaLocation="http://pmd.sourceforge.net/report/2.0.0 http://pmd.sourceforge.net/report_2_0_0.xsd"
                   version="1.0.0"
                   timestamp={timestamp}>
      {files}
    </pmd>

    val printer = new scala.xml.PrettyPrinter(80, 2)
    println(printer.format(pmd))
    STATUS_OK
  }

  private object JSON {
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
