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
import com.nawforce.pkgforce.diagnostics.UNUSED_CATEGORY
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.api.{Issue, IssueLocation}
import mainargs.{Flag, ParserForMethods, TokensReader, arg, main}

import java.time.Instant
import scala.annotation.unused
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Command line for running project analysis.
  *
  * Defaults to reporting issue but can also be used to report dependency information.
  */
object CheckForIssues {
  private final val STATUS_OK: Int                  = 0
  private final val STATUS_ARGS: Int                = 1
  private final val STATUS_EXCEPTION: Int           = 3
  private final val STATUS_ISSUES: Int              = 4
  private final val STATUS_WARNINGS_ONLY: Int       = 5
  private final val STATUS_UNUSED_ONLY: Int         = 6
  private final val STATUS_WARNINGS_AND_UNUSED: Int = 7

  /** Reporting detail, expressed as the two independent axes it actually has. Ordinary warnings
    * and unused findings are selected separately, errors are always reported.
    */
  private[apexls] case class DetailMode(
    name: String,
    includeWarnings: Boolean,
    includeUnused: Boolean
  ) {

    /** True if the analysis engine needs to produce unused findings for this mode. */
    def unusedAnalysis: Boolean = includeUnused

    def includes(issue: Issue): Boolean = {
      if (issue.isError) true
      else if (isUnusedIssue(issue.rule().name())) includeUnused
      else includeWarnings
    }
  }

  private[apexls] object DetailMode {
    val errors: DetailMode =
      DetailMode("errors", includeWarnings = false, includeUnused = false)
    val warnings: DetailMode =
      DetailMode("warnings", includeWarnings = true, includeUnused = false)
    val errorsAndUnused: DetailMode =
      DetailMode("errors-and-unused", includeWarnings = false, includeUnused = true)
    val unused: DetailMode =
      DetailMode("unused", includeWarnings = true, includeUnused = true)

    val all: Seq[DetailMode] = Seq(errors, warnings, errorsAndUnused, unused)

    def parse(value: String): Option[DetailMode] = all.find(_.name == value)

    def displayNames: String = all.map(mode => s"'${mode.name}'").mkString(", ")
  }

  case class Param(providerId: String, name: String, values: Option[List[String]])

  private object Param {
    def toMap(params: Seq[Param]): Map[String, List[(String, List[String])]] = {
      val collected = mutable.Map[String, List[(String, List[String])]]()
      params.foreach(param => {
        val providerParams = collected.getOrElse(param.providerId, Nil)
        collected.put(param.providerId, (param.name, param.values.getOrElse(Nil)) :: providerParams)
      })
      collected.toMap
    }
  }

  implicit object ParamRead extends TokensReader.Simple[Param] {
    def shortName = "param"

    def read(text: Seq[String]): Either[String, Param] = {
      val parts     = text.head.split("=", 2)
      val value     = Option.when(parts.length == 2) { parts(1) }
      val headParts = parts.head.split(":", 2)
      if (headParts.length != 2) {
        Left(
          s"Expecting params to have format <provider-id>:<name>[=<value>[,<value>...], not '$text"
        )
      } else {
        Right(
          Param(headParts.head.trim, headParts(1).trim, value.map(_.split(",").map(_.trim).toList))
        )
      }
    }
  }

  @unused
  @main(name = "io.github.apexdevtools.apexls.CheckForIssues")
  def mainWithArgs(
    @arg(short = 'f', doc = "Output format text (default), json or pmd")
    format: String = "text",
    @arg(short = 'l', doc = "Text output logging level, none (default), info or debug")
    logging: String = "none",
    @arg(short = 'd', doc = "Detail level, errors (default), warnings, errors-and-unused, unused")
    detail: String = "errors",
    @arg(short = 'n', doc = "Disable cache use")
    nocache: Flag,
    @arg(
      short = 'p',
      doc = "Analysis provider param in format <provider-id>:<name>[=<value>[,<value>...]]"
    )
    param: Seq[Param],
    @arg(short = 'w', doc = "Workspace directory path, defaults to current directory")
    workspace: String = "",
    @arg(short = 'c', doc = "Cache directory path, defaults to env or home dir")
    cacheDir: String = ""
  ): Unit = {
    System.exit(run(format, logging, detail, nocache.value, param, workspace, cacheDir))
  }

  def main(args: Array[String]): Unit = {
    ParserForMethods(this).runOrExit(ArraySeq.unsafeWrapArray(args))
  }

  def run(
    format: String,
    logging: String,
    detail: String,
    nocache: Boolean,
    params: Seq[Param],
    directory: String,
    cacheDirectory: String
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

      val detailMode = DetailMode.parse(detail) match {
        case Some(mode) => mode
        case None =>
          System.err.println(
            s"Unknown detail level provided '$detail', should be one of ${DetailMode.displayNames}"
          )
          return STATUS_ARGS
      }

      val options = OpenOptions
        .default()
        .withParser("OutlineSingle")
        .withAutoFlush(enabled = false)
        .withExternalAnalysisMode(LoadAndRefreshAnalysis.shortName, Param.toMap(params))
        .withLoggingLevel(loggingLevel)
        .withCache(!nocache)
        .withCacheDirectory(cacheDirectory)
        .withUnused(detailMode.unusedAnalysis)
        .withUnusedOnError(detailMode.unusedAnalysis)

      // Load org and flush to cache if we are using it
      val org = Org.newOrg(Path(workspace), options)
      if (!nocache) {
        org.flush()
      }

      // Every renderer and the exit status are derived from this one filtered set so that
      // what is reported and what the process status claims can not disagree.
      val issues = selectIssues(org, detailMode)
      outputFormat match {
        case "pmd"  => print(asPMD(issues))
        case "json" => print(render(issues, new JSONMessageWriter()))
        case _      => print(render(issues, new TextMessageWriter()))
      }
      exitStatus(issues)

    } catch {
      case ex: Throwable =>
        ex.printStackTrace(System.err)
        STATUS_EXCEPTION
    }
  }

  private[apexls] def selectIssues(org: Org, detailMode: DetailMode): Array[Issue] = {
    org.issues
      .issuesForFiles(null, detailMode.includeWarnings || detailMode.includeUnused, 0)
      .filter(detailMode.includes)
  }

  private def render(issues: Array[Issue], writer: MessageWriter): String = {
    writer.startOutput()
    var lastPath = ""

    issues.foreach(issue => {
      if (issue.filePath() != lastPath) {
        if (lastPath.nonEmpty)
          writer.endDocument()
        lastPath = issue.filePath()
        writer.startDocument(lastPath)
      }
      writer.writeMessage(
        issue.rule().name(),
        issue.rule().id(),
        issue.fileLocation(),
        issue.message
      )
    })
    if (lastPath.nonEmpty)
      writer.endDocument()

    writer.output
  }

  private[apexls] def exitStatus(issues: Array[Issue]): Int = {
    exitStatus(
      hasErrors = issues.exists(_.isError),
      hasWarnings = issues.exists(issue => !issue.isError && !isUnusedIssue(issue.rule().name())),
      hasUnused = issues.exists(issue => !issue.isError && isUnusedIssue(issue.rule().name()))
    )
  }

  private[apexls] def exitStatus(
    hasErrors: Boolean,
    hasWarnings: Boolean,
    hasUnused: Boolean
  ): Int = {
    if (hasErrors) STATUS_ISSUES
    else if (hasWarnings && hasUnused) STATUS_WARNINGS_AND_UNUSED
    else if (hasWarnings) STATUS_WARNINGS_ONLY
    else if (hasUnused) STATUS_UNUSED_ONLY
    else STATUS_OK
  }

  private def isUnusedIssue(categoryName: String): Boolean = {
    categoryName == UNUSED_CATEGORY.name
  }

  private trait MessageWriter {
    def startOutput(): Unit

    def startDocument(path: String): Unit

    def writeMessage(
      category: String,
      diagnosticId: String,
      location: IssueLocation,
      message: String
    ): Unit

    def endDocument(): Unit

    def output: String
  }

  private class TextMessageWriter(showPath: Boolean = true) extends MessageWriter {
    private val buffer = new mutable.StringBuilder()

    override def startOutput(): Unit = buffer.clear()

    override def startDocument(path: String): Unit = if (showPath) buffer ++= path + '\n'

    override def writeMessage(
      category: String,
      diagnosticId: String,
      location: IssueLocation,
      message: String
    ): Unit = buffer ++= s"$diagnosticId: ${location.displayPosition}: $message\n"

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

    override def writeMessage(
      category: String,
      diagnosticId: String,
      location: IssueLocation,
      message: String
    ): Unit = {
      buffer ++= (if (firstMessage) "" else ",\n")
      buffer ++= s"""{${locationAsJSON(
          location
        )}, "category": "$category", "id": "$diagnosticId", "message": "${JSON
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

  private def asPMD(issues: Array[Issue]): String = {
    val issuesByFile = issues.groupBy(_.filePath())
    val files = issuesByFile.map(kv => {
      val path   = kv._1
      val issues = kv._2

      val violations = issues.map(issue => {
        <violation beginline={issue.fileLocation().startLineNumber().toString}
                   endline={issue.fileLocation().endLineNumber().toString}
                   begincolumn={issue.fileLocation().startCharOffset().toString}
                   endcolumn={issue.fileLocation().endCharOffset().toString}
                   rule={issue.rule.id()}
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
    printer.format(pmd) + "\n"
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
