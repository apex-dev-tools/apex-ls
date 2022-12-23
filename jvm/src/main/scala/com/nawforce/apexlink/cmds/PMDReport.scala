/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cmds

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.plugins.{PluginsManager, UnusedPlugin}
import com.nawforce.pkgforce.diagnostics.{DefaultLogger, LoggerOps}
import com.nawforce.runtime.platform.Environment

import java.time.Instant
import scala.xml.XML

/** Generate a report using PMD format XML, see
  * https://github.com/pmd/pmd/blob/master/pmd-core/src/main/resources/report_2_0_0.xsd
  */
object PMDReport {
  final val REPORT_NAME           = "apex-ls-pmd-report.xml"
  final val STATUS_OK: Int        = 0
  final val STATUS_ARGS: Int      = 1
  final val STATUS_EXCEPTION: Int = 3

  def main(args: Array[String]): Unit = {
    System.exit(run(args))
  }

  def usage(name: String) =
    s"Usage: $name -verbose [-unused]] [-nocache] [-info|-debug] <directory>"

  def run(args: Array[String]): Int = {
    val flags =
      Set("-verbose", "-unused", "-nocache", "-info", "-debug")

    val verbose = args.contains("-verbose")
    val unused  = verbose && args.contains("-unused")
    val noCache = args.contains("-nocache")
    val debug   = args.contains("-debug")
    val info    = !debug && args.contains("-info")

    // Check we have some metadata directories to work with
    val dirs = args.filterNot(flags.contains)
    if (dirs.isEmpty) {
      System.err.println(s"No workspace directory argument provided.")
      return STATUS_ARGS
    }
    if (dirs.length > 1) {
      System.err.println(
        s"Multiple arguments provided, expected workspace directory, '${dirs.mkString(", ")}'}"
      )
      return STATUS_ARGS
    }

    try {
      // Setup cache flushing, analysis & logging defaults
      ServerOps.setAutoFlush(false)
      ServerOps.setExternalAnalysisMode(LoadAndRefreshAnalysis)
      LoggerOps.setLogger(new DefaultLogger(System.err))
      if (debug)
        LoggerOps.setLoggingLevel(LoggerOps.DEBUG_LOGGING)
      else if (info)
        LoggerOps.setLoggingLevel(LoggerOps.INFO_LOGGING)

      // Disable loading from the cache
      if (noCache) {
        Environment.setCacheDirOverride(Some(None))
      }

      // Don't use unused analysis unless we have both verbose and unused flags
      if (!verbose || !unused) {
        PluginsManager.removePlugins(Seq(classOf[UnusedPlugin]))
      }

      // Load org and flush to cache if we are using it
      val org = Org.newOrg(dirs.head)
      if (!noCache) {
        org.flush()
      }

      // Output report
      writeIssues(org, verbose)

    } catch {
      case ex: Throwable =>
        ex.printStackTrace(System.err)
        STATUS_EXCEPTION
    }
  }

  private def writeIssues(org: Org, includeWarnings: Boolean): Int = {

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
                   priority={issue.rule.priority().toString}
        >
          {issue.message()}
        </violation>
      })
      <file name={path}>{violations}</file>
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
    XML.save(REPORT_NAME, XML.loadString(printer.format(pmd)), "UTF-8", xmlDecl = true, null)
    STATUS_OK
  }
}
