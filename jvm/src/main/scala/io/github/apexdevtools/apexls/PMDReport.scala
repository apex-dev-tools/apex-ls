/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package io.github.apexdevtools.apexls

import com.nawforce.apexlink.api.{LoadAndRefreshAnalysis, Org}
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.runtime.platform.Path
import mainargs.{Flag, Leftover, ParserForClass, arg}

import java.time.Instant
import scala.collection.immutable.ArraySeq
import scala.xml.XML

/** Generate a report using PMD format XML, see
  * https://github.com/pmd/pmd/blob/master/pmd-core/src/main/resources/report_2_0_0.xsd
  */
object PMDReport {
  private final val REPORT_NAME           = "apex-ls-pmd-report.xml"
  private final val STATUS_OK: Int        = 0
  private final val STATUS_ARGS: Int      = 1
  private final val STATUS_EXCEPTION: Int = 3

  private case class ConfigArgs(
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

    val writeWarnings: Boolean = args.warnings.value
  }

  private object Config {
    def apply(args: Array[String]): Config = {
      new Config(
        ParserForClass[ConfigArgs]
          .constructOrExit(ArraySeq.unsafeWrapArray(args), customName = classOf[PMDReport].getName)
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
      if (config.dirs.isEmpty) {
        System.err.println(s"No workspace directory argument provided.")
        return STATUS_ARGS
      }
      if (config.dirs.length > 1) {
        System.err.println(
          s"Multiple arguments provided, expected single workspace directory, '${config.dirs.mkString(", ")}'}"
        )
        return STATUS_ARGS
      }

      // Load org and flush to cache if we are using it
      val workspace = config.dirs.head
      val org       = Org.newOrg(Path(workspace), config.asOpenOptions)
      if (config.shouldManualFlush) {
        org.flush()
      }

      // Output report
      val outputPath = Path(workspace).join(REPORT_NAME)
      writeIssues(outputPath, org, config.writeWarnings)

    } catch {
      case ex: Throwable =>
        ex.printStackTrace(System.err)
        STATUS_EXCEPTION
    }
  }

  private def writeIssues(outputPath: Path, org: Org, includeWarnings: Boolean): Int = {

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
    XML.save(
      outputPath.toString,
      XML.loadString(printer.format(pmd)),
      "UTF-8",
      xmlDecl = true,
      null
    )
    STATUS_OK
  }
}

class PMDReport {
  // To allow getting classOf, see use above
}
