package com.nawforce.apexlink.opcst

import com.financialforce.oparser.{Location => OPLocation}
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue, IssueLogger, WARNING_CATEGORY}
import com.nawforce.pkgforce.path.PathLike

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

/**
  * This should be moved to pkgforce alongside the outline parser
  */
private[opcst] class CodeOutlineParserLogger(path: PathLike) extends IssueLogger {

  private var issueLog: ArrayBuffer[Issue] = _

  def isEmpty: Boolean = issueLog == null

  override def log(issue: Issue): Unit = {
    if (issueLog == null)
      issueLog = new ArrayBuffer[Issue]()
    issueLog.append(issue)
  }

  def logError(location: OPLocation, message: String): Unit = {
    log(Issue(path, Diagnostic(ERROR_CATEGORY, LocationUtils.toLocation(location), message)))
  }

  def logWarning(location: OPLocation, message: String): Unit = {
    log(Issue(path, Diagnostic(WARNING_CATEGORY, LocationUtils.toLocation(location), message)))
  }

  def issues: ArraySeq[Issue] = {
    if (isEmpty)
      Issue.emptyArray
    else
      ArraySeq.unsafeWrapArray(issueLog.toArray)
  }
}
