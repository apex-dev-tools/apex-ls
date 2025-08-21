/*
 [The "BSD licence"]
 Copyright (c) 2017 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.nawforce.pkgforce.diagnostics

import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.parsers.CodeParser
import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import io.github.apexdevtools.api.{IssueLocation, Issue => APIIssue}
import io.github.apexdevtools.apexls.api.IssuesCollection
import com.nawforce.pkgforce.diagnostics.Issue.APEX_LS_PROVIDER
import com.nawforce.runtime.platform.Path

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Issue management class that holds Issues for each metadata file and tracks when they change
  * to allow clients to be more selective when pulling issues.
  *
  * Note: To support virtual filesystem testing then we need to have *Internal methods that pass
  * a PathLike, the public API methods use strings for simplicity.
  */
class IssueLogger(val externalPathFilter: Option[PathLike => Boolean] = None)
    extends IssuesCollection {

  // === CORE STATE ===
  private[diagnostics] val log             = mutable.HashMap[PathLike, List[Issue]]() withDefaultValue List()
  private[diagnostics] val possibleMissing = mutable.HashSet[PathLike]()
  private[diagnostics] val hasChanged      = mutable.HashSet[PathLike]()

  // === CORE ISSUE LOGGING METHODS ===
  
  def isEmpty: Boolean = log.isEmpty

  def nonEmpty: Boolean = log.nonEmpty

  def hasErrors: Boolean = log.values.exists(issueList => issueList.exists(_.isError))

  def log(issue: Issue): Unit = add(issue)

  def logAll(issues: ArraySeq[Issue]): Unit = issues.foreach(log)

  def logAndGet[T](andIssues: IssuesAnd[T]): T = {
    logAll(andIssues.issues)
    andIssues.value
  }

  def logError(path: PathLike, location: Location, message: String): Unit = {
    log(Issue(path, Diagnostic(ERROR_CATEGORY, location, message)))
  }

  def logWarning(path: PathLike, location: Location, message: String): Unit = {
    log(Issue(path, Diagnostic(WARNING_CATEGORY, location, message)))
  }

  /** Get all issues as an ArraySeq, compatible with CatchingLogger.issues */
  def issues: ArraySeq[Issue] = {
    val buffer = mutable.ArrayBuffer[Issue]()
    log.values.foreach(issueList => buffer.addAll(issueList))
    ArraySeq.unsafeWrapArray(buffer.toArray)
  }

  // === ISSUESCOLLECTION INTERFACE METHODS (PUBLIC API) ===

  override def hasUpdatedIssues: Array[String] = {
    hasChanged.map(_.toString).toArray
  }

  override def ignoreUpdatedIssues(path: String): Unit = {
    ignoreUpdatedIssuesInternal(Path(path))
  }

  override def issuesForFile(path: String): Array[APIIssue] = {
    issuesForFilesInternal(Array(Path(path))).toArray
  }

  override def issuesForFiles(
    paths: Array[String],
    includeWarnings: Boolean,
    maxIssuesPerFile: Int
  ): Array[APIIssue] = {
    val internalPaths: Array[PathLike] =
      Option(paths).map(paths => paths.map(Path.apply(_).asInstanceOf[PathLike])).orNull
    issuesForFilesInternal(internalPaths, includeWarnings, maxIssuesPerFile).toArray
  }

  /** This is specialised accessor to aid PMD integration. */
  override def issuesForFileLocation(path: String, location: IssueLocation): Array[APIIssue] = {
    issuesForFileLocationInternal(Path(path), location)
  }

  // === CORE OPERATIONS ===

  def add(issue: Issue): Unit = {
    if (shouldStoreIssue(issue)) {
      hasChanged.add(issue.path)
      log.put(issue.path, issue :: log(issue.path))
      if (issue.diagnostic.category == MISSING_CATEGORY)
        possibleMissing.add(issue.path)
    }
  }

  def clear(): Unit = {
    hasChanged.clear()
    log.clear()
  }

  def pop(path: PathLike): List[Issue] = {
    val issues = log.getOrElse(path, Nil)
    log.remove(path)
    if (issues.nonEmpty)
      hasChanged.add(path)
    issues
  }

  def push(path: PathLike, issues: List[Issue]): Unit = {
    hasChanged.add(path)
    val filteredIssues = issues.filter(shouldStoreIssue)
    if (filteredIssues.nonEmpty)
      log.put(path, filteredIssues)
  }

  // === INTERNAL HELPERS ===

  def ignoreUpdatedIssuesInternal(path: PathLike): Unit = {
    hasChanged.remove(path)
  }

  def issuesForFileInternal(path: PathLike): Seq[Issue] = {
    issuesForFilesInternal(Array(path))
  }

  def issuesForFilesInternal(
    paths: Array[PathLike],
    includeWarnings: Boolean = true,
    maxIssuesPerFile: Int = 0
  ): Seq[Issue] = {
    val files =
      if (paths == null || paths.isEmpty)
        log.keys.toSeq.sortBy(_.toString)
      else
        paths.toSeq

    val buffer = mutable.ArrayBuffer[Issue]()
    files.foreach(file => {
      var fileIssues = log
        .getOrElse(file, Nil)
        .filter(issue => {
          // Only apply warning filter - external path filtering is now done at write time
          includeWarnings || DiagnosticCategory.isErrorType(issue.diagnostic.category)
        })
        .sorted(Issue.ordering)
      if (maxIssuesPerFile > 0)
        fileIssues = fileIssues.take(maxIssuesPerFile)
      buffer.addAll(fileIssues)
      hasChanged.remove(file)
    })
    buffer.toSeq
  }

  def issuesForFileLocationInternal(path: PathLike, location: IssueLocation): Array[APIIssue] = {
    val loc = Location(
      location.startLineNumber(),
      location.startCharOffset(),
      location.endLineNumber(),
      location.endCharOffset()
    )
    log
      .getOrElse(path, Nil)
      .filter(issue => loc.contains(issue.diagnostic.location))
      .toArray[APIIssue]
  }

  def getDiagnostics(path: PathLike): List[Diagnostic] =
    log.getOrElse(path, Nil).map(_.diagnostic)

  private[diagnostics] def shouldStoreIssue(issue: Issue): Boolean = {
    externalPathFilter match {
      case Some(filter) if filter(issue.path) =>
        // For external paths, only store errors
        DiagnosticCategory.isErrorType(issue.diagnostic.category)
      case _ =>
        // For internal paths, store all issues
        true
    }
  }


}


/** Helper methods for specialized issue analysis operations */
object IssueAnalysis {

  /** Check if a file has syntax issues from apex-ls provider */
  def hasSyntaxIssues(logger: IssueLogger, path: PathLike): Boolean = {
    logger.log
      .getOrElse(path, Nil)
      .exists(issue =>
        issue.diagnostic.category == SYNTAX_CATEGORY && issue.provider == Issue.APEX_LS_PROVIDER
      )
  }

  /** Get paths that have missing dependency issues */
  def getMissing(logger: IssueLogger): Seq[PathLike] = {
    val missing = new mutable.ArrayBuffer[PathLike]()
    logger.possibleMissing.foreach(possible => {
      val issues = logger.log.getOrElse(possible, Nil).filter(_.diagnostic.category == MISSING_CATEGORY)
      if (issues.nonEmpty) {
        missing.append(possible)
      }
    })
    logger.possibleMissing.clear()
    missing.foreach(logger.possibleMissing.add)
    missing.toSeq
  }
}

/** Helper methods for provider-specific issue management operations */
object IssueProviderOps {

  /** Clear all issues except those from apex-ls provider */
  def clearProviderIssues(logger: IssueLogger, path: PathLike): Unit = {
    logger.hasChanged.add(path)
    logger.log.put(path, logger.log.getOrElse(path, Nil).filter(_.provider == Issue.APEX_LS_PROVIDER))
  }

  /** Replace issues from a specific provider */
  def replaceProviderIssues(
    logger: IssueLogger,
    providerId: String,
    path: PathLike,
    issues: Seq[Issue]
  ): Unit = {
    logger.hasChanged.add(path)
    val filteredNewIssues = issues.filter(logger.shouldStoreIssue)
    logger.log.put(path, logger.log.getOrElse(path, Nil).filterNot(_.provider == providerId) ++ filteredNewIssues)
  }

  /** Replace unused issues while preserving other issue types */
  def replaceUnusedIssues(logger: IssueLogger, path: PathLike, issues: Seq[Issue]): Unit = {
    logger.hasChanged.add(path)
    val filteredNewIssues = issues.filter(logger.shouldStoreIssue)
    val newIssues =
      logger.log(path).filterNot(_.diagnostic.category == UNUSED_CATEGORY) ++ filteredNewIssues
    if (newIssues.isEmpty)
      logger.log.remove(path)
    else
      logger.log.put(path, newIssues)
  }
}
