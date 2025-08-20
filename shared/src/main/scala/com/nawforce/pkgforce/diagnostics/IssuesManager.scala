/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.diagnostics

import io.github.apexdevtools.api.{IssueLocation, Issue => APIIssue}
import io.github.apexdevtools.apexls.api.IssuesCollection
import com.nawforce.pkgforce.diagnostics
import com.nawforce.pkgforce.diagnostics.Issue.APEX_LS_PROVIDER
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.platform.Path

import scala.collection.mutable

/** IssuesCollection implementation, holds Issues for each metadata file and tracks when they change
  * to allow clients to be more selective when pulling issues.
  *
  * Note: To support virtual filesystem testing then we need to have *Internal methods that pass
  * a PathLike, the public API methods use strings for simplicity.
  */
class IssuesManager(val externalPathFilter: Option[PathLike => Boolean] = None)
    extends IssuesCollection
    with IssueLogger {
  private val log             = mutable.HashMap[PathLike, List[Issue]]() withDefaultValue List()
  private val possibleMissing = mutable.HashSet[PathLike]()
  private val hasChanged      = mutable.HashSet[PathLike]()

  def isEmpty: Boolean = log.isEmpty

  def nonEmpty: Boolean = log.nonEmpty

  override def hasErrors: Boolean = log.values.exists(issueList => issueList.exists(_.isError))

  override def log(issue: Issue): Unit = add(issue)

  private def shouldStoreIssue(issue: Issue): Boolean = {
    externalPathFilter match {
      case Some(filter) if filter(issue.path) =>
        // For external paths, only store errors
        DiagnosticCategory.isErrorType(issue.diagnostic.category)
      case _ =>
        // For internal paths, store all issues
        true
    }
  }

  def clear(): Unit = {
    hasChanged.clear()
    log.clear()
  }

  def add(issue: diagnostics.Issue): Unit = {
    if (shouldStoreIssue(issue)) {
      hasChanged.add(issue.path)
      log.put(issue.path, issue :: log(issue.path))
      if (issue.diagnostic.category == MISSING_CATEGORY)
        possibleMissing.add(issue.path)
    }
  }

  def pop(path: PathLike): List[diagnostics.Issue] = {
    val issues = log.getOrElse(path, Nil)
    log.remove(path)
    if (issues.nonEmpty)
      hasChanged.add(path)
    issues
  }

  def push(path: PathLike, issues: List[diagnostics.Issue]): Unit = {
    hasChanged.add(path)
    val filteredIssues = issues.filter(shouldStoreIssue)
    if (filteredIssues.nonEmpty)
      log.put(path, filteredIssues)
  }

  def replaceUnusedIssues(path: PathLike, issues: Seq[diagnostics.Issue]): Unit = {
    hasChanged.add(path)
    val filteredNewIssues = issues.filter(shouldStoreIssue)
    val newIssues =
      log(path).filterNot(_.diagnostic.category == UNUSED_CATEGORY) ++ filteredNewIssues
    if (newIssues.isEmpty)
      log.remove(path)
    else
      log.put(path, newIssues)
  }

  def clearProviderIssues(path: PathLike): Unit = {
    hasChanged.add(path)
    log.put(path, log.getOrElse(path, Nil).filter(_.provider == APEX_LS_PROVIDER))
  }

  def replaceProviderIssues(
    providerId: String,
    path: PathLike,
    issues: Seq[diagnostics.Issue]
  ): Unit = {
    hasChanged.add(path)
    val filteredNewIssues = issues.filter(shouldStoreIssue)
    log.put(path, log.getOrElse(path, Nil).filterNot(_.provider == providerId) ++ filteredNewIssues)
  }

  def hasSyntaxIssues(path: PathLike): Boolean = {
    log
      .getOrElse(path, Nil)
      .exists(issue =>
        issue.diagnostic.category == SYNTAX_CATEGORY && issue.provider == APEX_LS_PROVIDER
      )
  }

  override def hasUpdatedIssues: Array[String] = {
    hasChanged.map(_.toString).toArray
  }

  override def ignoreUpdatedIssues(path: String): Unit = {
    ignoreUpdatedIssuesInternal(Path(path))
  }

  def ignoreUpdatedIssuesInternal(path: PathLike): Unit = {
    hasChanged.remove(path)
  }

  override def issuesForFile(path: String): Array[APIIssue] = {
    issuesForFilesInternal(Array(Path(path))).toArray
  }

  def issuesForFileInternal(path: PathLike): Seq[Issue] = {
    issuesForFilesInternal(Array(path))
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

  /** This is specialised accessor to aid PMD integration. */
  override def issuesForFileLocation(path: String, location: IssueLocation): Array[APIIssue] = {
    issuesForFileLocationInternal(Path(path), location)
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

  def getMissing: Seq[PathLike] = {
    val missing = new mutable.ArrayBuffer[PathLike]()
    possibleMissing.foreach(possible => {
      val issues = log.getOrElse(possible, Nil).filter(_.diagnostic.category == MISSING_CATEGORY)
      if (issues.nonEmpty) {
        missing.append(possible)
      }
    })
    possibleMissing.clear()
    missing.foreach(possibleMissing.add)
    missing.toSeq
  }
}
