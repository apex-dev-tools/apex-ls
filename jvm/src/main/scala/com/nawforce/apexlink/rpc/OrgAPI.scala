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

package com.nawforce.apexlink.rpc

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.names.{Name, TypeIdentifier, TypeName}
import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation}
import com.nawforce.runtime.platform.Path
import io.github.shogowada.scala.jsonrpc.api
import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler
import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler.{macroRW, ReadWriter => RW}

import scala.concurrent.Future

case class APIError(message: String, stack: Array[String])

object APIError {
  implicit val rw: RW[APIError] = macroRW

  def apply(ex: Throwable): APIError = {
    new APIError(ex.getMessage, ex.getStackTrace.map(_.toString))
  }

  def apply(message: String): APIError = {
    new APIError(message, Array())
  }
}

case class OpenResult(error: Option[APIError], namespaces: Array[String])

object OpenResult {
  implicit val rw: RW[OpenResult] = macroRW
}

case class GetIssuesResult(issues: Array[Issue])

object GetIssuesResult {
  implicit val rw: RW[GetIssuesResult]                              = macroRW
  implicit val rwIssue: RW[com.nawforce.pkgforce.diagnostics.Issue] = macroRW
  implicit val rwDiagnostic: RW[Diagnostic]                         = macroRW
  implicit val rwDiagnosticCategory: RW[DiagnosticCategory]         = macroRW
  implicit val rwLocation: RW[Location]                             = macroRW
  implicit val rwPathLike: RW[PathLike] =
    JSONRPCPickler.readwriter[String].bimap[PathLike](_.toString, Path(_))
}

case class IssuesResult(issues: Array[Issue])

object IssuesResult {
  implicit val rw: RW[IssuesResult]                                 = macroRW
  implicit val rwIssue: RW[com.nawforce.pkgforce.diagnostics.Issue] = macroRW
  implicit val rwDiagnostic: RW[Diagnostic]                         = macroRW
  implicit val rwDiagnosticCategory: RW[DiagnosticCategory]         = macroRW
  implicit val rwLocation: RW[Location]                             = macroRW
  implicit val rwPathLike: RW[PathLike] =
    JSONRPCPickler.readwriter[String].bimap[PathLike](_.toString, Path(_))
}

case class GetTypeIdentifiersResult(identifiers: Array[TypeIdentifier])

object GetTypeIdentifiersResult {
  implicit val rw: RW[GetTypeIdentifiersResult]     = macroRW
  implicit val rwTypeIdentifier: RW[TypeIdentifier] = macroRW
  implicit val rwTypeName: RW[TypeName]             = macroRW
  implicit val rwName: RW[Name]                     = macroRW
}

case class IdentifierLocationResult(pathLocation: PathLocation)

object IdentifierLocationResult {
  implicit val rw: RW[IdentifierLocationResult] = macroRW
  implicit val rwPathLocation: RW[PathLocation] = macroRW
  implicit val rwLocation: RW[Location]         = macroRW
  implicit val rwPathLike: RW[PathLike] =
    JSONRPCPickler.readwriter[String].bimap[PathLike](_.toString, Path(_))
}

case class IdentifierRequest(identifier: TypeIdentifier)

object IdentifierRequest {
  implicit val rw: RW[IdentifierRequest]            = macroRW
  implicit val rwTypeIdentifier: RW[TypeIdentifier] = macroRW
  implicit val rwTypeName: RW[TypeName]             = macroRW
  implicit val rwName: RW[Name]                     = macroRW
}

case class IdentifiersRequest(identifiers: Array[TypeIdentifier])

object IdentifiersRequest {
  implicit val rw: RW[IdentifiersRequest]           = macroRW
  implicit val rwTypeIdentifier: RW[TypeIdentifier] = macroRW
  implicit val rwTypeName: RW[TypeName]             = macroRW
  implicit val rwName: RW[Name]                     = macroRW
}

case class IdentifierForPathResult(identifier: Option[TypeIdentifier])

object IdentifierForPathResult {
  implicit val rw: RW[IdentifierForPathResult]      = macroRW
  implicit val rwTypeIdentifier: RW[TypeIdentifier] = macroRW
  implicit val rwTypeName: RW[TypeName]             = macroRW
  implicit val rwName: RW[Name]                     = macroRW
}

case class BombScore(identifier: TypeIdentifier, usedBy: Int, uses: Int, score: Double)

object BombScore {
  implicit val rw: RW[BombScore]                    = macroRW
  implicit val rwTypeIdentifier: RW[TypeIdentifier] = macroRW
  implicit val rwTypeName: RW[TypeName]             = macroRW
  implicit val rwName: RW[Name]                     = macroRW
}

case class GetTestClassNamesRequest(paths: Array[String])

object GetTestClassNamesRequest {
  implicit val rw: RW[GetTestClassNamesRequest] = macroRW
}

case class GetTestClassNamesResult(testClassesWithPath: Array[(String, Array[String])])

object GetTestClassNamesResult {
  implicit val rw: RW[GetTestClassNamesResult] = macroRW
}

case class GetDependencyCountsRequest(paths: Array[String], excludeTestClasses: Boolean)

object GetDependencyCountsRequest {
  implicit val rw: RW[GetDependencyCountsRequest] = macroRW
}

case class DependencyCount(
  path: String,
  count: Int,
  maxDependencyCount: Either[Option[String], Int]
)

object DependencyCount {
  implicit val rw: RW[DependencyCount] = macroRW
}

case class GetDependencyCountsResult(counts: Array[DependencyCount])

object GetDependencyCountsResult {
  implicit val rw: RW[GetDependencyCountsResult] = macroRW
}

case class GetAllDependencyCountsRequest(directoryScope: String, excludeTestClasses: Boolean)

object GetAllDependencyCountsRequest {
  implicit val rw: RW[GetAllDependencyCountsRequest] = macroRW
}

case class GetAllDependencyCountsResult(counts: Array[DependencyCount])

object GetAllDependencyCountsResult {
  implicit val rw: RW[GetAllDependencyCountsResult] = macroRW
}

case class TestClassItemsResult(items: Array[ClassTestItem])

object TestClassItemsResult {
  implicit val rw: RW[TestClassItemsResult]         = macroRW
  implicit val rwClassTestItem: RW[ClassTestItem]   = macroRW
  implicit val rwTargetLocation: RW[TargetLocation] = macroRW
  implicit val rwLocation: RW[Location]             = macroRW
}

case class TestMethodItemsResult(items: Array[MethodTestItem])

object TestMethodItemsResult {
  implicit val rw: RW[TestMethodItemsResult]        = macroRW
  implicit val rwClassTestItem: RW[MethodTestItem]  = macroRW
  implicit val rwTargetLocation: RW[TargetLocation] = macroRW
  implicit val rwLocation: RW[Location]             = macroRW
}

case class OpenOptions private (
  parser: Option[String] = None,
  loggingLevel: Option[String] = None,
  externalAnalysisMode: Option[(String, Map[String, List[(String, List[String])]])] = None,
  cache: Option[Boolean] = None,
  cacheDirectory: Option[String] = None,
  indexerConfiguration: Option[(Long, Long)] = None,
  autoFlush: Option[Boolean] = None,
  unused: Option[Boolean] = None
) {
  def withParser(name: String): OpenOptions = {
    copy(parser = Some(name))
  }

  def withLoggingLevel(level: String): OpenOptions = {
    copy(loggingLevel = Some(level))
  }

  def withExternalAnalysisMode(
    mode: String,
    params: Map[String, List[(String, List[String])]]
  ): OpenOptions = {
    copy(externalAnalysisMode = Some(mode, params))
  }

  def withCacheDirectory(path: String): OpenOptions = {
    copy(cacheDirectory = Option(path).filter(_.nonEmpty))
  }

  def withCache(enabled: Boolean): OpenOptions = {
    copy(cache = Some(enabled))
  }

  def withIndexerConfiguration(
    rescanTriggerTimeMs: Long,
    quietPeriodForRescanMs: Long
  ): OpenOptions = {
    copy(indexerConfiguration = Some((rescanTriggerTimeMs, quietPeriodForRescanMs)))
  }

  def withAutoFlush(enabled: Boolean): OpenOptions = {
    copy(autoFlush = Some(enabled))
  }

  def withUnused(enabled: Boolean): OpenOptions = {
    copy(unused = Some(enabled))
  }
}

object OpenOptions {
  implicit val rw: RW[OpenOptions] = macroRW

  def default(): OpenOptions = {
    new OpenOptions()
  }
}

trait OrgAPI {
  @api.JSONRPCMethod(name = "version")
  def version(): Future[String]

  @api.JSONRPCMethod(name = "setParser")
  def setParser(parser: String): Future[Unit]

  @api.JSONRPCMethod(name = "setLoggingLevel")
  def setLoggingLevel(level: String): Future[Unit]

  @api.JSONRPCMethod(name = "setExternalAnalysisMode")
  def setExternalAnalysisMode(mode: String): Future[Unit]

  @api.JSONRPCMethod(name = "setCacheDirectory")
  def setCacheDirectory(path: Option[String]): Future[Unit]

  @api.JSONRPCMethod(name = "open")
  def open(directory: String): Future[OpenResult]

  @api.JSONRPCMethod(name = "openWithOptions")
  def open(directory: String, options: OpenOptions): Future[OpenResult]

  @api.JSONRPCMethod(name = "getIssues")
  def getIssues(includeWarnings: Boolean, maxIssuesPerFile: Int): Future[GetIssuesResult]

  @api.JSONRPCMethod(name = "hasUpdatedIssues")
  def hasUpdatedIssues: Future[Array[String]]

  @api.JSONRPCMethod(name = "ignoreUpdatedIssues")
  def ignoreUpdatedIssues(path: String): Future[Unit]

  @api.JSONRPCMethod(name = "issuesForFile")
  def issuesForFile(path: String): Future[IssuesResult]

  @api.JSONRPCMethod(name = "issuesForFiles")
  def issuesForFiles(
    paths: Array[String],
    includeWarnings: Boolean,
    maxErrorsPerFile: Int
  ): Future[IssuesResult]

  @api.JSONRPCMethod(name = "refresh")
  def refresh(path: String, highPriority: Boolean): Future[Unit]

  @api.JSONRPCMethod(name = "getTypeIdentifiers")
  def typeIdentifiers(apexOnly: Boolean): Future[GetTypeIdentifiersResult]

  @api.JSONRPCMethod(name = "dependencyGraph")
  def dependencyGraph(
    identifiers: IdentifiersRequest,
    depth: Int,
    apexOnly: Boolean,
    ignoring: IdentifiersRequest
  ): Future[DependencyGraph]

  @api.JSONRPCMethod(name = "identifierLocation")
  def identifierLocation(identifier: IdentifierRequest): Future[IdentifierLocationResult]

  @api.JSONRPCMethod(name = "identifierForPath")
  def identifierForPath(path: String): Future[IdentifierForPathResult]

  @api.JSONRPCMethod(name = "getDefinition")
  def getDefinition(
    path: String,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Future[Array[LocationLink]]

  @api.JSONRPCMethod(name = "getImplementation")
  def getImplementation(
    path: String,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Future[Array[LocationLink]]

  @api.JSONRPCMethod(name = "getReferences")
  def getReferences(path: String, line: Int, offset: Int): Future[Array[TargetLocation]]

  @api.JSONRPCMethod(name = "getDependencyBombs")
  def getDependencyBombs(count: Int): Future[Array[BombScore]]

  @api.JSONRPCMethod(name = "getTestClassNames")
  def getTestClassNames(paths: GetTestClassNamesRequest): Future[GetTestClassNamesResult]

  @api.JSONRPCMethod(name = "getTestClassItems")
  def getTestClassItems(paths: Array[String]): Future[TestClassItemsResult]

  @api.JSONRPCMethod(name = "getTestClassItemsChanged")
  def getTestClassItemsChanged(paths: Array[String]): Future[TestClassItemsResult]

  @api.JSONRPCMethod(name = "getTestMethodItems")
  def getTestMethodItems(paths: Array[String]): Future[TestMethodItemsResult]

  @api.JSONRPCMethod(name = "getDependencyCounts")
  def getDependencyCounts(request: GetDependencyCountsRequest): Future[GetDependencyCountsResult]

  @api.JSONRPCMethod(name = "getAllDependencyCounts")
  def getAllDependencyCounts(
    request: GetAllDependencyCountsRequest
  ): Future[GetAllDependencyCountsResult]

  @api.JSONRPCMethod(name = "getCompletionItems")
  def getCompletionItems(
    path: String,
    line: Int,
    offset: Int,
    content: String
  ): Future[Array[CompletionItemLink]]
}

object OrgAPI {
  // Just a test entry point
  def apply(): OrgAPI = new OrgAPIImpl()
}
