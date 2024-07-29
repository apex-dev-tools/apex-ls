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

import com.nawforce.apexlink.api.{ExternalAnalysisConfiguration, Org, ServerOps}
import com.nawforce.apexlink.org.{OPM, OrgInfo}
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.names.TypeIdentifier
import com.nawforce.runtime.platform.{Environment, Path}

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

trait APIRequest {
  def process(org: OrgQueue): Unit
}

class OrgQueue(path: String, options: OpenOptions) {
  self =>
  val org: Org = Org.newOrg(Path(path), options)

  private val queue      = new LinkedBlockingQueue[APIRequest]()
  private val dispatcher = new APIRequestDispatcher()
  new Thread(dispatcher).start()

  private class APIRequestDispatcher() extends Runnable {

    override def run(): Unit = {
      while (true) {
        val request = queue.take()

        while (org.isDirty()) Thread.sleep(50)

        request.process(self)
      }
    }
  }

  def add(request: APIRequest): Unit =
    queue.add(request)

  def refresh(path: String, highPriority: Boolean): Unit = {
    Option(org.getPackageForPath(path)).foreach(_.refresh(path, highPriority))
  }
}

case class OpenRequest(promise: Promise[OpenResult]) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    promise.success(try {
      val namespaces = queue.org.getPackages().flatMap(_.getNamespaces(false))
      OpenResult(None, namespaces)
    } catch {
      case ex: IllegalArgumentException => OpenResult(Some(APIError(ex.getMessage)), Array())
      case ex: Throwable                => OpenResult(Some(APIError(ex)), Array())
    })
  }
}

object OpenRequest {
  def apply(queue: OrgQueue): Future[OpenResult] = {
    val promise = Promise[OpenResult]()
    queue.add(new OpenRequest(promise))
    promise.future
  }
}

case class GetIssues(
  promise: Promise[GetIssuesResult],
  includeWarnings: Boolean,
  maxIssuesPerFile: Int
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {

    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(
        GetIssuesResult(
          orgImpl.issueManager
            .issuesForFilesInternal(null, includeWarnings, maxIssuesPerFile)
            .toArray
        )
      )
    }
  }
}

object GetIssues {
  def apply(
    queue: OrgQueue,
    includeWarnings: Boolean,
    maxIssuesPerFile: Int
  ): Future[GetIssuesResult] = {
    val promise = Promise[GetIssuesResult]()
    queue.add(new GetIssues(promise, includeWarnings, maxIssuesPerFile))
    promise.future
  }
}

case class HasUpdatedIssues(promise: Promise[Array[String]]) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(orgImpl.issues.hasUpdatedIssues)
    }
  }
}

object HasUpdatedIssues {
  def apply(queue: OrgQueue): Future[Array[String]] = {
    val promise = Promise[Array[String]]()
    queue.add(new HasUpdatedIssues(promise))
    promise.future
  }
}

case class IgnoreUpdatedIssues(promise: Promise[Unit], path: String) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(orgImpl.issues.ignoreUpdatedIssues(path))
    }
  }
}

object IgnoreUpdatedIssues {
  def apply(queue: OrgQueue, path: String): Future[Unit] = {
    val promise = Promise[Unit]()
    queue.add(new IgnoreUpdatedIssues(promise, path))
    promise.future
  }
}

case class IssuesForFile(promise: Promise[IssuesResult], path: String) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(IssuesResult(orgImpl.issues.issuesForFileInternal(Path(path)).toArray))
    }
  }
}

object IssuesForFile {
  def apply(queue: OrgQueue, path: String): Future[IssuesResult] = {
    val promise = Promise[IssuesResult]()
    queue.add(new IssuesForFile(promise, path))
    promise.future
  }
}

case class IssuesForFiles(
  promise: Promise[IssuesResult],
  paths: Array[String],
  includeWarnings: Boolean,
  maxErrorsPerFile: Int
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(
        IssuesResult(
          orgImpl.issues
            .issuesForFilesInternal(paths.map(Path(_)), includeWarnings, maxErrorsPerFile)
            .toArray
        )
      )
    }
  }
}

object IssuesForFiles {
  def apply(
    queue: OrgQueue,
    paths: Array[String],
    includeWarnings: Boolean,
    maxErrorsPerFile: Int
  ): Future[IssuesResult] = {
    val promise = Promise[IssuesResult]()
    queue.add(new IssuesForFiles(promise, paths, includeWarnings, maxErrorsPerFile))
    promise.future
  }
}

case class TypeIdentifiers(promise: Promise[GetTypeIdentifiersResult], apexOnly: Boolean)
    extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(GetTypeIdentifiersResult(orgImpl.getTypeIdentifiers(apexOnly)))
    }
  }
}

object TypeIdentifiers {
  def apply(queue: OrgQueue, apexOnly: Boolean): Future[GetTypeIdentifiersResult] = {
    val promise = Promise[GetTypeIdentifiersResult]()
    queue.add(new TypeIdentifiers(promise, apexOnly))
    promise.future
  }
}

case class DependencyGraphRequest(
  promise: Promise[DependencyGraph],
  identifiers: Array[TypeIdentifier],
  depth: Int,
  apexOnly: Boolean,
  ignoring: Array[TypeIdentifier]
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    promise.success(queue.org.getDependencyGraph(identifiers, depth, apexOnly, ignoring))
  }
}

object DependencyGraphRequest {
  def apply(
    queue: OrgQueue,
    identifiers: Array[TypeIdentifier],
    depth: Int,
    apexOnly: Boolean,
    ignoring: Array[TypeIdentifier]
  ): Future[DependencyGraph] = {
    val promise = Promise[DependencyGraph]()
    queue.add(new DependencyGraphRequest(promise, identifiers, depth, apexOnly, ignoring))
    promise.future
  }
}

case class IdentifierLocation(
  promise: Promise[IdentifierLocationResult],
  identifier: TypeIdentifier
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    promise.success(IdentifierLocationResult(queue.org.getIdentifierLocation(identifier)))
  }
}

object IdentifierLocation {
  def apply(queue: OrgQueue, identifier: TypeIdentifier): Future[IdentifierLocationResult] = {
    val promise = Promise[IdentifierLocationResult]()
    queue.add(new IdentifierLocation(promise, identifier))
    promise.future
  }
}

case class IdentifierForPath(promise: Promise[IdentifierForPathResult], path: String)
    extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      val types = orgImpl.packagesByNamespace.values.flatMap(pkg => Option(pkg.getTypeOfPath(path)))
      promise.success(IdentifierForPathResult(types.headOption))
    }
  }
}

object IdentifierForPath {
  def apply(queue: OrgQueue, identifier: String): Future[IdentifierForPathResult] = {
    val promise = Promise[IdentifierForPathResult]()
    queue.add(new IdentifierForPath(promise, identifier))
    promise.future
  }
}

case class GetDefinition(
  promise: Promise[Array[LocationLink]],
  path: String,
  line: Int,
  offset: Int,
  content: Option[String]
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(orgImpl.getDefinition(path, line, offset, content.orNull))
  }
}

object GetDefinition {
  def apply(
    queue: OrgQueue,
    path: String,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Future[Array[LocationLink]] = {
    val promise = Promise[Array[LocationLink]]()
    queue.add(new GetDefinition(promise, path, line, offset, content))
    promise.future
  }
}

case class GetImplementation(
  promise: Promise[Array[LocationLink]],
  path: String,
  line: Int,
  offset: Int,
  content: Option[String]
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(orgImpl.getImplementation(path, line, offset, content.orNull))
  }
}

object GetImplementation {
  def apply(
    queue: OrgQueue,
    path: String,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Future[Array[LocationLink]] = {
    val promise = Promise[Array[LocationLink]]()
    queue.add(new GetImplementation(promise, path, line, offset, content))
    promise.future
  }
}

case class GetReferences(
  promise: Promise[Array[TargetLocation]],
  path: String,
  line: Int,
  offset: Int
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(orgImpl.getReferences(path, line, offset))
  }
}

object GetReferences {
  def apply(
    queue: OrgQueue,
    path: String,
    line: Int,
    offset: Int
  ): Future[Array[TargetLocation]] = {
    val promise = Promise[Array[TargetLocation]]()
    queue.add(new GetReferences(promise, path, line, offset))
    promise.future
  }
}

case class GetDependencyBombs(promise: Promise[Array[BombScore]], count: Int) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(orgImpl.getDependencyBombs(count))
    }
  }
}

object GetDependencyBombs {
  def apply(queue: OrgQueue, count: Int): Future[Array[BombScore]] = {
    val promise = Promise[Array[BombScore]]()
    queue.add(new GetDependencyBombs(promise, count))
    promise.future
  }
}

case class GetTestClassNames(promise: Promise[GetTestClassNamesResult], paths: Array[String])
    extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(
        GetTestClassNamesResult(orgImpl.getTestClassNamesInternal(paths.map(p => Path(p))).toArray)
      )
    }
  }
}

object GetTestClassNames {
  def apply(queue: OrgQueue, paths: Array[String]): Future[GetTestClassNamesResult] = {
    val promise = Promise[GetTestClassNamesResult]()
    queue.add(new GetTestClassNames(promise, paths))
    promise.future
  }
}

case class GetDependencyCounts(
  promise: Promise[GetDependencyCountsResult],
  request: GetDependencyCountsRequest
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(
        GetDependencyCountsResult(
          orgImpl
            .getDependencyCounts(request.paths, request.excludeTestClasses)
        )
      )
    }
  }
}

object GetDependencyCounts {
  def apply(
    queue: OrgQueue,
    request: GetDependencyCountsRequest
  ): Future[GetDependencyCountsResult] = {
    val promise = Promise[GetDependencyCountsResult]()
    queue.add(new GetDependencyCounts(promise, request))
    promise.future
  }
}

case class GetAllDependencyCounts(
  promise: Promise[GetAllDependencyCountsResult],
  request: GetAllDependencyCountsRequest
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    OrgInfo.current.withValue(orgImpl) {
      promise.success(
        GetAllDependencyCountsResult(
          orgImpl
            .getAllDependencyCounts(request.directoryScope, request.excludeTestClasses)
        )
      )
    }
  }
}

object GetAllDependencyCounts {
  def apply(
    queue: OrgQueue,
    request: GetAllDependencyCountsRequest
  ): Future[GetAllDependencyCountsResult] = {
    val promise = Promise[GetAllDependencyCountsResult]()
    queue.add(new GetAllDependencyCounts(promise, request))
    promise.future
  }
}

case class GetCompletionItems(
  promise: Promise[Array[CompletionItemLink]],
  path: String,
  line: Int,
  offset: Int,
  content: String
) extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(orgImpl.getCompletionItems(path, line, offset, content))
  }
}

object GetCompletionItems {
  def apply(
    queue: OrgQueue,
    path: String,
    line: Int,
    offset: Int,
    content: String
  ): Future[Array[CompletionItemLink]] = {
    val promise = Promise[Array[CompletionItemLink]]()
    queue.add(new GetCompletionItems(promise, path, line, offset, content))
    promise.future
  }
}

case class GetTestClassItems(promise: Promise[TestClassItemsResult], paths: Array[String])
    extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(TestClassItemsResult(orgImpl.getTestClassItems(paths)))
  }
}

object GetTestClassItems {
  def apply(queue: OrgQueue, paths: Array[String]): Future[TestClassItemsResult] = {
    val promise = Promise[TestClassItemsResult]()
    queue.add(new GetTestClassItems(promise, paths))
    promise.future
  }
}

case class GetTestClassItemsChanged(promise: Promise[TestClassItemsResult], paths: Array[String])
    extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(TestClassItemsResult(orgImpl.getTestClassItemsChanged(paths)))
  }
}

object GetTestClassItemsChanged {
  def apply(queue: OrgQueue, paths: Array[String]): Future[TestClassItemsResult] = {
    val promise = Promise[TestClassItemsResult]()
    queue.add(new GetTestClassItemsChanged(promise, paths))
    promise.future
  }
}

case class GetTestMethodItems(promise: Promise[TestMethodItemsResult], paths: Array[String])
    extends APIRequest {
  override def process(queue: OrgQueue): Unit = {
    val orgImpl = queue.org.asInstanceOf[OPM.OrgImpl]
    promise.success(TestMethodItemsResult(orgImpl.getTestMethodItems(paths)))
  }
}

object GetTestMethodItems {
  def apply(queue: OrgQueue, paths: Array[String]): Future[TestMethodItemsResult] = {
    val promise = Promise[TestMethodItemsResult]()
    queue.add(new GetTestMethodItems(promise, paths))
    promise.future
  }
}

object OrgQueue {
  private var _instance: Option[OrgQueue] = None

  def open(path: String, options: OpenOptions = OpenOptions.default()): OrgQueue = {
    synchronized {
      _instance = Some(new OrgQueue(path, options))
      _instance.get
    }
  }

  def instance(): OrgQueue = {
    synchronized {
      _instance.get
    }
  }
}

class OrgAPIImpl extends OrgAPI {
  override def version(): Future[String] = {
    Future(classOf[OrgAPIImpl].getProtectionDomain.getCodeSource.getLocation.getPath)
  }

  override def setLoggingLevel(level: String): Future[Unit] = {
    LoggerOps.setLoggingLevel(level)
    Future.successful(())
  }

  override def setExternalAnalysisMode(mode: String): Future[Unit] = {
    ServerOps.setExternalAnalysis(ExternalAnalysisConfiguration(mode))
    Future.successful(())
  }

  override def setParser(parser: String): Future[Unit] = {
    ServerOps.setCurrentParser(parser)
    Future.successful(())
  }

  override def setCacheDirectory(path: Option[String]): Future[Unit] = {
    Environment.setCacheDirOverride(Some(path.filter(_.nonEmpty).map(p => Path(p))))
    ServerOps.setAutoFlush(path.nonEmpty)
    Future.successful(())
  }

  override def open(directory: String): Future[OpenResult] = {
    OrgQueue.open(directory)
    OpenRequest(OrgQueue.instance())
  }

  override def open(directory: String, options: OpenOptions): Future[OpenResult] = {
    OrgQueue.open(directory, options)
    OpenRequest(OrgQueue.instance())
  }

  override def getIssues(
    includeWarnings: Boolean,
    maxIssuesPerFile: Int
  ): Future[GetIssuesResult] = {
    GetIssues(OrgQueue.instance(), includeWarnings, maxIssuesPerFile)
  }

  override def hasUpdatedIssues: Future[Array[String]] = {
    HasUpdatedIssues(OrgQueue.instance())
  }

  override def ignoreUpdatedIssues(path: String): Future[Unit] = {
    IgnoreUpdatedIssues(OrgQueue.instance(), path)
  }

  override def issuesForFile(path: String): Future[IssuesResult] = {
    IssuesForFile(OrgQueue.instance(), path)
  }

  override def issuesForFiles(
    paths: Array[String],
    includeWarnings: Boolean,
    maxErrorsPerFile: Int
  ): Future[IssuesResult] = {
    IssuesForFiles(OrgQueue.instance(), paths, includeWarnings, maxErrorsPerFile)
  }

  override def refresh(path: String, highPriority: Boolean): Future[Unit] = {
    Future(OrgQueue.instance().refresh(path, highPriority))
  }

  override def typeIdentifiers(apexOnly: Boolean): Future[GetTypeIdentifiersResult] = {
    TypeIdentifiers(OrgQueue.instance(), apexOnly)
  }

  override def dependencyGraph(
    identifiers: IdentifiersRequest,
    depth: Int,
    apexOnly: Boolean,
    ignoring: IdentifiersRequest
  ): Future[DependencyGraph] = {
    DependencyGraphRequest(
      OrgQueue.instance(),
      identifiers.identifiers,
      depth,
      apexOnly,
      ignoring.identifiers
    )
  }

  override def identifierLocation(request: IdentifierRequest): Future[IdentifierLocationResult] = {
    IdentifierLocation(OrgQueue.instance(), request.identifier)
  }

  override def identifierForPath(path: String): Future[IdentifierForPathResult] = {
    IdentifierForPath(OrgQueue.instance(), path)
  }

  override def getDefinition(
    path: String,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Future[Array[LocationLink]] = {
    GetDefinition(OrgQueue.instance(), path, line, offset, content)
  }

  override def getImplementation(
    path: String,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Future[Array[LocationLink]] = {
    GetImplementation(OrgQueue.instance(), path, line, offset, content)
  }

  override def getReferences(
    path: String,
    line: Int,
    offset: Int
  ): Future[Array[TargetLocation]] = {
    GetReferences(OrgQueue.instance(), path, line, offset)
  }

  override def getDependencyBombs(count: Int): Future[Array[BombScore]] = {
    GetDependencyBombs(OrgQueue.instance(), count)
  }

  override def getTestClassNames(
    request: GetTestClassNamesRequest
  ): Future[GetTestClassNamesResult] = {
    GetTestClassNames(OrgQueue.instance(), request.paths)
  }

  override def getDependencyCounts(
    request: GetDependencyCountsRequest
  ): Future[GetDependencyCountsResult] = {
    GetDependencyCounts(OrgQueue.instance(), request)
  }

  override def getAllDependencyCounts(
    request: GetAllDependencyCountsRequest
  ): Future[GetAllDependencyCountsResult] = {
    GetAllDependencyCounts(OrgQueue.instance(), request)
  }

  override def getCompletionItems(
    path: String,
    line: Int,
    offset: Int,
    content: String
  ): Future[Array[CompletionItemLink]] = {
    GetCompletionItems(OrgQueue.instance(), path, line, offset, content)
  }

  override def getTestClassItems(paths: Array[String]): Future[TestClassItemsResult] = {
    GetTestClassItems(OrgQueue.instance(), paths)
  }

  override def getTestClassItemsChanged(paths: Array[String]): Future[TestClassItemsResult] = {
    GetTestClassItemsChanged(OrgQueue.instance(), paths)
  }

  override def getTestMethodItems(paths: Array[String]): Future[TestMethodItemsResult] = {
    GetTestMethodItems(OrgQueue.instance(), paths)
  }

}
