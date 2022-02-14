/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser.{OutlineParser, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents.MetadataDocument
import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.parsers.SourceData

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

object ApexClassLoader {
  private val executors                             = Executors.newFixedThreadPool(2)
  private implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(executors)

  def shutdown(): Unit = {
    executors.shutdown()
  }

  def loadClasses(
    documents: Iterator[MetadataDocument],
    logger: IssueLogger
  ): Array[(MetadataDocument, TypeDeclaration)] = {

    val loadFutures = Future.traverse(documents)(document => {

      val source = document.path.readSourceData()
      source match {
        case Left(err) =>
          Future.successful(
            Left(Some(new Issue(document.path, Diagnostic(ERROR_CATEGORY, Location.empty, err))))
          )
        case Right(source) =>
          parseSource(document, source)
      }
    })

    val results = Await.result(loadFutures, Duration.Inf)

    results.toArray.flatMap {
      case Right(td) =>
        Some(td)
      case Left(optIssue) =>
        optIssue.foreach(logger.log)
        None
    }
  }

  private def parseSource(
    document: MetadataDocument,
    source: SourceData
  ): Future[Either[Option[Issue], (MetadataDocument, TypeDeclaration)]] = {
    Future {
      val (_, reason, td) = OutlineParser.parse(document.path.toString, source.asString)
      td
        .map(td => Right(document, td))
        .getOrElse(
          Left(
            Some(new Issue(document.path, Diagnostic(ERROR_CATEGORY, Location.empty, reason.get)))
          )
        )
    }
  }
}
