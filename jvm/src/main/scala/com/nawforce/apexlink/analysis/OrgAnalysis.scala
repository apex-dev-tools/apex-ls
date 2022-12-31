/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.api.{LoadAndRefreshAnalysis, NoAnalysis, ServerOps}
import com.nawforce.apexlink.org.OPM.OrgImpl
import com.nawforce.pkgforce.diagnostics.{Diagnostic, DiagnosticCategory, Issue}
import com.nawforce.pkgforce.documents.{ApexNature, MetadataDocument}
import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.apexls.api.{Issue => APIIssue}
import io.github.apexdevtools.apexls.spi.AnalysisProvider

import java.util.ServiceLoader
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class OrgAnalysis(org: OrgImpl) {
  private val analysisProviders = ServiceLoader.load(classOf[AnalysisProvider])

  def afterLoad(): Unit = {
    if (ServerOps.externalAnalysisMode != LoadAndRefreshAnalysis)
      return

    val files = mutable.Set[Path]()
    // TODO: Tidy?
    org.packages.foreach(pkg =>
      pkg.modules.foreach(module => {
        val docs = module.index.get(ApexNature)
        docs.iterator.foreach(md => {
          md._2
            .map(p => Path(p))
            .filter(p => MetadataDocument(p).exists(_.nature == ApexNature))
            .foreach(p => files.add(p))
        })
      })
    )
    runAnalysis(files.toSet)
  }

  def afterRefresh(files: Set[Path]): Unit = {
    if (ServerOps.externalAnalysisMode == NoAnalysis)
      return

    runAnalysis(files)
  }

  private def runAnalysis(files: Set[Path]): Unit = {
    val issueManager = org.issues
    val syntaxGroups = files.groupBy(file => issueManager.hasSyntaxIssues(file))
    // Clear provider issues for files that already have syntax errors to reduce noise
    syntaxGroups
      .getOrElse(true, Set())
      .foreach(path => org.issues.clearProviderIssues(path))

    analysisProviders
      .iterator()
      .asScala
      .foreach(provider => {
        val providerId = provider.getProviderId

        // Collect and replace for other files
        val issuesByFile =
          ArraySeq
            .unsafeWrapArray(
              provider
                .collectIssues(
                  Path(org.path).native,
                  syntaxGroups.getOrElse(false, Set()).map(_.native).toArray
                )
            )
            .groupBy(issue => Path(issue.filePath))

        issuesByFile.foreach(kv =>
          org.issues.replaceProviderIssues(providerId, kv._1, kv._2.map(toIssue(providerId, _)))
        )
      })
  }

  private def toIssue(providerId: String, issue: APIIssue): Issue = {
    // Convert from JVM only APIIssue back to Scala compatible Issue
    new Issue(
      Path(issue.filePath()),
      new Diagnostic(
        DiagnosticCategory(issue.category()),
        new Location(
          issue.fileLocation().startLineNumber(),
          issue.fileLocation().startCharOffset(),
          issue.fileLocation().endLineNumber(),
          issue.fileLocation().endCharOffset()
        ),
        issue.message()
      ),
      providerId
    )
  }
}

object OrgAnalysis {
  def afterLoad(org: OrgImpl): Unit = {
    val analysis = new OrgAnalysis(org)
    analysis.afterLoad()
  }

  def afterRefresh(org: OrgImpl, files: Set[Path]): Unit = {
    val analysis = new OrgAnalysis(org)
    analysis.afterRefresh(files)
  }
}
