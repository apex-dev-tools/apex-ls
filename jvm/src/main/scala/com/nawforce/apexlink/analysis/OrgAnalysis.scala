/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.api.ServerOps
import com.nawforce.apexlink.org.OPM.OrgImpl
import com.nawforce.pkgforce.diagnostics.{Diagnostic, DiagnosticCategory, Issue}
import com.nawforce.pkgforce.documents.ApexNature
import com.nawforce.pkgforce.path.{Location, PathFactory, PathLike}
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
    if (!ServerOps.isExternalAnalysisEnabled)
      return

    val files = mutable.Set[PathLike]()
    org.packages.foreach(pkg =>
      pkg.modules.foreach(module =>
        module.index
          .get(ApexNature)
          .foreach(doc => files.add(doc.path))
      )
    )
    val issueManager = org.issues
    val syntaxGroups = files.groupBy(file => issueManager.hasSyntaxIssues(file))
    // Clear provider issues if file already has syntax errors to reduce noise
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
                  org.path.native,
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
      PathFactory(issue.filePath()),
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
