/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.org.OPM.OrgImpl
import com.nawforce.pkgforce.diagnostics.{Diagnostic, DiagnosticCategory, Issue}
import com.nawforce.pkgforce.documents.ApexNature
import com.nawforce.pkgforce.path.{Location, PathFactory}
import com.nawforce.runtime.platform
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.apexls.spi.AnalysisProvider
import io.github.apexdevtools.apexls.api.{Issue => APIIssue}

import java.nio.file.{Path => JVMPath}
import java.util.ServiceLoader
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class OrgAnalysis(org: OrgImpl) {
  private val analysisProviders = ServiceLoader.load(classOf[AnalysisProvider])

  def afterLoad(): Unit = {
    val files = mutable.Set[JVMPath]()
    org.packages.foreach(pkg =>
      pkg.modules.foreach(module =>
        module.index
          .get(ApexNature)
          .foreach(doc => files.add(doc.path.native.asInstanceOf[JVMPath]))
      )
    )

    analysisProviders
      .iterator()
      .asScala
      .foreach(provider => {
        val providerId = provider.getProviderId
        val issuesByFile =
          ArraySeq
            .unsafeWrapArray(
              provider
                .collectIssues(org.path.native.asInstanceOf[JVMPath], files.toArray)
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
