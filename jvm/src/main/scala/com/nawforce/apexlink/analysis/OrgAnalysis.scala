/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.api.{LoadAndRefreshAnalysis, NoAnalysis, ServerOps}
import com.nawforce.apexlink.org.OPM.OrgImpl
import com.nawforce.pkgforce.diagnostics.{IssueAnalysis, IssueProviderOps, _}
import com.nawforce.pkgforce.documents.ApexNature
import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.api.{Issue => APIIssue}
import io.github.apexdevtools.spi.AnalysisProvider

import java.util.ServiceLoader
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try, Failure}

/** Service to invoke custom analysis providers that can augment normal diagnostics.
  * @param org run analysis against for this org
  */
class OrgAnalysis(org: OrgImpl) {
  private val analysisProviders = ServiceLoader
    .load(classOf[AnalysisProvider])
    .iterator()
    .asScala
    .flatMap(provider => configureProvider(provider))
    .toList

  /** Apply custom parameters to a provider.
    * @param provider apply to this provider
    * @return the provider or None if an error occurred
    */
  private def configureProvider(provider: AnalysisProvider): Option[AnalysisProvider] = {
    Option.when(
      ServerOps.getExternalAnalysis.params
        .getOrElse(provider.getProviderId, Nil)
        .forall(param => {
          Try(provider.setConfiguration(param._1, param._2.asJava)) match {
            case Success(_) => true
            case Failure(ex) =>
              LoggerOps.info(
                s"Analysis provider '${provider.getProviderId} threw when setting parameter ${param._1}",
                ex
              )
              false
          }
        })
    ) { provider }
  }

  /** Invoke the providers after the org has been loaded.
    * Passed all Apex classes for analysis.
    */
  def afterLoad(): Unit = {
    if (ServerOps.getExternalAnalysis.mode != LoadAndRefreshAnalysis)
      return

    val workspaceProviders = analysisProviders.filter(_.isConfigured(Path(org.path).native))
    if (workspaceProviders.isEmpty)
      return

    // Collect Apex class files over all modules
    val files  = mutable.Set[Path]()
    var module = org.packages.headOption.flatMap(_.firstModule)
    while (module.nonEmpty) {
      module.get.index
        .getControllingDocuments(ApexNature)
        .map(_.path)
        .collect { case p: Path => p }
        .foreach(files.add)
      module = module.get.nextModule
    }
    runAnalysis(workspaceProviders, files.toSet)
  }

  /** Invoke the providers after some files have been changed.
    * @param paths the files (assumed to be Apex classes) that changed
    */
  def afterRefresh(paths: Set[Path]): Unit = {
    if (ServerOps.getExternalAnalysis.mode == NoAnalysis)
      return

    val workspaceProviders = analysisProviders.filter(_.isConfigured(Path(org.path).native))
    if (workspaceProviders.isEmpty)
      return

    runAnalysis(workspaceProviders, paths)
  }

  private def runAnalysis(providers: List[AnalysisProvider], files: Set[Path]): Unit = {
    val issueManager = org.issues
    val syntaxGroups = files.groupBy(file => IssueAnalysis.hasSyntaxIssues(issueManager, file))
    // Clear provider issues for files that already have syntax errors to reduce noise
    syntaxGroups
      .getOrElse(true, Set())
      .foreach(path => IssueProviderOps.clearProviderIssues(org.issues, path))

    providers
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
          IssueProviderOps.replaceProviderIssues(
            org.issues,
            providerId,
            kv._1,
            kv._2.map(toIssue(providerId, _))
          )
        )
      })
  }

  private def toIssue(providerId: String, issue: APIIssue): Issue = {
    new Issue(
      Path(issue.filePath()),
      new Diagnostic(
        if (issue.isError) ERROR_CATEGORY else WARNING_CATEGORY,
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
