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
package com.nawforce.pkgforce.workspace

import com.nawforce.pkgforce.diagnostics.{CatchingLogger, IssuesManager}
import com.nawforce.pkgforce.documents.{DocumentIndex, MetadataDocument}
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.pkgforce.sfdx.{ForceIgnoreVersion, SFDXProject}
import com.nawforce.pkgforce.stream.{PackageEvent, PackageStream}

/** Contains any config option that can be used by the Org
  */
case class ProjectConfig(maxDependencyCount: Option[Int])

/** Metadata workspace, maintains information on available metadata within a project/package.
  *
  * Duplicate detection is based on the relevant MetadataDocumentType(s) being able to generate an
  * accurate TypeName for the metadata. Where multiple metadata items may contribute to a type, e.g.
  * labels, make sure that duplicatesAllowed is set which will bypass the duplicate detection.
  * Duplicates are reported as errors and then ignored.
  *
  * During an upsert/deletion of new types the index will also need to be updated so that it
  * maintains an accurate view of the metadata files being used.
  */
case class Workspace(
  logger: IssuesManager,
  layers: Seq[NamespaceLayer],
  projectConfig: Option[ProjectConfig] = None,
  forceIgnoreVersion: ForceIgnoreVersion = ForceIgnoreVersion.default
) {

  // Document indexes for each layer of actual metadata
  val indexes: Map[ModuleLayer, DocumentIndex] =
    layers.foldLeft(Map[ModuleLayer, DocumentIndex]())((acc, layer) =>
      acc ++ layer.indexes(logger, forceIgnoreVersion)
    )

  def get(typeName: TypeName): List[MetadataDocument] = {
    val indexes = deployOrderedIndexes.toSeq.reverse
    indexes
      .find(_.get(typeName).nonEmpty)
      .map(index => {
        index.get(typeName)
      })
      .getOrElse(List())
  }

  def events: Iterator[PackageEvent] = {
    deployOrderedIndexes.flatMap(index => PackageStream.eventStream(index))
  }

  private def deployOrderedIndexes: Iterator[DocumentIndex] = {
    layers.iterator.flatMap(layer => layer.layers).flatMap(indexes.get)
  }
}

object Workspace {
  /* We need to pass an IssueManager here as this may generate a lot of diagnostics */
  def apply(project: Option[SFDXProject], issueManager: IssuesManager): Option[Workspace] = {
    val layers = project.map(_.layers(issueManager)).getOrElse(Seq())
    if (issueManager.hasErrors) {
      None
    } else {
      project.map { proj =>
        new Workspace(
          issueManager,
          layers,
          Some(ProjectConfig(proj.apexConfig.maxDependencyCount)),
          proj.forceIgnoreVersion
        )
      }
    }
  }

  def apply(path: PathLike): (Option[Workspace], IssuesManager) = {
    val logger = new CatchingLogger()

    validateWorkspacePath(path, logger) match {
      case Some(error) => error
      case None        => createWorkspaceFromValidPath(path, logger)
    }
  }

  private def validateWorkspacePath(
    path: PathLike,
    logger: CatchingLogger
  ): Option[(Option[Workspace], IssuesManager)] = {
    if (!path.exists || !path.isDirectory) {
      logger.logError(path, Location.empty, s"No directory at $path")
      val issueManager = new IssuesManager(None)
      logger.issues.foreach(issueManager.add)
      Some((None, issueManager))
    } else {
      None
    }
  }

  private def createWorkspaceFromValidPath(
    path: PathLike,
    logger: CatchingLogger
  ): (Option[Workspace], IssuesManager) = {
    val project            = loadSFDXProject(path, logger)
    val externalPathFilter = createExternalPathFilter(project)
    val issueManager       = new IssuesManager(externalPathFilter)
    logger.issues.foreach(issueManager.add)
    (Workspace(project, issueManager), issueManager)
  }

  private def loadSFDXProject(path: PathLike, logger: CatchingLogger): Option[SFDXProject] = {
    if (path.join("sfdx-project.json").exists) {
      // SFDXProject.apply already logs detailed errors for all failure cases
      // (parsing errors, read failures, etc.) so no additional logging is needed here
      SFDXProject(path, logger)
    } else {
      logger.logError(
        path,
        Location.empty,
        s"No sfdx-project.json found at $path. Only SFDX format projects are supported."
      )
      None
    }
  }

  private def createExternalPathFilter(
    project: Option[SFDXProject]
  ): Option[PathLike => Boolean] = {
    // External metadata filtering was removed - return None
    None
  }
}
