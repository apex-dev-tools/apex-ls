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

import com.nawforce.pkgforce.diagnostics.{IssueLogger, IssuesManager}
import com.nawforce.pkgforce.documents.DocumentIndex
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.sfdx.ForceIgnoreVersion

/** Project metadata is modeled as an ordered sequence of namespace layers which contain an ordered
  * sequence of module layers. The layers should be ordered by deploy order, this means external
  * layers defined via $.plugins.dependencies will appear first before the namespace layer for the
  * target project. Namespaces must be unique, which means you can not use dependencies to reference
  * a second sfdx-project.json using the same namespace, all 2GP modules must be contained in a
  * single sfdx-project.json.
  */
sealed trait Layer

/** A namespace layer provides the namespace for some list of module layers. The list will be empty
  * for 'ghosted' packages where only knowledge of a namespace is provided. In a 1GP package each
  * layer will depend on its predecessor, with 2GP the layer dependencies are declared.
  */
case class NamespaceLayer(namespace: Option[Name], isGulped: Boolean, layers: Seq[ModuleLayer])
    extends Layer {
  def indexes(
    logger: IssuesManager,
    forceIgnoreVersion: ForceIgnoreVersion = ForceIgnoreVersion.default
  ): Map[ModuleLayer, DocumentIndex] =
    layers.foldLeft(Map[ModuleLayer, DocumentIndex]())((acc, layer) =>
      acc + (layer -> layer.index(logger, namespace, isGulped, forceIgnoreVersion))
    )
}

/** A package layer encompasses a packageDirectory from sfdx-project.json or a 1GP style MDAPI
  * metadata directory. The dependencies should only reference layers defined within the same
  * NamespaceLayer.
  */
case class ModuleLayer(
  projectPath: PathLike,
  relativePath: String,
  dependencies: Seq[ModuleLayer]
) {

  val path: PathLike = projectPath.join(relativePath)

  def pathRelativeTo(root: PathLike): String = {
    path.toString.substring(root.toString.length)
  }

  def index(
    logger: IssuesManager,
    namespace: Option[Name],
    isGulped: Boolean,
    forceIgnoreVersion: ForceIgnoreVersion = ForceIgnoreVersion.default
  ): DocumentIndex = {
    DocumentIndex(logger, namespace, isGulped, projectPath, path, forceIgnoreVersion)
  }
}
