/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.pkgs

import com.nawforce.pkgforce.diagnostics.IssueLogger
import com.nawforce.pkgforce.documents.DocumentIndex
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}

import scala.reflect.ClassTag

class PackageBase[+Org, +Module <: ModuleBase[Org] : ClassTag]
(val org: Org, val namespace: Option[Name], val basePackages: Seq[PackageBase[Org, Module]], workspace: Workspace,
 layers: Seq[ModuleLayer], mdlFactory: (PackageBase[Org, Module], DocumentIndex, Seq[Module]) => Module, logger: IssueLogger) {

  val modules: Seq[Module] =
    layers.foldLeft(Map[ModuleLayer, Module]())((acc, layer) => {
      val issuesAndIndex = workspace.indexes(layer)
      logger.logAll(issuesAndIndex.issues)
      val module = mdlFactory(this, issuesAndIndex.value, layers.flatMap(acc.get))
      acc + (layer -> module)
    }).values.toSeq
}

object PackageBase {

  def construct[Org, Package <: PackageBase[Org, Module], Module <: ModuleBase[Org]](
                                                                                      workspace: Workspace, org: Org,
                                                                                      pkgFactory: (Org, Option[Name], Seq[Package], Workspace, Seq[ModuleLayer], (Package, DocumentIndex, Seq[Module]) => Module, IssueLogger) => Package,
                                                                                      mdlFactory: (Package, DocumentIndex, Seq[Module]) => Module,
                                                                                      logger: IssueLogger): Seq[Package] = {

    // Fold over layers to create packages - with any package(namespace) dependencies linked to each package
    // The workspace layers form a deploy ordering, so each is dependent on all previously created
    val packages =
    workspace.layers.foldLeft(Seq[Package]())((acc, pkgLayer) => {
      acc :+ pkgFactory(org, pkgLayer.namespace, acc, workspace, pkgLayer.layers, mdlFactory, logger)
    })

    // If no unmanaged, create it
    val unmanaged =
      if (packages.isEmpty || packages.lastOption.exists(_.namespace.nonEmpty))
        Seq(pkgFactory(org, None, packages, workspace, Seq.empty, mdlFactory, logger))
      else
        Seq.empty

    packages ++ unmanaged
  }
}
