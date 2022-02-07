/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.workspace

import com.nawforce.pkgforce.diagnostics.{CatchingLogger, IssueLogger}
import com.nawforce.pkgforce.documents.DocumentIndex
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.pkgs.TriHierarchy

import scala.collection.immutable.ArraySeq

object IPM extends TriHierarchy {
  type TOrg = Index
  type TPackage = Package
  type TModule = Module

  class Index(val path: PathLike, initWorkspace: Option[Workspace]) extends TriOrg {
    val workspace: Workspace = initWorkspace.getOrElse(new Workspace(Seq()))

    override val packages: ArraySeq[Package] = {

      def createModule(
                        pkg: Package,
                        index: DocumentIndex,
                        dependencies: ArraySeq[Module]
                      ): Module = {
        new Module(pkg, index, dependencies)
      }

      val logger = new CatchingLogger

      // Fold over layers to create packages - with any package(namespace) dependencies linked to each package
      // The workspace layers form a deploy ordering, so each is dependent on all previously created
      val declared =
      workspace.layers.foldLeft(ArraySeq[Package]())((acc, pkgLayer) => {
        acc :+ new Package(
          this,
          pkgLayer.namespace,
          acc,
          workspace,
          ArraySeq.unsafeWrapArray(pkgLayer.layers.toArray),
          createModule,
          logger
        )
      })

      // If no unmanaged, create it
      val unmanaged =
        if (declared.isEmpty || declared.lastOption.exists(_.namespace.nonEmpty))
          Seq(
            new Package(
              this,
              None,
              declared,
              workspace,
              ArraySeq.empty,
              createModule,
              logger
            )
          )
        else
          Seq.empty
      ArraySeq.unsafeWrapArray((declared ++ unmanaged).toArray)
    }

  }

  class Package(
                 override val org: Index,
                 override val namespace: Option[Name],
                 override val basePackages: ArraySeq[Package],
                 workspace: Workspace,
                 layers: ArraySeq[ModuleLayer],
                 mdlFactory: (Package, DocumentIndex, ArraySeq[Module]) => Module,
                 logger: IssueLogger
               ) extends TriPackage {

    val modules: ArraySeq[Module] =
      ArraySeq.unsafeWrapArray(
        layers
          .foldLeft(Map[ModuleLayer, Module]())((acc, layer) => {
            val issuesAndIndex = workspace.indexes(layer)
            logger.logAll(issuesAndIndex.issues)
            val module = mdlFactory(this, issuesAndIndex.value, layers.flatMap(acc.get))
            acc + (layer -> module)
          })
          .values
          .toArray
      )
  }

  class Module(
                override val pkg: Package,
                override val index: DocumentIndex,
                override val dependents: ArraySeq[Module]
              ) extends TriModule {
  }
}
