/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser.{OutlineParser, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents.{ApexNature, DocumentIndex}
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.pkgforce.pkgs.TriHierarchy
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object IPM extends TriHierarchy {
  type TOrg     = Index
  type TPackage = Package
  type TModule  = Module

  class Index(val path: PathLike) extends TriOrg {

    val issues: IssuesManager = new IssuesManager

    val workspace: Workspace = {
      val wsAndIssues = Workspace.apply(path)
      wsAndIssues.issues.foreach(issues.add)
      wsAndIssues.value.getOrElse(new Workspace(Seq()))
    }

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
          Seq(new Package(this, None, declared, workspace, ArraySeq.empty, createModule, logger))
        else
          Seq.empty
      ArraySeq.unsafeWrapArray((declared ++ unmanaged).toArray)
    }

    def rootModule: Option[Module] = {
      packages.find(_.modules.nonEmpty).map(_.modules.head)
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
      layers
        .foldLeft(ArraySeq[Module]())((acc, layer) => {
          val issuesAndIndex = workspace.indexes(layer)
          logger.logAll(issuesAndIndex.issues)
          val module = mdlFactory(this, issuesAndIndex.value, acc)
          acc :+ module
        })
  }

  class Module(
    override val pkg: Package,
    override val index: DocumentIndex,
    override val dependents: ArraySeq[Module]
  ) extends TriModule {

    private val types = mutable.Map[Name, TypeDeclaration]()

    loadClasses()

    private def loadClasses(): Unit = {
      val namespace = pkg.namespace
      ApexClassLoader
        .loadClasses(index.get(ApexNature), pkg.org.issues)
        .map(
          docAndType => types.put(Name(docAndType._1.typeName(namespace).toString), docAndType._2)
        )
    }

    def findExactTypeId(name: String): Option[TypeDeclaration] = {
      types.get(Name(name))
    }
  }
}
