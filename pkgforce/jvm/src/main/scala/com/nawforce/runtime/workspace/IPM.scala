/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser.{
  ClassTypeDeclaration,
  EnumTypeDeclaration,
  InterfaceTypeDeclaration,
  TypeDeclaration,
  TypeDeclarationFactory
}
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents.{ApexNature, DocumentIndex}
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.pkgs.TriHierarchy
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}

import java.util.concurrent.{ExecutorService, Executors}
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

      val loadingPool = Executors.newFixedThreadPool(2)

      def createModule(
        pkg: Package,
        index: DocumentIndex,
        dependencies: ArraySeq[Module]
      ): Module = {
        new Module(pkg, index, dependencies, loadingPool)
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

      loadingPool.shutdown()

      // If no unmanaged, create it
      val unmanaged =
        if (declared.isEmpty || declared.lastOption.exists(_.namespace.nonEmpty))
          Seq(new Package(this, None, declared, workspace, ArraySeq.empty, createModule, logger))
        else
          Seq.empty
      ArraySeq.unsafeWrapArray((declared ++ unmanaged).toArray)
    }

    def rootModule: Option[Module] = {
      packages.find(_.modules.nonEmpty).map(_.modules.last)
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

    def namespaceAsString: String = namespace.map(_.value).getOrElse("")
  }

  class Module(
    override val pkg: Package,
    override val index: DocumentIndex,
    override val dependents: ArraySeq[Module],
    loadingPool: ExecutorService
  ) extends TriModule {

    private val lowerNames = mutable.TreeSet[String]()
    private val types      = mutable.Map[Name, TypeDeclaration]()

    loadClasses()

    private def loadClasses(): Unit = {
      val namespace = pkg.namespace
      new ApexClassLoader(loadingPool, ModuleClassFactory)
        .loadClasses(index.get(ApexNature), pkg.org.issues)
        .foreach { docAndType =>
          markModule(docAndType._2)
          insertClass(docAndType._1.typeName(namespace).toString, docAndType._2)
        }
    }

    private def markModule(decl: TypeDeclaration): Unit = {
      decl match {
        case scoped: ModuleScoped =>
          decl.innerTypes.foreach(markModule)
          scoped.module = Some(this)
        case _ => ()
      }
    }

    private def insertClass(name: String, decl: TypeDeclaration): Unit = {
      lowerNames.add(name.toLowerCase)
      types.put(Name(name), decl)

      decl match {
        case outer: ClassTypeDeclaration =>
          outer.innerTypes.foreach(inner => {
            val innerName = s"$name.${inner.id}"
            lowerNames.add(name.toLowerCase)
            types.put(Name(innerName), inner)
          })
        case _ => ()
      }
    }

    def findExactTypeId(name: String): Option[TypeDeclaration] = {
      types
        .get(Name(name))
        .orElse(baseModules.headOption.flatMap(_.findExactTypeId(name)))
        .orElse(
          basePackages.headOption
            .flatMap(_.orderedModules.headOption.flatMap(_.findExactTypeId(name)))
        )
    }

    def fuzzyFindTypeId(name: String): Option[TypeDeclaration] = {
      if (name != null && name.nonEmpty) {
        val lower = name.toLowerCase
        lowerNames
          .rangeFrom(lower)
          .take(1)
          .find(_.startsWith(lower))
          .flatMap(name => types.get(Name(name)))
          .orElse(baseModules.headOption.flatMap(_.fuzzyFindTypeId(name)))
          .orElse(
            basePackages.headOption
              .flatMap(_.orderedModules.headOption.flatMap(_.fuzzyFindTypeId(name)))
          )
      } else {
        None
      }
    }

    def fuzzyFindTypeIds(name: String): Seq[TypeDeclaration] = {
      if (name != null && name.nonEmpty) {
        val accum = new mutable.HashMap[Name, TypeDeclaration]()
        accumFuzzyFindTypeIds(name, accum)
        accum.keys.toSeq.sortBy(_.value.length).flatMap(accum.get)
      } else {
        Seq.empty
      }
    }

    private def accumFuzzyFindTypeIds(
      name: String,
      accum: mutable.Map[Name, TypeDeclaration]
    ): Unit = {
      // Accumulate lower layers first
      if (baseModules.isEmpty) {
        basePackages.headOption.foreach(
          _.orderedModules.headOption.foreach(_.accumFuzzyFindTypeIds(name, accum))
        )
      } else {
        baseModules.headOption.foreach(_.accumFuzzyFindTypeIds(name, accum))
      }

      // Add/Overwrite with this module
      val lower = name.toLowerCase
      lowerNames
        .rangeFrom(lower)
        .iterator
        .filter(_.startsWith(lower))
        .foreach(typeName => {
          val name = Name(typeName)
          types.get(name).foreach(accum.put(name, _))
        })
    }

    def findTypeIdsByNamespace(namespacePrefix: String): Seq[TypeDeclaration] = {
      if (namespacePrefix != null) {
        val accum = new mutable.HashMap[Name, TypeDeclaration]()
        accumFindTypeIdsByNamespace(namespacePrefix, accum)
        accum.keys.toSeq.sortBy(_.value.length).flatMap(accum.get)
      } else {
        Seq.empty
      }
    }

    private def accumFindTypeIdsByNamespace(
      namespacePrefix: String,
      accum: mutable.Map[Name, TypeDeclaration]
    ): Unit = {
      basePackages.headOption.foreach(
        _.orderedModules.headOption.foreach(_.accumFindTypeIdsByNamespace(namespacePrefix, accum))
      )

      val namespaceMatches =
        if (namespacePrefix.isEmpty)
          pkg.namespace.isEmpty
        else
          pkg.namespace.exists(ns => ns.value.toLowerCase.startsWith(namespacePrefix.toLowerCase))

      if (namespaceMatches) {
        baseModules.headOption.foreach(_.accumFindTypeIdsByNamespace(namespacePrefix, accum))
        accum.addAll(types)
      }
    }

    def getTypesByPath(path: String): Seq[TypeDeclaration] = {
      findTypesByPathPredicate(t => t == path)
    }

    def findTypesByPath(path: String): Seq[TypeDeclaration] = {
      findTypesByPathPredicate(t => t.equalsIgnoreCase(path))
    }

    def fuzzyFindTypesByPath(path: String): Seq[TypeDeclaration] = {
      findTypesByPathPredicate(t => t.toLowerCase.startsWith(path.toLowerCase))
    }

    private def findTypesByPathPredicate(predicate: String => Boolean): Seq[TypeDeclaration] = {
      var typesForPath = types.values.filter(t => t.paths.exists(p => predicate(p)))
      if (typesForPath.nonEmpty) return typesForPath.toSeq

      typesForPath = baseModules.headOption
        .map(_.findTypesByPathPredicate(predicate))
        .getOrElse(Seq.empty)
      if (typesForPath.nonEmpty) return typesForPath.toSeq

      typesForPath = basePackages.headOption
        .flatMap(_.orderedModules.headOption.map(_.findTypesByPathPredicate(predicate)))
        .getOrElse(Seq.empty)
      typesForPath.toSeq
    }
  }
}
