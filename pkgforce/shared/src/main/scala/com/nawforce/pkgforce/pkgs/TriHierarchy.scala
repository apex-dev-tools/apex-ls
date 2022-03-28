/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.pkgs

import com.nawforce.pkgforce.documents.DocumentIndex
import com.nawforce.pkgforce.names.TypeNameFuncs.TypeNameFuncs
import com.nawforce.pkgforce.names.{EncodedName, Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}

import scala.collection.immutable.ArraySeq

abstract class TriHierarchy {
  type TOrg <: TriOrg
  type TPackage <: TriPackage
  type TModule <: TriModule

  trait TriOrg {
    self: TOrg =>

    /** Packages in org in deploy order, the last entry is the unmanaged package identified by namespace = None */
    val packages: ArraySeq[TPackage]

    /** Packages in org by namespace */
    lazy val packagesByNamespace: Map[Option[Name], TPackage] =
      packages.map(pkg => (pkg.namespace, pkg)).toMap

    /** All orgs have an unmanaged package, it has to be the last entry in 'packages'. */
    lazy val unmanaged: TPackage = packages.last
  }

  trait TriPackage {
    self: TPackage =>

    /** The parent org that this package belongs to */
    val org: TOrg

    /** Namespace for the package, None=unmanaged */
    val namespace: Option[Name]

    /** The packages this package depends on */
    val basePackages: ArraySeq[TPackage]

    /** The modules in this package in deploy order */
    val modules: ArraySeq[TModule]

    /** Package modules in reverse deploy order. */
    lazy val orderedModules: ArraySeq[TModule] = modules.reverse

    /** Is this a ghost package, aka it has no modules. */
    lazy val isGhosted: Boolean = modules.isEmpty

    override def toString: String = s"Package(${namespace.map(_.toString).getOrElse("")})"

    /* Check if a type is ghosted in this package */
    def isGhostedType(typeName: TypeName): Boolean = {
      if (typeName.outer.contains(TypeName.Schema)) {
        val encName = EncodedName(typeName.name)
        basePackages.filter(_.isGhosted).exists(_.namespace == encName.namespace)
      } else {
        basePackages.filter(_.isGhosted).exists(_.namespace.contains(typeName.outerName)) ||
        typeName.params.exists(isGhostedType)
      }
    }

    /** Check if a field name is ghosted in this package. */
    def isGhostedFieldName(name: Name): Boolean = {
      EncodedName(name).namespace match {
        case None     => false
        case Some(ns) => basePackages.filter(_.isGhosted).exists(_.namespace.contains(ns))
      }
    }

  }

  trait TriModule {
    self: TModule =>

    /** The parent package that this module belongs to */
    val pkg: TPackage

    /** The modules that this module depends on deploy order */
    val dependents: ArraySeq[TModule]

    /** The module (& owning package namespace) */
    lazy val namespace: Option[Name] = pkg.namespace

    /** The package the parent package depends on in reverse deploy order */
    lazy val basePackages: ArraySeq[TPackage] = pkg.basePackages.reverse

    /** The modules that this module depends on in reverse deploy order */
    lazy val baseModules: ArraySeq[TModule] = dependents.reverse

    /** Test if a file is visible to this module, i.e. in scope & not ignored */
    def isVisibleFile(path: PathLike): Boolean

    /* Transitive Modules (dependent modules for this modules & its dependents) */
    def transitiveModules: Set[TModule] = {
      namespace
        .map(_ => dependents.toSet ++ dependents.flatMap(_.transitiveModules))
        .getOrElse(baseModules.toSet)
    }

    /* Check if a type name is ghosted in this module */
    def isGhostedType(typeName: TypeName): Boolean = pkg.isGhostedType(typeName)

    /* Check if a field name is ghosted in this module */
    def isGhostedFieldName(name: Name): Boolean = pkg.isGhostedFieldName(name)
  }
}
