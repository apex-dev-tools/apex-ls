package com.nawforce.pkgforce.pkgs

import com.nawforce.pkgforce.documents.DocumentIndex

abstract class TriHierarchy {
  type TOrg <: TriOrg
  type TPackage <: TriPackage
  type TModule <: TriModule

  trait TriOrg {
    self: TOrg =>
    def packages: Seq[TPackage]
  }

  trait TriPackage {
    self: TPackage =>
    def org: TOrg

    def basePackages: Seq[TPackage]
    def modules: Seq[TModule]

    /** Package modules in reverse deploy order. */
    lazy val orderedModules: Seq[TModule] = modules.reverse

    /** Is this a ghost package, aka it has no modules. */
    lazy val isGhosted: Boolean =  modules.isEmpty
  }

  trait TriModule {
    self: TModule =>
    def pkg: TPackage

    def index: DocumentIndex

    def dependents: Seq[TModule]
  }

}
