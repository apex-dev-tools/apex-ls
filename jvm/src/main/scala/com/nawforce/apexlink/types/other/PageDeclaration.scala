/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.types.other

import com.nawforce.apexlink.finding.TypeResolver.TypeCache
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.types.core._
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.modifiers.{
  GLOBAL_MODIFIER,
  Modifier,
  PRIVATE_MODIFIER,
  STATIC_MODIFIER
}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLocation
import com.nawforce.pkgforce.stream.{PackageStream, PageEvent}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/** A individual Page being represented as a static field. */
case class Page(module: OPM.Module, location: PathLocation, name: Name, vfContainer: VFContainer)
    extends FieldDeclaration {
  override lazy val modifiers: ArraySeq[Modifier] = ArraySeq(STATIC_MODIFIER, GLOBAL_MODIFIER)
  override lazy val typeName: TypeName            = TypeNames.PageReference
  override lazy val readAccess: Modifier          = GLOBAL_MODIFIER
  override lazy val writeAccess: Modifier         = PRIVATE_MODIFIER
  override val idTarget: Option[TypeName]         = None

  private var depends: Option[Set[Dependent]] = None

  override def dependencies(): Iterable[Dependent] =
    depends.getOrElse(Array[Dependent]())

  def validate(): Unit = {
    depends = Some(vfContainer.validate())
    propagateDependencies()
  }
}

object Page {
  def apply(module: OPM.Module, event: PageEvent): Seq[Page] = {
    val location  = event.sourceInfo.location
    val document  = MetadataDocument(location.path)
    val container = new VFContainer(module, event)
    Seq(new Page(module, location, document.get.name, container)) ++
      (if (module.namespace.nonEmpty)
         Seq(
           new Page(
             module,
             location,
             Name(s"${module.namespace.get.value}__${document.get.name}"),
             container
           )
         )
       else
         Seq.empty)
  }
}

/** Page 'namespace' implementation. Provides access to pages in the package as well as pages that
  * are accessible in base packages via the `namespace__name` format.
  */
final case class PageDeclaration(
  sources: ArraySeq[SourceInfo],
  override val module: OPM.Module,
  pages: ArraySeq[Page]
) extends BasicTypeDeclaration(
      pages.map(page => page.location.path).distinct,
      module,
      TypeNames.Page
    )
    with DependentType
    with Dependent {

  // Propagate dependencies to base packages
  module.baseModules.foreach(_.pages.addTypeDependencyHolder(typeId))

  val sourceHash: Int = MurmurHash3.unorderedHash(sources.map(_.hash), 0)

  override lazy val isComplete: Boolean           = !module.pkg.hasGhosted
  override val fields: ArraySeq[FieldDeclaration] = pages

  /** Create new pages from merging those in the provided stream */
  def merge(stream: PackageStream): PageDeclaration = {
    merge(stream.pages)
  }

  def merge(pageEvents: ArraySeq[PageEvent]): PageDeclaration = {
    val newPages   = pages ++ pageEvents.flatMap(pe => Page(module, pe))
    val sourceInfo = pageEvents.map(_.sourceInfo).distinct
    new PageDeclaration(sourceInfo, module, newPages)
  }

  override protected def validate(): Unit = {
    // We may create multiple Pages for each .page file to handle namespaces
    // We only want to validate one of them to avoid duplicate diagnostics
    val uniquePages = pages.map(page => (page.location.path, page)).toMap.values
    uniquePages.foreach(_.validate())
    propagateOuterDependencies(new TypeCache())
  }

  override def gatherDependencies(
    dependsOn: mutable.Set[TypeId],
    apexOnly: Boolean,
    outerTypesOnly: Boolean,
    typeCache: TypeCache
  ): Unit = {
    val dependents = mutable.Set[Dependent]()
    pages.foreach(page => page.dependencies().foreach(dependents.add))
    DependentType.dependentsToTypeIds(module, dependents, apexOnly, outerTypesOnly, dependsOn)
    if (!apexOnly)
      module.baseModules.foreach(bp => dependsOn.add(bp.pages.typeId))
  }
}

object PageDeclaration {
  def apply(module: OPM.Module): PageDeclaration = {
    new PageDeclaration(ArraySeq(), module, collectBasePages(module))
  }

  private def collectBasePages(module: OPM.Module): ArraySeq[Page] = {
    module.nextModule
      .map(next => {
        if (next.namespace != module.namespace) {
          next.pages.pages.filter(page => page.name.value.contains("__"))
        } else {
          next.pages.pages
        }
      })
      .getOrElse(ArraySeq())
  }
}
