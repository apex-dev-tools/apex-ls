/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.org

import com.nawforce.apexlink.api.TypeSummary
import com.nawforce.apexlink.deps.ReferencingCollector
import com.nawforce.apexlink.org.OPM.Module
import com.nawforce.apexlink.types.apex.ApexClassDeclaration
import com.nawforce.apexlink.types.core.TypeId
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path

import scala.collection.mutable

/** Test class discovery helper */
trait OrgTestClasses {
  self: OPM.OrgImpl =>

  def getTestClassNames(paths: Array[String]): Array[String] = {
    getTestClassNamesInternal(paths.map(p => Path(p))).toArray
  }

  def getTestClassNamesInternal(paths: Array[PathLike]): Set[String] = {
    // Locate starting typeIds for the passed paths, this includes super classes & interfaces
    val sourceIds = paths
      .flatMap { path =>
        findPackageIdentifierAndSummary(path).toArray.flatMap {
          case (typeId, summary) =>
            findSources(typeId, summary)
        }
      }

    // Locate tests for the sourceIds
    ReferencingCollector
      .testReferences(sourceIds.toSet)
      .filter(_.outerTypeName.isEmpty) // Safety check, we only want outer types here
      .map(_.typeName.toString)
  }

  /** Given a summary type find related targets of interest. Related in this case means finding interfaces and
    * superclasses which may have tests targeting them.
    */
  private def findSources(typeId: TypeId, summary: TypeSummary): Array[TypeId] = {
    val accum = mutable.Set[TypeId]()
    accum.addOne(typeId)
    sourcesForSummary(typeId.module, summary, accum)
    accum.toArray
  }

  /** Retrieve type info from a path */
  private def findPackageIdentifierAndSummary(path: PathLike): Option[(TypeId, TypeSummary)] = {
    packages.view
      .flatMap(pkg => {
        pkg
          .getTypeOfPathInternal(path)
          .flatMap(
            typeId =>
              Option(pkg.getSummaryOfType(typeId.asTypeIdentifier))
                .map(summary => (typeId, summary))
          )
      })
      .headOption
  }

  /** Collect source information from a summary, examines super classes, interfaces and nested classes */
  private def sourcesForSummary(
    module: Module,
    summary: TypeSummary,
    accum: mutable.Set[TypeId]
  ): Unit = {
    sourcesForSuperclass(module, summary, accum)
    sourcesForInterfaces(module, summary, accum)
    summary.nestedTypes.foreach { nested => sourcesForSummary(module, nested, accum) }
  }

  /** Collect source information on interfaces, recursive over super classes & includes interfaces. */
  private def sourcesForSuperclass(
    module: Module,
    summary: TypeSummary,
    accum: mutable.Set[TypeId]
  ): Unit = {
    summary.superClass.foreach { superclass =>
      module.findPackageType(superclass, None) match {
        case Some(td: ApexClassDeclaration) if !accum.contains(td.typeId) =>
          val summary = td.summary
          accum.addOne(td.typeId)
          sourcesForSummary(td.module, summary, accum)
        case _ => ()
      }
    }
  }

  /** Collect source information on interfaces, recursive over interface extends. */
  private def sourcesForInterfaces(
    module: Module,
    summary: TypeSummary,
    accum: mutable.Set[TypeId]
  ): Unit = {
    summary.interfaces.foreach { interface =>
      module.findPackageType(interface, None) match {
        case Some(td: ApexClassDeclaration) if !accum.contains(td.typeId) =>
          // If we find, log & recurse on it
          val summary = td.summary
          accum.addOne(td.typeId)
          sourcesForInterfaces(td.module, summary, accum)
        case _ => ()
      }
    }
  }

  /** Information held on sources */
  private case class SourceInfo(typeId: TypeId, summary: TypeSummary)
}
