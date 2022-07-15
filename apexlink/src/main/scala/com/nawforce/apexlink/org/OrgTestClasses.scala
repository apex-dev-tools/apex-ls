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
import com.nawforce.apexlink.deps.ReferencingCollector.NodeInfo
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.types.apex.ApexDeclaration
import com.nawforce.apexlink.types.core.{TypeDeclaration, TypeId}
import com.nawforce.pkgforce.names.TypeName
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
    val startingIds = paths.flatMap { path => findPackageIdentifier(path) }

    // Convert to source set by searching for related types
    val accum = mutable.Set[NodeInfo]()
    startingIds.foreach(typeId => {
      typeId.module
        .findPackageType(typeId.typeName, None)
        .collect { case td: ApexDeclaration => td }
        .foreach(
          td => (td +: td.nestedTypes).foreach(td => sourcesForType(td, primary = true, accum))
        )
    })

    // Locate tests for the sourceIds
    ReferencingCollector
      .testReferences(accum.toSet)
      .filter(_.outerTypeName.isEmpty) // Safety check, we only want outer types here
      .map(_.typeName.toString)
  }

  /** Retrieve type info from a path */
  private def findPackageIdentifier(path: PathLike): Option[TypeId] = {
    packages.view
      .flatMap(pkg => pkg.getTypeOfPathInternal(path))
      .headOption
  }

  /** Collect source information from a summary, examines super classes & interfaces */
  private def sourcesForType(
    td: ApexDeclaration,
    primary: Boolean,
    accum: mutable.Set[NodeInfo]
  ): Unit = {
    accum.add(NodeInfo(td, primary))
    sourcesForSuperclass(td, accum)
    sourcesForInterfaces(td, accum)
  }

  /** Collect source information on interfaces, recursive over super classes & includes interfaces. */
  private def sourcesForSuperclass(td: ApexDeclaration, accum: mutable.Set[NodeInfo]): Unit = {
    td.superClass.foreach { superclass =>
      toApexDeclaration(superclass, td).foreach(
        superClassTd => sourcesForType(superClassTd, primary = false, accum)
      )
    }
  }

  /** Collect source information on interfaces, recursive over interface extends. */
  private def sourcesForInterfaces(td: ApexDeclaration, accum: mutable.Set[NodeInfo]): Unit = {
    td.interfaces.foreach { interface =>
      toApexDeclaration(interface, td).foreach(interfaceTd => {
        accum.addOne(NodeInfo(interfaceTd, primary = false))
        sourcesForInterfaces(interfaceTd, accum)
      })
    }
  }

  private def toApexDeclaration(
    typeName: TypeName,
    from: TypeDeclaration
  ): Option[ApexDeclaration] = {
    TypeResolver(typeName, from).toOption.collect { case td: ApexDeclaration => td }
  }

  /** Information held on sources */
  private case class SourceInfo(typeId: TypeId, summary: TypeSummary)
}
