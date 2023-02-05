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
import com.nawforce.apexlink.rpc.{ClassTestItem, MethodTestItem, TargetLocation}
import com.nawforce.apexlink.types.apex.{ApexDeclaration, ApexMethodLike}
import com.nawforce.apexlink.types.core.{TypeDeclaration, TypeId}
import com.nawforce.pkgforce.modifiers.{ISTEST_ANNOTATION, Modifier, TEST_METHOD_MODIFIER}
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Test class discovery helper */
trait OrgTestClasses {
  self: OPM.OrgImpl =>

  def getTestClassNames(paths: Array[String]): Array[String] = {
    getTestClassNamesInternal(paths.map(p => Path(p))).map(_._1).toArray
  }

  def getTestClassNamesInternal(paths: Array[PathLike]): Set[(String, Array[String])] = {
    findTestClassReferences(paths).map(_.asTypeNameStrings())
  }

  def getTestClassItems(paths: Array[String]): Array[ClassTestItem] = {
    findTestClasses(paths).map(cls => {
      ClassTestItem(cls.name.toString, TargetLocation(cls.location.path.toString, cls.idLocation))
    })
  }

  def getTestClassItemsChanged(paths: Array[String]): Array[ClassTestItem] = {
    findTestClassReferences(paths.map(p => Path(p)))
      .map(info => {
        val cls = info.testClass
        ClassTestItem(cls.name.toString, TargetLocation(cls.location.path.toString, cls.idLocation))
      })
      .toArray
  }

  def getTestMethodItems(paths: Array[String]): Array[MethodTestItem] = {
    findTestClasses(paths).flatMap(cls => {
      cls.methods
        .collect({ case m: ApexMethodLike => m })
        .filter(m => hasTestModifier(m.modifiers))
        .map(m => {
          MethodTestItem(
            m.name.toString,
            cls.name.toString,
            TargetLocation(m.location.path.toString, m.idLocation)
          )
        })
    })
  }

  private def findTestClasses(paths: Array[String]): Array[ApexDeclaration] = {
    if (paths.isEmpty) {
      getAllTestClasses()
    } else {
      findTestClassesFromPaths(paths)
    }
  }

  private def getAllTestClasses(): Array[ApexDeclaration] = {
    packages.view.flatMap(_.orderedModules.flatMap(_.testClasses.toSeq)).toArray
  }

  private def findTestClassesFromPaths(paths: Array[String]): Array[ApexDeclaration] = {
    paths
      .map(p => Path(p))
      .flatMap(path => {
        findPackageIdentifier(path).flatMap(typeId => {
          typeId.module
            .findPackageType(typeId.typeName, None)
            .collect { case td: ApexDeclaration if td.inTest => td }
            .filter(_.outerTypeName.isEmpty)
        })
      })
  }

  private def hasTestModifier(modifiers: ArraySeq[Modifier]): Boolean = {
    modifiers.contains(TEST_METHOD_MODIFIER) || modifiers.contains(ISTEST_ANNOTATION)
  }

  private def findTestClassReferences(
    paths: Array[PathLike]
  ): Set[ReferencingCollector.TestInfo] = {
    // Locate starting typeIds for the passed paths, this includes super classes & interfaces
    val startingIds = paths.flatMap { path => findPackageIdentifier(path) }

    // Convert to source set by searching for related types
    val accum = mutable.Set[NodeInfo]()
    startingIds.foreach(typeId => {
      typeId.module
        .findPackageType(typeId.typeName, None)
        .collect { case td: ApexDeclaration => td }
        .foreach(td =>
          (td +: td.nestedTypes).foreach(td => sourcesForType(td, primary = true, accum))
        )
    })

    // Locate tests for the sourceIds
    val refs = ReferencingCollector
      .testReferences(accum.toSet)
      .filter(_.testClass.outerTypeName.isEmpty) // Safety check, we only want outer types here
    refs
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

  /** Collect source information on interfaces, recursive over super classes & includes interfaces.
    */
  private def sourcesForSuperclass(td: ApexDeclaration, accum: mutable.Set[NodeInfo]): Unit = {
    td.superClass.foreach { superclass =>
      toApexDeclaration(superclass, td).foreach(superClassTd =>
        sourcesForType(superClassTd, primary = false, accum)
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
