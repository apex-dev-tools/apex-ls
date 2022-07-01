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

import com.nawforce.apexlink.api.{Package, TypeSummary}
import com.nawforce.pkgforce.modifiers.ISTEST_ANNOTATION
import com.nawforce.pkgforce.names.TypeIdentifier

import scala.collection.mutable

// Test class discover. The logic here is a bit more involved because it operates over summary types exclusively.
// This is useful to avoid the need to fully load everything.
trait OrgTestClasses {
  self: OPM.OrgImpl =>

  def getTestClassNames(paths: Array[String]): Array[String] = {
    getTestClassNames(paths, findTests = true)
  }

  // Note: If findTests is false, targets are only returned if they are test classes, which means they have been
  // passed to us rather than discovered via dependencies. Really this should be handled via a standalone API
  // rather than overloading test class discovery via dependencies.
  def getTestClassNames(paths: Array[String], findTests: Boolean): Array[String] = {
    paths.flatMap { path =>
      findPackageIdentifierAndSummary(path).toArray.flatMap {
        case (pkg, typeId, summary) =>
          findTargets(pkg, typeId, summary).flatMap(_.findReferencingTests(pkg, findTests))
      }
    }.distinct
  }

  // Given a summary type find related targets of interest. Related in this case means finding interfaces and
  // superclasses which may have tests targeting them.
  private def findTargets(
    pkg: Package,
    typeId: TypeIdentifier,
    summary: TypeSummary
  ): Array[TargetInfo] = {
    val accum = mutable.ArrayBuffer[TargetInfo]()

    accum.addOne(TargetInfo(typeId, typeId, summary))
    targetsForInterfaces(pkg, summary, accum)
    summary.nestedTypes.foreach { nested => targetsForInterfaces(pkg, nested, accum) }
    targetsForSuperclass(pkg, summary, accum)

    accum.toArray
  }

  // Retrieve basic info from a path
  private def findPackageIdentifierAndSummary(
    path: String
  ): Option[(Package, TypeIdentifier, TypeSummary)] = {
    packages.view
      .flatMap(pkg => {
        Option(pkg.getTypeOfPath(path))
          .flatMap(
            typeId =>
              Option(pkg.getSummaryOfType(typeId))
                .map(summary => (pkg, typeId, summary))
          )
      })
      .headOption
  }

  // Collect targets by walking via superclass(s), including their interfaces
  private def targetsForSuperclass(
    pkg: Package,
    summary: TypeSummary,
    accum: mutable.ArrayBuffer[TargetInfo]
  ): Unit = {
    summary.superClass
      .foreach { typeName =>
        Option(pkg.getTypeIdentifier(typeName))
          .foreach { typeIdentifier =>
            if (!accum.exists(_.actual == typeIdentifier)) {
              Option(pkg.getSummaryOfType(typeIdentifier))
                .foreach { summary =>
                  val outerTypeIdentifier =
                    typeIdentifier.typeName.outer
                      .map(pkg.getTypeIdentifier)
                      .getOrElse(typeIdentifier)
                  accum.addOne(TargetInfo(typeIdentifier, outerTypeIdentifier, summary))
                  targetsForInterfaces(pkg, summary, accum)
                }
            }
          }
      }
  }

  // Collect target information on interfaces (possibly recursively)
  private def targetsForInterfaces(
    pkg: Package,
    summary: TypeSummary,
    accum: mutable.ArrayBuffer[TargetInfo]
  ): Unit = {
    summary.interfaces.foreach { interface =>
      {
        Option(pkg.getTypeIdentifier(interface))
          .foreach { interfaceTypeId =>
            if (!accum.exists(_.actual == interfaceTypeId)) {
              // Try for outer, but fallback to given typename
              val summary = interfaceTypeId.typeName.outer
                .map(pkg.getTypeIdentifier)
                .flatMap(t => Option(pkg.getSummaryOfType(t)))
                .orElse(Option(pkg.getSummaryOfType(interfaceTypeId)))

              // If we find, log & recurse on it
              summary.foreach(summary => {
                accum.addOne(
                  TargetInfo(
                    interfaceTypeId,
                    TypeIdentifier(interfaceTypeId.namespace, summary.typeName),
                    summary
                  )
                )
                targetsForInterfaces(pkg, summary, accum)
              })
            }
          }
      }
    }
  }
}

// For targets we collect outer details
case class TargetInfo(
  actual: TypeIdentifier,
  outerOrActual: TypeIdentifier,
  outerSummary: TypeSummary
) {

  // Find test class names that refer to this target
  def findReferencingTests(pkg: Package, findTests: Boolean): Array[String] = {
    if (outerSummary.modifiers.contains(ISTEST_ANNOTATION))
      return Array(outerSummary.name)
    if (!findTests)
      return Array.empty

    Option(pkg.getDependencyHolders(outerOrActual, apexOnly = true))
      .getOrElse(Array.empty)
      .flatMap { holderTypeId =>
        if (pkg.hasDependency(holderTypeId, actual)) {
          Option(pkg.getSummaryOfType(holderTypeId))
            .filter(_.modifiers.contains(ISTEST_ANNOTATION))
            .map(_.name)
        } else {
          None
        }
      }
  }

}
