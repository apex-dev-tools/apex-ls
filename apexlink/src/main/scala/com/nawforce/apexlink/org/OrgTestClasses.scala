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

import scala.collection.immutable.ArraySeq

trait OrgTestClasses {
  self: OPM.OrgImpl =>

  def getTestClassNames(paths: Array[String]): Array[String] = {
    getTestClassNames(paths, findTests = true)
  }

  def getTestClassNames(paths: Array[String], findTests: Boolean): Array[String] = {
    def findPackageIdentifierAndSummary(
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

    def findReferencedTestPaths(
      pkg: Package,
      typeId: TypeIdentifier,
      summary: TypeSummary,
      filterTypeId: TypeIdentifier
    ): Array[String] = {
      if (summary.modifiers.contains(ISTEST_ANNOTATION)) return Array(summary.name)

      Option(pkg.getDependencyHolders(typeId, apexOnly = true)).getOrElse(Array.empty).flatMap {
        dependentTypeId =>
          Option(pkg.getSummaryOfType(dependentTypeId)).toArray
            .filter { dependentSummary =>
              dependentSummary.modifiers.contains(ISTEST_ANNOTATION)
            }
            .filter { _ =>
              pkg.hasDependency(dependentTypeId, filterTypeId)
            }
            .map { dependentSummary =>
              dependentSummary.name
            }
      }
    }

    def targetsForInterfaces(
      pkg: Package,
      summary: TypeSummary
    ): ArraySeq[(TypeIdentifier, TypeIdentifier, TypeSummary)] = {
      summary.interfaces.flatMap { interface =>
        Option(pkg.getTypeIdentifier(interface))
          .flatMap { interfaceTypeId =>
            val outerTypeId =
              interfaceTypeId.typeName.outer.map(pkg.getTypeIdentifier).getOrElse(interfaceTypeId)
            Option(pkg.getSummaryOfType(outerTypeId))
              .map((interfaceTypeId, outerTypeId, _))
          }
      }
    }

    def targetsForSuperclass(
      pkg: Package,
      summary: TypeSummary
    ): Array[(TypeIdentifier, TypeIdentifier, TypeSummary)] = {
      summary.superClass
        .flatMap { tn =>
          Option(pkg.getTypeIdentifier(tn))
            .flatMap { tid =>
              Option(pkg.getSummaryOfType(tid))
                .flatMap { summary =>
                  val otid = tid.typeName.outer.map(pkg.getTypeIdentifier).getOrElse(tid)
                  Some(Array((tid, otid, summary)) ++ targetsForInterfaces(pkg, summary))
                }
            }
        }
        .getOrElse(Array.empty)
    }

    paths.flatMap { path =>
      findPackageIdentifierAndSummary(path).toArray.flatMap {
        case (pkg, typeId, summary) =>
          val interfaces = targetsForInterfaces(pkg, summary)
          val nestedInterfaces = summary.nestedTypes.flatMap { nestedSummary =>
            targetsForInterfaces(pkg, nestedSummary)
          }
          val superClassTargets = targetsForSuperclass(pkg, summary)

          val targets =
            Seq((typeId, typeId, summary)) ++ interfaces ++ nestedInterfaces ++ superClassTargets

          targets.flatMap {
            case (actualTypeId, outerTypeId, outerSummary) =>
              findReferencedTestPaths(pkg, outerTypeId, outerSummary, actualTypeId)
          }
      }
    }.distinct
  }

}
