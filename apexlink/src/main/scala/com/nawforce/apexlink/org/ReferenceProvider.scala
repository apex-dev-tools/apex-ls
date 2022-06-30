/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.ClassBodyDeclaration
import com.nawforce.apexlink.org.ReferenceProvider.emptyTargetLocations
import com.nawforce.apexlink.rpc.TargetLocation
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, FullDeclaration, SummaryDeclaration}
import com.nawforce.apexlink.types.core.{Dependent, TypeDeclaration}
import com.nawforce.pkgforce.path.{IdLocatable, PathLike}

trait Referencable {
  this: Dependent =>
  def findReferences(): Array[TargetLocation] = {
    getDependencyHolders
      .collect({ case loc: IdLocatable with ClassBodyDeclaration => loc })
      .map(ref => TargetLocation(ref.location.path.toString, ref.idLocation))
      .toArray
  }
}

trait ReferenceProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getReferences(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[TargetLocation] = {
    val sourceTD = loadTypeFromModule(path).getOrElse(return emptyTargetLocations)
    sourceTD match {
      case sm: SummaryDeclaration => reValidate(Set(sm.typeId) ++ sm.getTypeDependencyHolders.toSet)
      case _                      =>
    }

    getFromValidation(sourceTD, line, offset)
      .getOrElse({
        sourceTD match {
          case fd: FullDeclaration =>
            fd.getBodyDeclarationFromLocation(line, offset)
              .map(_._2)
              .collect({ case ref: Referencable => ref })
              .map(_.findReferences())
              .getOrElse(emptyTargetLocations)
          case _ => emptyTargetLocations
        }
      })
  }

  private def getFromValidation(
    td: TypeDeclaration,
    line: Int,
    offset: Int
  ): Option[Array[TargetLocation]] = {
    td match {
      case td: ApexFullDeclaration =>
        val result = locateFromValidation(td, line, offset)
        result._2.flatMap(
          result
            ._1(_)
            .result
            .locatable
            .collect({ case ref: Referencable => ref })
            .map(_.findReferences())
        )
      case _ => None
    }

  }

}
object ReferenceProvider {
  private val emptyTargetLocations: Array[TargetLocation] = Array.empty
}
