/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.memory.SkinnySet
import com.nawforce.apexlink.org.ReferenceProvider.emptyTargetLocations
import com.nawforce.apexlink.rpc.TargetLocation
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, FullDeclaration, SummaryDeclaration}
import com.nawforce.apexlink.types.core.{Dependent, PreReValidatable, TypeDeclaration}
import com.nawforce.pkgforce.path.{IdLocatable, PathLike, PathLocation}

trait Referencable extends PreReValidatable {
  this: Dependent =>
  private var referenceLocations: SkinnySet[PathLocation] = new SkinnySet()

  override def preReValidate(): Unit = {
    super.preReValidate()
    referenceLocations = new SkinnySet()
  }

  def addLocation(pathLocation: PathLocation): Unit = {
    referenceLocations.add(pathLocation)
  }
  //Find all the holders and find the locations of the usage inside the block
  def findReferences(): Array[TargetLocation] = {
    getDependencyHolders
      .collect({ case idLocatable: IdLocatable => idLocatable })
      .flatMap(ref => referenceLocations.toSet.filter(_.path == ref.location.path))
      .map(ref => TargetLocation(ref.path.toString, ref.location))
      .toArray
  }
}

trait ReferenceProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getReferences(path: PathLike, line: Int, offset: Int): Array[TargetLocation] = {
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
