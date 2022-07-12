/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.memory.SkinnySet
import com.nawforce.apexlink.org.ReferenceProvider.emptyTargetLocations
import com.nawforce.apexlink.rpc.TargetLocation
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, FullDeclaration, SummaryDeclaration}
import com.nawforce.apexlink.types.core.{Dependent, PreReValidatable, TypeDeclaration, TypeId}
import com.nawforce.pkgforce.path.{PathLike, PathLocation}

trait Referenceable extends PreReValidatable {
  this: Dependent =>
  private var referenceLocations: SkinnySet[PathLocation] = new SkinnySet()

  override def preReValidate(): Unit = {
    super.preReValidate()
    referenceLocations = new SkinnySet()
  }

  def getTargetLocations: Array[TargetLocation] = {
    referenceLocations.toSet
      .map(ref => TargetLocation(ref.path.toString, ref.location))
      .toArray
  }

  def addLocation(pathLocation: PathLocation): Unit = {
    referenceLocations.add(pathLocation)
  }

  /**
    * Returns the detailed reference locations
    */
  def findReferences(): Set[TargetLocation]

  /**
    * Returns the types that needs to be revalidated to build up reference locations.
    * This represents a high level view of all the types that hold a references.
    */
  def getTypeIdsForReValidation: Set[TypeId]
}

trait ReferenceProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getReferences(path: PathLike, line: Int, offset: Int): Array[TargetLocation] = {
    val sourceTd = loadTypeFromModule(path) match {
      case Some(sm: SummaryDeclaration) =>
        reValidate(Set(sm.typeId) ++ sm.getTypeDependencyHolders.toSet)
        //Reload the source after summary type has been validated so we can get the full type
        loadTypeFromModule(path).getOrElse(return emptyTargetLocations)
      case Some(td) => td
      case None     => return emptyTargetLocations
    }

    getReferenceableFromValidation(sourceTd, line, offset)
      .orElse({
        sourceTd match {
          case fd: FullDeclaration =>
            fd.getBodyDeclarationFromLocation(line, offset)
              .map(_._2)
              .collect({ case ref: Referenceable => ref })
          case _ => None
        }
      })
      .map(ref => {
        //ReValidate any references so that ReferenceLocations can be built up
        reValidate(ref.getTypeIdsForReValidation)
        ref.findReferences().toArray
      })
      .getOrElse(emptyTargetLocations)
  }

  private def getReferenceableFromValidation(
    td: TypeDeclaration,
    line: Int,
    offset: Int
  ): Option[Referenceable] = {
    td match {
      case td: ApexFullDeclaration =>
        val result = locateFromValidation(td, line, offset)
        result._2.flatMap(
          result
            ._1(_)
            .result
            .locatable
            .collect({ case ref: Referenceable => ref })
        )
      case _ => None
    }

  }

}
object ReferenceProvider {
  private val emptyTargetLocations: Array[TargetLocation] = Array.empty
}
