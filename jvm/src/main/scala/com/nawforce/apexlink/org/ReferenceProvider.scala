/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.ExprContext
import com.nawforce.apexlink.memory.SkinnySet
import com.nawforce.apexlink.org.ReferenceProvider.emptyTargetLocations
import com.nawforce.apexlink.rpc.TargetLocation
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexFullDeclaration,
  FullDeclaration,
  SummaryDeclaration
}
import com.nawforce.apexlink.types.core.{Dependent, DependentType, TypeDeclaration, TypeId}
import com.nawforce.pkgforce.path.{PathLike, PathLocation}

import scala.collection.mutable

/** This trait is used to mark a class as being referenceable. It is used in conjunction with
  * ReferenceProvider to find all the locations that reference something in Apex code,
  * currently only method references (aka method calls) are supported.
  *
  * @see ReferenceProvider
  */
trait Referenceable {
  this: Dependent =>

  /** Referencable must be able to provide a TypeId for the containing type to support revalidation. */
  val thisTypeId: TypeId

  /** Temporary storage for locations referencing this Referenceable, null default to space save. */
  private var referenceLocations: SkinnySet[TargetLocation] = _

  /** Add a reference location to the list of references for this specific Referencable. Adding
    * references is gated so that we can control when references are collected. This must be
    * called from some part of the validation logic to establish the references.
    */
  def addReferencingLocation(referencingLocation: PathLocation): Unit = {
    if (Referenceable.allowReferenceCollection) {
      if (referenceLocations == null) {
        referenceLocations = new SkinnySet[TargetLocation]()
      }
      referenceLocations.add(
        TargetLocation(referencingLocation.path.toString, referencingLocation.location)
      )
      Referenceable.addReference(this)
    }
  }

  /** Collect all the referenceable elements that are related to this class. Override this
    * to expand the reference search, such as for method overloading.
    */
  def collectRelatedReferencable(): Set[_ <: Referenceable] = Set[Referenceable](this)

  /** Collect all reference locations for this Referencable. The basic approach here
    * is to collect references to things during a revalidation pass. The ReferenceProvider
    * uses this.
    */
  def getCurrentReferences(
    line: Int,
    offset: Int,
    refresh: Seq[PathLike] => Unit
  ): Set[TargetLocation] = {

    try {
      Referenceable.allowReferenceCollection = true

      // Re-validate to remove summary types and build up the references
      // We used to cache here but as we expand the number of Referenceable things that
      // would cause some memory bloat, this will be slower but hopefully OK
      refresh(
        collectHolderTypeIds(collectRelatedReferencable())
          .flatMap(_.toTypeDeclaration[ApexClassDeclaration])
          .map(_.paths.head)
          .toSeq
      )

      // Find the target Referencable as it may have been replaced during the refresh
      thisTypeId
        .toTypeDeclaration[FullDeclaration]
        .flatMap(_.getBodyDeclarationFromLocation(line, offset))
        .map(_._2)
        .flatMap({
          case newRef: Referenceable =>
            // Now gather the created references as the result
            Some(newRef.collectRelatedReferencable().flatMap(_.getReferencingLocations))
          case _ => Some(Set[TargetLocation]())
        })
        .getOrElse(Set.empty)

    } finally {
      Referenceable.allowReferenceCollection = false
      Referenceable.clearReferences()
    }
  }

  /** Collects all the dependency holder classes for the passed Referencable set */
  private def collectHolderTypeIds(referenceable: Set[_ <: Referenceable]): Set[TypeId] = {
    referenceable
      .map(_.thisTypeId)
      .flatMap(id => id.toTypeDeclaration[DependentType])
      .flatMap(td => {
        (td.outermostTypeDeclaration match {
          case d: DependentType => d.getTypeDependencyHolders.toSet
          case _                => Set.empty[TypeId]
        }) ++ Set(td.outerTypeId)
      })
  }

  /** Get all accumulated referencing locations. */
  private def getReferencingLocations: Set[TargetLocation] = {
    if (referenceLocations == null) {
      Set.empty
    } else {
      referenceLocations.toSet
    }
  }
}

object Referenceable {

  /** Gate controlling if reference collection is enabled. */
  private var allowReferenceCollection: Boolean = false

  /** Set of all references that have recorded reference locations so we can
    * clear them after processing.
    */
  private val collectedReferences: mutable.Set[Referenceable] = mutable.Set.empty

  /** Add a Referenceable that has recorded a reference location */
  private def addReference(ref: Referenceable): Unit = {
    collectedReferences.add(ref)
  }

  /** Clear all Referenceable that are known to have some reference locations */
  private def clearReferences(): Unit = {
    collectedReferences.foreach(_.referenceLocations = null)
    collectedReferences.clear()
  }
}

trait ReferenceProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getReferences(path: PathLike, line: Int, offset: Int): Array[TargetLocation] = {
    val sourceTd = loadTypeFromModule(path) match {
      case Some(_: SummaryDeclaration) =>
        // Refresh the summary type to get the full type
        refreshBatched(Seq(RefreshRequest(this, path, highPriority = true)))
        loadTypeFromModule(path).getOrElse(return emptyTargetLocations)
      case Some(td) => td
      case None     => return emptyTargetLocations
    }

    val expr = getExpressionFromValidation(sourceTd, line, offset)

    if (expr.nonEmpty && expr.get.locatable.isEmpty)
      return emptyTargetLocations

    val locationOpt = expr
      .flatMap(_.locatable)
      .orElse({
        sourceTd match {
          case fd: FullDeclaration =>
            fd.getBodyDeclarationFromLocation(line, offset)
              .map(_._2)
        }
      })

    locationOpt
      .flatMap({
        case ref: Referenceable =>
          val targetLocation = locationOpt.get.location.location
          Some(
            ref
              .getCurrentReferences(
                targetLocation.startLine,
                targetLocation.startPosition,
                (paths: Seq[PathLike]) => {
                  refreshBatched(paths.map(path => RefreshRequest(this, path, highPriority = true)))
                }
              )
              .toArray
          )
        case _ => None
      })
      .getOrElse(emptyTargetLocations)
  }

  private def getExpressionFromValidation(
    td: TypeDeclaration,
    line: Int,
    offset: Int
  ): Option[ExprContext] = {
    td match {
      case td: ApexFullDeclaration =>
        val result = locateFromValidation(td, line, offset)
        result._2.map(loc => result._1(loc).result)
      case _ => None
    }

  }

}

object ReferenceProvider {
  private val emptyTargetLocations: Array[TargetLocation] = Array.empty
}
