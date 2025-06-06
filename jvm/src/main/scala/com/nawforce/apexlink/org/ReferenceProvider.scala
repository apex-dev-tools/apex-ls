/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.memory.SkinnySet
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.org.ReferenceProvider.emptyTargetLocations
import com.nawforce.apexlink.rpc.TargetLocation
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, FullDeclaration}
import com.nawforce.apexlink.types.core.{Dependent, DependentType, TypeDeclaration, TypeId}
import com.nawforce.pkgforce.path.{IdLocatable, PathLike, PathLocation}
import com.nawforce.apexlink.types.platform.GenericPlatformTypeDeclaration
import com.nawforce.pkgforce.diagnostics.LoggerOps

import scala.collection.mutable

/** This trait is used to mark a class as being referenceable. It is used in conjunction with
  * ReferenceProvider to find all the locations that reference something in Apex code,
  * currently only method references (aka method calls) are supported.
  *
  * @see ReferenceProvider
  */
trait Referenceable extends IdLocatable {
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
      if (referencingLocation.path == null) {
        LoggerOps.debug(
          s"Referenceable.addReferencingLocation: No referencing path provided to $this, location: $referencingLocation"
        )
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
  def getCurrentReferences(refresh: Seq[PathLike] => Unit): Set[TargetLocation] = {

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
        .flatMap(td => {
          if (td.idLocation.contains(idLocation.startLine, idLocation.startPosition))
            Some(td)
          else
            td.getBodyDeclarationFromLocation(idLocation.startLine, idLocation.startPosition)
              .map(_._2)
        })
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

  /** Helper to add a reference location for something which may be a Referenceable */
  def addReferencingLocation(
    referenceable: Any,
    location: PathLocation,
    from: TypeDeclaration
  ): Unit = {

    referenceable match {
      case td: GenericPlatformTypeDeclaration if td.typeName.isList || td.typeName.isSet =>
        TypeResolver(td.typeName.params.head, from).toOption.foreach(td =>
          Referenceable.addReferencingLocation(td, location, from)
        )
      case td: GenericPlatformTypeDeclaration if td.typeName.isMap =>
        TypeResolver(td.typeName.params.head, from).toOption.foreach(td =>
          Referenceable.addReferencingLocation(td, location, from)
        )
        TypeResolver(td.typeName.params.last, from).toOption.foreach(td =>
          Referenceable.addReferencingLocation(td, location, from)
        )
      case ref: Referenceable => ref.addReferencingLocation(location)
      case _                  => ()
    }
  }

  /** Helper to add a reference location for a pair which may be Referenceable */
  def addReferencingLocation(
    referenceDeclaration: TypeDeclaration,
    referenceable: Any,
    location: PathLocation,
    from: TypeDeclaration
  ): Unit = {
    addReferencingLocation(referenceDeclaration, location, from)
    addReferencingLocation(referenceable, location, from)
  }

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
    loadTypeFromModule(path)
      .collect({ case td: ApexClassDeclaration => td })
      .flatMap(_.findReferenceableFromLocation(line, offset))
      .map(_.getCurrentReferences((paths: Seq[PathLike]) => {
        refreshBatched(paths.map(path => RefreshRequest(this, path, highPriority = true)))
      }).toArray)
      .getOrElse(emptyTargetLocations)
  }
}

object ReferenceProvider {
  private val emptyTargetLocations: Array[TargetLocation] = Array.empty
}
