/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.ExprContext
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.memory.SkinnySet
import com.nawforce.apexlink.org.ReferenceProvider.{TypeIdOps, emptyTargetLocations}
import com.nawforce.apexlink.rpc.TargetLocation
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexFullDeclaration,
  FullDeclaration,
  PreReValidatable,
  SummaryDeclaration
}
import com.nawforce.apexlink.types.core.{Dependent, TypeDeclaration, TypeId}
import com.nawforce.pkgforce.path.{PathLike, PathLocation}

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

trait Referenceable extends PreReValidatable {
  this: Dependent =>
  private var referenceLocations: SkinnySet[PathLocation] = new SkinnySet()
  private var cache: (Int, Set[TargetLocation])           = (0, Set.empty)

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

  def findReferences(): Set[TargetLocation] = {
    val hashCode = hash()
    if (hashCode != cache._1)
      cache = (hashCode, collectReferences())
    cache._2
  }

  def doesNeedReValidation(): Boolean = {
    hash() != cache._1
  }

  /**
    * This represents a high level view of all the types that hold a reference to this object.
    * Returns the types that needs can be revalidated to build up  the more specific reference locations.
    */
  def getReferenceHolderTypeIds: Set[TypeId]

  /**
    * Returns the detailed reference locations
    */
  protected def collectReferences(): Set[TargetLocation]

  private def hash(): Int = {
    MurmurHash3.arrayHash(
      getReferenceHolderTypeIds.toArray
        .flatMap(id => id.toTypeDeclaration[ApexClassDeclaration])
        .map(_.deepHash)
    )
  }
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

    val expr = getExpressionFromValidation(sourceTd, line, offset)

    if (expr.nonEmpty && expr.get.locatable.isEmpty)
      return emptyTargetLocations

    expr
      .flatMap(_.locatable)
      .orElse({
        sourceTd match {
          case fd: FullDeclaration =>
            fd.getBodyDeclarationFromLocation(line, offset)
              .map(_._2)
        }
      })
      .flatMap({
        case ref: Referenceable =>
          //ReValidate any references so that ReferenceLocations can be built up
          if (ref.doesNeedReValidation())
            reValidate(ref.getReferenceHolderTypeIds)
          Some(ref.findReferences().toArray)
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

  implicit class TypeIdOps(typeId: TypeId) {
    def toTypeDeclaration[T <: TypeDeclaration: ClassTag]: Option[T] = {
      TypeResolver(typeId.typeName, typeId.module).toOption.collect { case td: T => td }
    }
  }
}
