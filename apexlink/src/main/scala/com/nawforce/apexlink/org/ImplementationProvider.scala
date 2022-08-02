/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.org

import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.LocationLink
import com.nawforce.apexlink.types.apex.{ApexDeclaration, ApexMethodLike}
import com.nawforce.apexlink.types.core.{Dependent, DependentType, TypeDeclaration}
import com.nawforce.pkgforce.modifiers.{ABSTRACT_MODIFIER, VIRTUAL_MODIFIER}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, INTERFACE_NATURE}
import com.nawforce.pkgforce.path.{Locatable, PathLike}

import scala.collection.mutable.ArrayBuffer

trait ImplementationProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getImplementation(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[LocationLink] = {
    val sourceAndType = loadSource(path, content).flatMap(source => {
      loadTypeFromModule(path) match {
        case Some(dt: DependentType) => Some((source, dt))
        case _                       => loadRawType(path, source)
      }
    })

    if (sourceAndType.isEmpty)
      return Array.empty

    getImplementation(line, offset, sourceAndType)
  }

  private def getImplementation(
    line: Int,
    offset: Int,
    sourceAndType: Option[(String, DependentType)]
  ): Array[LocationLink] = {
    def getTransitiveDependents(td: DependentType): ArrayBuffer[TypeDeclaration] = {
      td.getTypeDependencyHolders.toIterable.foldLeft(ArrayBuffer[TypeDeclaration]())((acc, id) => {
        TypeResolver(id.typeName, id.module).toOption match {
          //if the used by declaration is extensible, find the other classes that use it and add it to the acc
          case Some(ExtensibleClassesAndInterface(clsOrInterface)) =>
            acc.appendAll(
              getTransitiveDependents(clsOrInterface)
                .appendAll(clsOrInterface.nestedTypes)
                .append(clsOrInterface)
            )
          case Some(value) => acc.append(value)
          case _           =>
        }
        acc
      })
    }

    def getSearchContext(td: TypeDeclaration): Option[Dependent with Locatable] = {
      td match {
        case ExtensibleClassesAndInterface(td) =>
          //Only find method or class declaration to search implementations for
          td.methods
            .collect { case m: ApexMethodLike => m }
            .find(_.location.location.contains(line, offset))
            .orElse({
              (td, td.location.location.contains(line, offset)) match {
                case (ad: ApexDeclaration, true) => Some(ad)
                case _                           => None
              }
            })

        case _ =>
          if (td.nestedTypes.isEmpty) None else td.nestedTypes.flatMap(getSearchContext).headOption
      }
    }

    val source   = sourceAndType.get._1
    val sourceTD = sourceAndType.get._2

    val searchLocation =
      source
        .extractDotTermInclusive(() => new IdentifierLimiter, line, offset)
        .orElse(return Array.empty)
        .get
        ._2

    val searchContext = getSearchContext(sourceTD)

    lazy val usedByTds = getTransitiveDependents(sourceTD)

    searchContext match {
      case Some(method: ApexMethodLike) =>
        method
          .collectMethods()
          .filterNot(_ eq method)
          .map(
            m =>
              LocationLink(
                searchLocation,
                m.location.path.toString,
                m.location.location,
                m.idLocation
              )
          )
          .toArray
      case Some(td: ApexDeclaration) =>
        usedByTds
          .collect { case fd: ApexDeclaration => fd }
          .filter(_.nature == CLASS_NATURE)
          .filter(_.superTypes().contains(td.typeName))
          .map(
            fd =>
              LocationLink(
                searchLocation,
                fd.location.path.toString,
                fd.location.location,
                fd.idLocation
              )
          )
          .distinct
          .toArray
      case _ => Array.empty
    }
  }

}

private object ExtensibleClassesAndInterface {
  def unapply(td: TypeDeclaration): Option[DependentType with Locatable] =
    Option.when(td.isExtensible) { td } collect { case td: DependentType with Locatable => td }
}
