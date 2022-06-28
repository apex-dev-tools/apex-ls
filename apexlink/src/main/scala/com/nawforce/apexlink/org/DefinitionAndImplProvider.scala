/*
 Copyright (c) 2021 Kevin Jones & FinancialForce, All rights reserved.
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

import com.nawforce.apexlink.cst.{ApexMethodDeclaration, InterfaceDeclaration}
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.LocationLink
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexDeclaration,
  ApexFullDeclaration,
  ApexMethodLike
}
import com.nawforce.apexlink.types.core.{Dependent, TypeDeclaration}
import com.nawforce.pkgforce.modifiers.{ABSTRACT_MODIFIER, VIRTUAL_MODIFIER}
import com.nawforce.pkgforce.parsers.CLASS_NATURE
import com.nawforce.pkgforce.path.{IdLocatable, Locatable, PathLike, UnsafeLocatable}

import scala.collection.mutable.ArrayBuffer

trait DefinitionAndImplProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getDefinition(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[LocationLink] = {
    // Make sure we have access to source code and a type to resolve things against
    val sourceAndType = loadSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return Array.empty

    getFromValidation(sourceAndType.get._2, line, offset)
      .orElse({

        val source   = sourceAndType.get._1
        val sourceTD = sourceAndType.get._2
        val searchTermAndLocation =
          source.extractDotTermInclusive(() => new IdentifierLimiter, line, offset)
        if (searchTermAndLocation.isEmpty)
          return Array.empty
        val searchTerm     = searchTermAndLocation.get._1
        val sourceLocation = searchTermAndLocation.get._2

        sourceTD
          .findDeclarationFromSourceReference(searchTerm, sourceLocation)
          .map(ad => {
            LocationLink(
              sourceLocation,
              ad.location.path.toString,
              ad.location.location,
              ad.idLocation
            )
          })
      })
      .toArray
  }

  def getImplementation(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[LocationLink] = {
    val sourceAndType = loadSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return Array.empty

    getImplementation(line, offset, sourceAndType)
  }

  private def getImplementation(
    line: Int,
    offset: Int,
    sourceAndType: Option[(String, ApexFullDeclaration)]
  ): Array[LocationLink] = {
    def getTransitiveDependents(td: ApexDeclaration): ArrayBuffer[TypeDeclaration] = {
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

    val source   = sourceAndType.get._1
    val sourceTD = sourceAndType.get._2

    val searchLocation =
      source
        .extractDotTermInclusive(() => new IdentifierLimiter, line, offset)
        .orElse(return Array.empty)
        .get
        ._2

    def getSearchContext(td: TypeDeclaration): Option[Dependent with IdLocatable] = {
      td match {
        case ExtensibleClassesAndInterface(td) =>
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
    val searchContext = getSearchContext(sourceTD)

    val usedByTds = getTransitiveDependents(sourceTD)

    searchContext match {
      case Some(method: ApexMethodDeclaration) =>
        usedByTds
          .flatMap(
            _.methods
              .collect { case m: ApexMethodLike => m }
              .find(m => m.signature == method.signature)
          )
          .map(
            m =>
              LocationLink(
                searchLocation,
                m.location.path.toString,
                m.location.location,
                m.idLocation
              )
          )
          .distinct
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

  private def getFromValidation(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): Option[LocationLink] = {
    val validation = locateFromValidation(td, line, offset)

    validation._2.flatMap(loc => {
      // If the result has a locatable we can use that as the target, beware the order here matters due
      // to both inheritance and some objects supporting multiple Locatable traits
      validation._1(loc).result.locatable match {
        case Some(l: IdLocatable) =>
          Some(LocationLink(loc, l.location.path.toString, l.location.location, l.idLocation))
        case Some(l: UnsafeLocatable) =>
          Option(l.location).map(l => LocationLink(loc, l.path.toString, l.location, l.location))
        case Some(l: Locatable) =>
          Some(
            LocationLink(loc, l.location.path.toString, l.location.location, l.location.location)
          )
        case _ =>
          None
      }
    })
  }
}

private object ExtensibleClassesAndInterface {
  def unapply(td: TypeDeclaration): Option[ApexDeclaration] = {
    td match {
      case id: InterfaceDeclaration => Some(id)
      case cd: ApexClassDeclaration =>
        val modifiers = cd.modifiers.toSet
        if (modifiers.intersect(Set(ABSTRACT_MODIFIER, VIRTUAL_MODIFIER)).nonEmpty)
          Some(cd)
        else None
      case _ => None
    }
  }
}
