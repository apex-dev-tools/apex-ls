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

import com.nawforce.apexlink.cst.InterfaceDeclaration
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.LocationLink
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexDeclaration, ApexFullDeclaration}
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.modifiers.{ABSTRACT_MODIFIER, VIRTUAL_MODIFIER}
import com.nawforce.pkgforce.path.{IdLocatable, Locatable, PathLike, UnsafeLocatable}

trait DefinitionProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getDefinition(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[LocationLink] = {
    // Make sure we have access to source code and a type to resolve things against
    val sourceAndType = loadFullSourceAndType(path, content)
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
