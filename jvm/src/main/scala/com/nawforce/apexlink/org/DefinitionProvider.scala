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

import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.LocationLink
import com.nawforce.apexlink.types.apex.ApexFullDeclaration
import com.nawforce.pkgforce.path.{IdLocatable, Locatable, Location, PathLike, UnsafeLocatable}

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

        val source     = sourceAndType.get._1
        val sourceType = sourceAndType.get._2
        val searchTermAndLocation =
          source.extractDotTermInclusive(() => new IdentifierLimiter, line, offset)
        if (searchTermAndLocation.isEmpty)
          return Array.empty
        val searchTerm     = searchTermAndLocation.get._1
        val sourceLocation = searchTermAndLocation.get._2

        sourceType
          .findDeclarationFromSourceReference(searchTerm, sourceLocation)
          .flatMap(targetType => {
            resolveLocation(sourceLocation, targetType)
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

    validation._2.flatMap(source => {
      validation._1(source).result.locatable.flatMap(target => resolveLocation(source, target))
    })
  }

  private def resolveLocation(source: Location, target: Locatable): Option[LocationLink] = {
    // Beware the order here matters due to both inheritance and some objects
    // supporting multiple Locatable traits
    target match {
      case l: IdLocatable =>
        Some(LocationLink(source, l.location.path.toString, l.location.location, l.idLocation))
      case l: UnsafeLocatable =>
        Option(l.location).map(l => LocationLink(source, l.path.toString, l.location, l.location))
      case l: Locatable =>
        Some(
          LocationLink(source, l.location.path.toString, l.location.location, l.location.location)
        )
      case _ =>
        None
    }
  }
}
