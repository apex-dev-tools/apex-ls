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

import com.nawforce.apexlink.cst.{ApexMethodDeclaration, ClassDeclaration, InterfaceDeclaration}
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.LocationLink
import com.nawforce.apexlink.types.apex.{
  ApexFullDeclaration,
  ApexMethodLike,
  FullDeclaration,
  TriggerDeclaration
}
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.documents.{ApexClassDocument, ApexTriggerDocument, MetadataDocument}
import com.nawforce.pkgforce.modifiers.ABSTRACT_MODIFIER
import com.nawforce.pkgforce.parsers.CLASS_NATURE
import com.nawforce.pkgforce.path.{IdLocatable, Locatable, PathLike, UnsafeLocatable}

trait DefinitionProvider {
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

    locateFromValidation(sourceAndType.get._2, line, offset)
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

    val source   = sourceAndType.get._1
    val sourceTD = sourceAndType.get._2
    val searchTermAndLocation =
      source.extractDotTermInclusive(() => new IdentifierLimiter, line, offset)

    if (searchTermAndLocation.isEmpty)
      return Array.empty

    val searchContext = sourceTD match {
      case AbstractClassesAndInterface(td) =>
        td.bodyDeclarations
          .find(_.location.location.contains(line, offset))
          .orElse({
            (sourceTD, sourceTD.location.location.contains(line, offset)) match {
              case (fd: FullDeclaration, true) => Some(fd)
              case _                           => None
            }
          })
      case _ => None
    }

    val usedByTds = sourceTD.getTypeDependencyHolders.toIterable.flatMap(
      id => TypeResolver(id.typeName, id.module).toOption
    )

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
                searchTermAndLocation.get._2,
                m.location.path.toString,
                m.location.location,
                m.idLocation
              )
          )
          .toArray
      case Some(_: ClassDeclaration) =>
        usedByTds
          .collect { case fd: FullDeclaration => fd }
          .filterNot(_.modifiers.contains(ABSTRACT_MODIFIER))
          .filter(_.nature == CLASS_NATURE)
          .map(
            fd =>
              LocationLink(
                searchTermAndLocation.get._2,
                fd.location.path.toString,
                fd.location.location,
                fd.idLocation
              )
          )
          .toArray
      case None => Array.empty
    }
  }

  private def loadSourceAndType(
    path: PathLike,
    content: Option[String]
  ): Option[(String, ApexFullDeclaration)] = {
    // We need source code no matter what
    val sourceOpt = content.orElse(path.read().toOption)
    if (sourceOpt.isEmpty)
      return None

    // If we don't have new source we can assume the loaded type is current, but it could be a summary
    if (content.isEmpty) {
      MetadataDocument(path) collect {
        case doc: ApexTriggerDocument =>
          orderedModules.view
            .flatMap(_.moduleType(doc.typeName(namespace)))
            .headOption
            .collect { case td: TriggerDeclaration => td }
            .orElse({
              loadTrigger(path, sourceOpt.get)._2
            })
            .map(td => (sourceOpt.get, td))
            .get
        case doc: ApexClassDocument =>
          orderedModules.view
            .flatMap(_.moduleType(doc.typeName(namespace)))
            .headOption
            .collect { case td: FullDeclaration => td }
            .orElse({
              loadClass(path, sourceOpt.get)._2
            })
            .map(td => (sourceOpt.get, td))
            .get
      }
    } else {
      // No option but to load it as content is being provided
      if (path.basename.toLowerCase.endsWith(".trigger")) {
        loadTrigger(path, sourceOpt.get)._2.map(td => (sourceOpt.get, td))
      } else {
        loadClass(path, sourceOpt.get)._2.map(td => (sourceOpt.get, td))
      }
    }
  }

  /** Extract a location link from an expression at the passed location */
  private def locateFromValidation(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): Option[LocationLink] = {
    val resultMap = td.getValidationMap(line, offset)

    // Find the inner-most expression containing location from those that do
    val exprLocations = resultMap.keys.filter(_.contains(line, offset))
    val innerExprLocation = resultMap.keys
      .filter(_.contains(line, offset))
      .find(exprLocation => exprLocations.forall(_.contains(exprLocation)))

    innerExprLocation
      .flatMap(loc => {
        // If the result has a locatable we can use that as the target, beware the order here matters due
        // to both inheritance and some objects supporting multiple Locatable traits
        resultMap(loc).result.locatable match {
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

  private object AbstractClassesAndInterface {
    def unapply(td: TypeDeclaration): Option[FullDeclaration] = {
      td match {
        case id: InterfaceDeclaration => Some(id)
        case cd: ClassDeclaration =>
          if (cd.modifiers.contains(ABSTRACT_MODIFIER)) Some(cd) else None
        case _ => None
      }
    }
  }
}
