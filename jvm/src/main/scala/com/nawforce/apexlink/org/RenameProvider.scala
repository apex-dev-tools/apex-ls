/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.{ApexFieldDeclaration, Id, VariableDeclarator}
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.Rename
import com.nawforce.apexlink.types.apex.FullDeclaration
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.{Location, PathLike}

trait RenameProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getRenameLocations(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[Rename] = {

    val sourceAndType = loadFullSourceAndType(path, content)

    val searchSymbolLocation = sourceAndType match {
      case Some(source) =>
        source._1
          .extractSymbolLocation(() => new IdentifierAndMethodLimiter, line, offset)

      case None => return Array(Rename(path.toString, Array.empty))
    }

    searchSymbolLocation match {
      case Some(location) => {
        val editLocations = getVarLocations(
          sourceAndType.get._2.asInstanceOf[FullDeclaration],
          line,
          offset,
          location
        )
        Array(Rename(path.toString, editLocations))
      }
      case None => Array(Rename(path.toString, Array.empty))
    }

  }

  private def getVarLocations(
    td: FullDeclaration,
    line: Int,
    offset: Int,
    searchSymbolLocation: Location
  ): Array[Location] = {
    val validationMap = td.getValidationMap(line, offset)

    // start by giving searchSymbolLocation otherwise it is missed when renaming 1 unused variable.
    var locations = Set(searchSymbolLocation)

    var requiresClassValidation = true

    val symbolDeclarationOption = {
      td.getValidationMapForMethodDeclarations.flatten.find(x => {
        if (
          x._1.startPosition == searchSymbolLocation.startPosition && x._1.startLine == searchSymbolLocation.startLine
        ) {
          true
        } else {
          x._2.result.locatable match {
            case Some(l: ApexFieldDeclaration) =>
              l.idLocation.startLine == searchSymbolLocation.startLine && l.idLocation.startPosition == searchSymbolLocation.startPosition
            case Some(l) =>
              l.location.location.startLine == searchSymbolLocation.startLine && l.location.location.startPosition == searchSymbolLocation.startPosition
            case _ =>
              false
          }
        }
      })
    }

    val symbolDeclaration = symbolDeclarationOption.getOrElse(return locations.toArray)

    validationMap.foreach(x => {
      x._2.result.locatable collect {
        case l: Id =>
          if (l == symbolDeclaration._2.result.locatable.get) {
            locations = locations + x._1 + l.location.location

            if (requiresClassValidation) {
              x._2.vars.foreach(scopeVarDefinition => {
                if (scopeVarDefinition.contains(Name(symbolDeclaration.toString))) {
                  requiresClassValidation = false
                }
              })
            }
          }
        case l: VariableDeclarator =>
          if (l == symbolDeclaration._2.result.locatable.get) {
            locations = locations + x._1 + l.id.location.location

            if (requiresClassValidation) {
              x._2.vars.foreach(scopeVarDefinition => {
                if (scopeVarDefinition.contains(Name(symbolDeclaration.toString))) {
                  requiresClassValidation = false
                }
              })
            }
          }
      }
    })

    if (requiresClassValidation) {
      td.getValidationMapForMethodDeclarations.flatten.foreach(x => {
        Some(x._2.result.locatable) collect {
          case Some(l: Id) =>
            if (l == symbolDeclaration._2.result.locatable.get) {
              locations = locations + x._1 + l.location.location
            }
          case Some(l: ApexFieldDeclaration) =>
            if (l == symbolDeclaration._2.result.locatable.get) {
              locations = locations + x._1 + l.idLocation
            }
        }
      })
    }

    locations.toArray
  }
}
