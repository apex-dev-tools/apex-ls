/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.ValidationResult
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, FullDeclaration, TriggerDeclaration}
import com.nawforce.apexlink.types.core.{DependentType, TypeDeclaration}
import com.nawforce.pkgforce.documents.{ApexClassDocument, ApexTriggerDocument, MetadataDocument}
import com.nawforce.pkgforce.path.{Location, PathLike}

trait SourceOps {
  this: OPM.PackageImpl =>

  def loadFullSourceAndType(
    path: PathLike,
    content: Option[String]
  ): Option[(String, ApexFullDeclaration)] = {
    // We need source code no matter what
    val sourceOpt = loadSource(path, content).orElse(return None)

    // If we don't have new source we can assume the loaded type is current, but it could be a summary
    if (content.isEmpty) {
      loadTypeFromModule(path) match {
        case Some(fd: FullDeclaration)     => return Some((sourceOpt.get, fd))
        case Some(atd: TriggerDeclaration) => return Some((sourceOpt.get, atd))
        case _                             =>
      }
    }
    // No option but to load it as content is being provided
    loadRawType(path, sourceOpt.get)
  }

  def loadSource(path: PathLike, content: Option[String]): Option[String] = {
    content.orElse(path.read().toOption)
  }

  def loadRawType(path: PathLike, source: String): Option[(String, ApexFullDeclaration)] = {
    MetadataDocument(path) match {
      case Some(_: ApexTriggerDocument) => loadTrigger(path, source)._2.map(td => (source, td))
      case Some(_: ApexClassDocument)   => loadClass(path, source)._2.map(td => (source, td))
      case _                            => None
    }

  }

  def loadTypeFromModule(path: PathLike): Option[TypeDeclaration] = {
    MetadataDocument(path)
      .collect({
        case doc @ (_: ApexTriggerDocument | _: ApexClassDocument) =>
          orderedModules.view
            .flatMap(_.moduleType(doc.typeName(namespace)))
            .headOption
      })
      .flatten
  }

  /** Extract a location link from an expression at the passed location */
  def locateFromValidation(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): (Map[Location, ValidationResult], Option[Location]) = {
    val resultMap = td.getValidationMap(line, offset)

    // Find the inner-most expression containing location from those that do
    val exprLocations = resultMap.keys.filter(_.contains(line, offset))
    val innerExprLocation = resultMap.keys
      .filter(_.contains(line, offset))
      .find(exprLocation => exprLocations.forall(_.contains(exprLocation)))

    (resultMap, innerExprLocation)

  }
}
