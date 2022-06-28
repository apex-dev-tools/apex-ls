package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.ValidationResult
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, FullDeclaration, TriggerDeclaration}
import com.nawforce.pkgforce.documents.{ApexClassDocument, ApexTriggerDocument, MetadataDocument}
import com.nawforce.pkgforce.path.{Location, PathLike}

trait SourceOps {
  this: OPM.PackageImpl =>
  def loadSourceAndType(
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
