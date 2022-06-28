package com.nawforce.apexlink.org

import com.nawforce.apexlink.rpc.SimpleLocation
import com.nawforce.apexlink.types.core.Dependent
import com.nawforce.pkgforce.path.{IdLocatable, PathLike}

trait Referencable {
  this: Dependent =>
  def findReferences(): Array[SimpleLocation] = {
    getDependencyHolders
      .collect({ case loc: IdLocatable => loc })
      .map(loc => SimpleLocation(loc.location.path.toString, loc.idLocation))
      .toArray
  }
}

trait ReferenceProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getReferences(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[SimpleLocation] = {
    val sourceAndType = loadSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return Array.empty

    val validation = locateFromValidation(sourceAndType.get._2, line, offset)

    validation._2
      .flatMap(loc => {
        validation
          ._1(loc)
          .result
          .locatable
          .collect({ case ref: Referencable => ref })
          .map(_.findReferences())
      })
      .getOrElse(Array.empty)

  }

}
