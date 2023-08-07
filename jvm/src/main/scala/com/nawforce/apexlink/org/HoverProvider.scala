/*
 Copyright (c) 2023 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.ApexMethodDeclaration
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.HoverItem
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexConstructorLike,
  ApexFullDeclaration,
  ApexMethodLike
}
import com.nawforce.pkgforce.path.{IdLocatable, Locatable, Location, PathLike, UnsafeLocatable}

trait HoverProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getHover(path: PathLike, line: Int, offset: Int, content: Option[String]): HoverItem = {
    val sourceAndType = loadFullSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return HoverItem(None, None)

    toHoverItem(getFromValidationLocatable(sourceAndType.get._2, line, offset))
  }

  private def getFromValidationLocatable(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): Option[(Locatable, Location)] = {
    val validation = locateFromValidation(td, line, offset)

    validation._2.flatMap(loc => {
      validation._1(loc).result.locatable match {
        case Some(l: ApexMethodLike) =>
          Some(l, loc)
        case Some(l: ApexConstructorLike) =>
          Some(l, loc)
        case Some(l: ApexClassDeclaration) =>
          Some(l, loc)
        case _ =>
          None
      }
    })
  }

  private def toHoverItem(l: Option[(Locatable, Location)]): HoverItem = {
    l match {
      case Some(l) => HoverItem(Some(l._1.toString), Some(l._2))
      case _       => HoverItem(None, None)
    }
  }
}
