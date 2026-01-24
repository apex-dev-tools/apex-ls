/*
 Copyright (c) 2023 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.rpc.HoverItem
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexConstructorLike,
  ApexFullDeclaration,
  ApexMethodLike
}
import com.nawforce.apexlink.types.synthetic.CustomConstructorDeclaration
import com.nawforce.pkgforce.path.{Locatable, Location, PathLike}

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
      val result = validation._1(loc).result
      result.locatable match {
        case Some(l: ApexMethodLike) =>
          Some(l, loc)
        case Some(l: ApexConstructorLike) =>
          Some(l, loc)
        case Some(l: ApexClassDeclaration) =>
          Some(l, loc)
        case Some(_: CustomConstructorDeclaration) =>
          result.declaration match {
            case Some(c: ApexClassDeclaration) => Some(c, loc)
            case _                             => None
          }
        case _ =>
          None
      }
    })
  }

  private def toHoverItem(l: Option[(Locatable, Location)]): HoverItem = {
    l match {
      case Some((td, loc)) => HoverItem(Some(td.toString), Some(loc))
      case _               => HoverItem(None, None)
    }
  }
}
