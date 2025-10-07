/*
 Copyright (c) 2023 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.hover.ApexDocFormatter
import com.nawforce.apexlink.rpc.HoverItem
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexConstructorLike,
  ApexFullDeclaration,
  ApexMethodLike
}
import com.nawforce.pkgforce.path.{Locatable, Location, PathLike}

trait HoverProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getHover(path: PathLike, line: Int, offset: Int, content: Option[String]): HoverItem = {
    val sourceAndType = loadFullSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return HoverItem(None, None)

    val (source, declaration) = sourceAndType.get

    toHoverItem(getFromValidationLocatable(declaration, line, offset), source)
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

  private def toHoverItem(l: Option[(Locatable, Location)], source: String): HoverItem = {
    l match {
      case Some((td, loc)) =>
        val signature = td.toString
        val declarationPath     = td.location.path
        val declarationLocation = td.location.location
        val docSource           = loadSource(declarationPath, None).getOrElse(source)
        val doc                 = ApexDocFormatter.format(docSource, declarationLocation)
        val content = doc match {
          case Some(markdown) if markdown.nonEmpty => s"$signature\n$markdown"
          case _                                    => signature
        }
        HoverItem(Some(content), Some(loc))
      case _ =>
        HoverItem(None, None)
    }
  }
}
