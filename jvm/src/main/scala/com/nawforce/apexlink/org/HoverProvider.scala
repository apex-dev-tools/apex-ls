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
      case Some((td, loc)) =>
        HoverItem(Some(s"```apex\n${signature(td)}\n```"), Some(loc), Some("markdown"))
      case _ => HoverItem(None, None)
    }
  }

  private def signature(declaration: Locatable): String = {
    declaration match {
      case constructor: ApexConstructorLike =>
        val modifiers = constructor.modifiers.map(_.toString).mkString(" ")
        val prefix    = if (modifiers.nonEmpty) s"$modifiers " else ""
        val parameters = constructor.parameters
          .map(parameter => s"${parameter.typeName} ${parameter.name}")
          .mkString(", ")
        s"$prefix${constructor.thisTypeId.typeName.name}($parameters)"
      case other => other.toString
    }
  }
}
