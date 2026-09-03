/*
 Copyright (c) 2023 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.rpc.HoverItem
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexConstructorLike,
  ApexFieldLike,
  ApexFullDeclaration,
  ApexMethodLike,
  TriggerDeclaration
}
import com.nawforce.apexlink.types.synthetic.CustomConstructorDeclaration
import com.nawforce.pkgforce.path.{Locatable, Location, PathLike}

trait HoverProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getHover(path: PathLike, line: Int, offset: Int, content: Option[String]): HoverItem = {
    val sourceAndType = loadFullSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return HoverItem(None, None)

    val td = sourceAndType.get._2
    toHoverItem(
      getFromValidationLocatable(td, line, offset).orElse(getFromDeclaration(td, line, offset))
    )
  }

  private def getFromValidationLocatable(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): Option[(Locatable, Location)] = {
    val validation = locateFromValidation(td, line, offset)

    validation._2.flatMap(loc => {
      val result = validation._1(loc).result
      result.locatable
        .flatMap {
          case _: CustomConstructorDeclaration =>
            result.declaration.collect { case c: ApexClassDeclaration => c }
          case other => hoverTarget(other)
        }
        .map(target => (target, loc))
    })
  }

  /** Declaration identifiers are not expressions so have no validation result, resolve them from
    * the declared members instead so a declaration site hovers the same as a reference to it.
    */
  private def getFromDeclaration(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): Option[(Locatable, Location)] = {
    td match {
      case trigger: TriggerDeclaration =>
        Some(trigger).filter(_.idLocation.contains(line, offset)).map(t => (t, t.idLocation))
      case _ =>
        td.findReferenceableFromLocation(line, offset)
          .flatMap(ref => hoverTarget(ref).map(target => (target, ref.idLocation)))
    }
  }

  private def hoverTarget(locatable: Locatable): Option[Locatable] = {
    locatable match {
      case l: ApexMethodLike       => Some(l)
      case l: ApexConstructorLike  => Some(l)
      case l: ApexClassDeclaration => Some(l)
      case l: ApexFieldLike        => Some(l)
      case _                       => None
    }
  }

  private def toHoverItem(l: Option[(Locatable, Location)]): HoverItem = {
    l match {
      case Some((td, loc)) =>
        HoverItem(Some(s"```apex\n${header(td)}\n```"), Some(loc), Some("markdown"))
      case _ => HoverItem(None, None)
    }
  }

  private def header(declaration: Locatable): String = {
    declaration match {
      case constructor: ApexConstructorLike => constructor.header
      case other                            => other.toString
    }
  }
}
