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
