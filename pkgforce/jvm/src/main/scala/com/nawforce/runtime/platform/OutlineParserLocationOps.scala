/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.platform

import com.financialforce.oparser.{Location => OPLocation}
import com.nawforce.pkgforce.path.{Location, PathLike, Positionable}

/** Utility methods to manipulate and map from Outline Parser Location to pkgforce Location */
object OutlineParserLocationOps {

  def toLocation(src: Option[OPLocation]): Location = {
    src match {
      case None    => Location.empty
      case Some(l) => toLocation(l)
    }
  }

  def toLocation(l: OPLocation): Location = {
    Location(l.startLine, l.startLineOffset, l.endLine, l.endLineOffset)
  }

  def stampLocation(positionable: Positionable, l: OPLocation, path: PathLike): Unit = {
    positionable.setLocation(path, l.startLine, l.startLineOffset, l.endLine, l.endLineOffset)
  }

  def extendLocation(location: OPLocation, startLineOffset: Int = 0): OPLocation = {
    location.copy(
      startLineOffset = location.startLineOffset + startLineOffset,
      startByteOffset = location.startByteOffset + startLineOffset
    )
  }
}
