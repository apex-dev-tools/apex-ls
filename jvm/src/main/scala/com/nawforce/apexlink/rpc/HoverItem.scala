/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.rpc

import com.nawforce.pkgforce.path.Location
import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler.{macroRW, ReadWriter => RW}

case class HoverItem(content: Option[String], location: Option[Location])

object HoverItem {
  implicit val rw: RW[HoverItem]        = macroRW
  implicit val rwLocation: RW[Location] = macroRW
}
