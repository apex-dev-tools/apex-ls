/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.rpc

import com.nawforce.pkgforce.path.Location
import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler.{macroRW, ReadWriter => RW}

case class Rename(path: String, edits: Array[Location])

object Rename {
  implicit val rw: RW[Rename]           = macroRW
  implicit val rwLocation: RW[Location] = macroRW
}
