package com.nawforce.apexlink.rpc

import com.nawforce.pkgforce.path.Location
import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler.{macroRW, ReadWriter => RW}

case class SimpleLocation(targetPath: String, range: Location)

object SimpleLocation {
  implicit val rw: RW[LocationLink]     = macroRW
  implicit val rwLocation: RW[Location] = macroRW
}
