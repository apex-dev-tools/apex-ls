/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.rpc

import com.nawforce.pkgforce.path.Location
import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler.{read, write}
import org.scalatest.funsuite.AnyFunSuite

class LocationRPCTest extends AnyFunSuite {

  test("RPC locations retain exact four-field ranges") {
    val link = LocationLink(
      Location.point(1, 2),
      "/CustomLabels.labels",
      Location.span(3, 4, 8, 13),
      Location.wholeLine(5)
    )

    val json = ujson.read(write(link))
    Seq("origin", "target", "targetSelection").foreach { field =>
      assert(json(field).obj.keySet == Set("startLine", "startPosition", "endLine", "endPosition"))
    }
    assert(read[LocationLink](json.render()) == link)
  }
}
