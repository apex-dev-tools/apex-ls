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
package com.nawforce.pkgforce.path

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.{read, write}

class LocationSerializationTest extends AnyFunSuite {
  test("location retains its four-field serialization") {
    val location = Location(2, 3, 4, 5)
    val json     = ujson.read(write(location)).obj
    assert(json.keySet == Set("startLine", "startPosition", "endLine", "endPosition"))
    assert(read[Location](json.render()) == location)
  }
}
