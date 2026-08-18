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

class LocationTest extends AnyFunSuite {

  test("named factories retain constructor and sentinel compatibility") {
    assert(Location.point(2, 3) == Location(2, 3))
    assert(Location.span(2, 3, 4, 5) == Location(2, 3, 4, 5))
    assert(Location.wholeLine(2) == Location(2, 0, 2, Int.MaxValue))
    assert(Location.empty == Location(1))
    assert(Location.all == Location(1, 0, Int.MaxValue, 0))
  }

  test("named factories retain structural equality, hashing and four-field serialization") {
    val location = Location.span(2, 3, 4, 5)
    assert(location == Location(2, 3, 4, 5))
    assert(location.hashCode == Location(2, 3, 4, 5).hashCode)

    val json = ujson.read(write(location))
    assert(json.obj.keySet == Set("startLine", "startPosition", "endLine", "endPosition"))
    assert(read[Location](json) == location)
  }

  Seq(
    "LF"    -> ("first\nsecond\nthird", 2, "second"),
    "CRLF"  -> ("first\r\nsecond\r\nthird", 2, "second"),
    "CR"    -> ("first\rsecond\rthird", 2, "second"),
    "empty" -> ("first\n\nthird", 2, ""),
    "final" -> ("first\nfinal", 2, "final")
  ).foreach { case (name, (source, line, expected)) =>
    test(s"wholeLine clamps to $name line content") {
      assert(Location.extract(source, Location.wholeLine(line)) == expected)
    }
  }

  test("wholeLine clamps empty source and an empty final line") {
    assert(Location.extract("", Location.wholeLine(1)).isEmpty)
    assert(Location.extract("first\n", Location.wholeLine(2)).isEmpty)
  }

  test("span extraction is half-open and uses Unicode code-point columns") {
    val source = "zero\né🤦abc\nlast"
    assert(Location.extract(source, Location.span(2, 1, 2, 4)) == "🤦ab")
    assert(Location.extract(source, Location.span(1, 2, 3, 2)) == "ro\né🤦abc\nla")
    assert(Location.extract(source, Location.point(2, 2)).isEmpty)
    assert(Location.extract(source, Location.all) == source)
    assert(Location.extract("first\n", Location.all) == "first\n")
  }
}
