/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.sfdx

import org.scalatest.funsuite.AnyFunSuite

class ForceIgnoreVersionTest extends AnyFunSuite {

  test("fromString valid values") {
    assert(ForceIgnoreVersion.fromString("v1") == Some(ForceIgnoreVersion.V1))
    assert(ForceIgnoreVersion.fromString("v2") == Some(ForceIgnoreVersion.V2))
  }

  test("fromString invalid values") {
    assert(ForceIgnoreVersion.fromString("v3") == None)
    assert(ForceIgnoreVersion.fromString("invalid") == None)
    assert(ForceIgnoreVersion.fromString("") == None)
    assert(ForceIgnoreVersion.fromString("V1") == None) // Case sensitive
  }

  test("value property") {
    assert(ForceIgnoreVersion.V1.value == "v1")
    assert(ForceIgnoreVersion.V2.value == "v2")
  }

  test("default value") {
    assert(ForceIgnoreVersion.default == ForceIgnoreVersion.V2)
  }

  test("all versions") {
    assert(ForceIgnoreVersion.all == Seq(ForceIgnoreVersion.V1, ForceIgnoreVersion.V2))
  }

  test("valid values") {
    assert(ForceIgnoreVersion.validValues == Seq("v1", "v2"))
  }
}
