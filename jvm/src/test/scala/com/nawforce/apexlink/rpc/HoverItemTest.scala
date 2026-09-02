/*
 * Copyright (c) 2026 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.rpc

import org.scalatest.funsuite.AnyFunSuite

class HoverItemTest extends AnyFunSuite {
  test("Two argument construction defaults content kind") {
    val hoverItem = HoverItem(None, None)

    assert(hoverItem.kind.isEmpty)
  }
}
