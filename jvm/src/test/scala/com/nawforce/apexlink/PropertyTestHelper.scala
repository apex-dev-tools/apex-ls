/*
 * Copyright (c) 2026 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink

/** Base trait for property-based tests in apex-ls.
  *
  * Combines ScalaCheck property checking with the existing TestHelper utilities for Apex parsing
  * and validation.
  *
  * Usage:
  * {{{
  * class MyPropertyTest extends AnyFunSuite with PropertyTestHelper {
  *   test("property holds for all inputs") {
  *     forAll { (s: String) =>
  *       // property assertion
  *     }
  *   }
  * }
  * }}}
  */
trait PropertyTestHelper
    extends _root_.org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
    with TestHelper
