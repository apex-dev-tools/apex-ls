/*
 * Copyright (c) 2026 Certinia Inc. All rights reserved.
 */
package com.nawforce.pkgforce.names

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import _root_.org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Property-based tests for Identifier validation.
  *
  * These tests verify that the identifier validation logic correctly handles all valid and invalid
  * identifier patterns, using generated inputs rather than hand-picked examples.
  */
class IdentifierPropertyTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  // Generator for valid identifier characters (alphanumeric + underscore)
  val validChar: Gen[Char] = Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('_'))

  // Generator for valid first characters (must be a letter)
  val validFirstChar: Gen[Char] = Gen.alphaChar

  // Generator for valid middle sections (no double underscores)
  val validMiddleSection: Gen[String] = Gen.listOf(validChar).map { chars =>
    chars.mkString.replaceAll("_+", "_") // collapse multiple underscores to single
  }

  // Generator for valid simple identifiers
  val validIdentifier: Gen[String] = for {
    first  <- validFirstChar
    middle <- validMiddleSection
  } yield {
    val raw = s"$first$middle"
    // Clean up: remove trailing underscore if present
    if (raw.endsWith("_")) raw.dropRight(1) else raw
  }

  // Generator for identifiers starting with underscore (invalid)
  val startsWithUnderscore: Gen[String] = for {
    rest <- Gen.alphaNumStr.suchThat(_.nonEmpty)
  } yield "_" + rest

  // Generator for identifiers ending with underscore (invalid)
  val endsWithUnderscore: Gen[String] = for {
    start <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield start + "_"

  // Generator for identifiers starting with digit (invalid)
  val startsWithDigit: Gen[String] = for {
    digit <- Gen.numChar
    rest  <- Gen.alphaNumStr
  } yield digit.toString + rest

  // Generator for identifiers containing double underscore (invalid)
  val containsDoubleUnderscore: Gen[String] = for {
    before <- Gen.alphaStr.suchThat(_.nonEmpty)
    after  <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield before + "__" + after

  // Generator for identifiers containing illegal characters (invalid)
  val illegalCharacter: Gen[Char] = Gen.oneOf('$', '@', '#', '!', '%', '^', '&', '*', '-', '+', '=')

  val containsIllegalChar: Gen[String] = for {
    before  <- Gen.alphaStr
    illegal <- illegalCharacter
    after   <- Gen.alphaStr
  } yield before + illegal + after

  // Property: Valid identifiers are accepted
  test("valid identifiers are accepted") {
    forAll(validIdentifier) { id =>
      whenever(id.nonEmpty && !id.contains("__")) {
        val result = Identifier.isLegalIdentifier(Name(id))
        assert(result.isEmpty, s"Expected valid but got error for '$id': ${result.getOrElse("")}")
      }
    }
  }

  // Property: Identifiers starting with underscore are rejected
  test("identifiers starting with underscore are rejected") {
    forAll(startsWithUnderscore) { id =>
      val result = Identifier.isLegalIdentifier(Name(id))
      assert(result.isDefined, s"Expected rejection for '$id'")
      assert(result.get.contains("_"), s"Error should mention underscore for '$id'")
    }
  }

  // Property: Identifiers ending with underscore are rejected
  test("identifiers ending with underscore are rejected") {
    forAll(endsWithUnderscore) { id =>
      val result = Identifier.isLegalIdentifier(Name(id))
      assert(result.isDefined, s"Expected rejection for '$id'")
      assert(result.get.contains("_"), s"Error should mention underscore for '$id'")
    }
  }

  // Property: Identifiers starting with digit are rejected
  test("identifiers starting with digit are rejected") {
    forAll(startsWithDigit) { id =>
      whenever(id.nonEmpty) {
        val result = Identifier.isLegalIdentifier(Name(id))
        assert(result.isDefined, s"Expected rejection for '$id'")
        assert(result.get.contains("digit"), s"Error should mention digit for '$id'")
      }
    }
  }

  // Property: Identifiers containing double underscore are rejected
  test("identifiers containing double underscore are rejected") {
    forAll(containsDoubleUnderscore) { id =>
      val result = Identifier.isLegalIdentifier(Name(id))
      assert(result.isDefined, s"Expected rejection for '$id'")
      assert(result.get.contains("__"), s"Error should mention double underscore for '$id'")
    }
  }

  // Property: Identifiers containing illegal characters are rejected
  test("identifiers containing illegal characters are rejected") {
    forAll(containsIllegalChar) { id =>
      whenever(id.nonEmpty) {
        val result = Identifier.isLegalIdentifier(Name(id))
        assert(result.isDefined, s"Expected rejection for '$id'")
      }
    }
  }

  // Property: Single letter identifiers are valid
  test("single letter identifiers are valid") {
    forAll(Gen.alphaChar) { c =>
      val result = Identifier.isLegalIdentifier(Name(c.toString))
      assert(result.isEmpty, s"Expected valid but got error for '$c': ${result.getOrElse("")}")
    }
  }

  // Property: Single digit identifiers are invalid
  test("single digit identifiers are invalid") {
    forAll(Gen.numChar) { c =>
      val result = Identifier.isLegalIdentifier(Name(c.toString))
      assert(result.isDefined, s"Expected rejection for '$c'")
    }
  }
}
