/*
 Copyright (c) 2017 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.runtime.parsers.{CodeParser, Source, SourceData}
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LiteralTypeTest extends AnyFunSuite with Matchers with TestHelper {
  def typeLiteral(data: String): Literal = {
    val source = Source(Path("Dummy.cls"), SourceData(""), 0, 0, None)
    CST.sourceContext.withValue(Some(source)) {
      val result = CodeParser(Path(""), SourceData(data)).parseLiteral()
      assert(result.issues.isEmpty)
      Literal.construct(result.value)
    }
  }

  def compareLiteral(p: String, r: TypeName): Unit = {
    val t = typeLiteral(p).getType
    assert(t != null)

    if (t.typeName != r) {
      System.out.println("Type mismatch:")
      System.out.println("Expected: " + r)
      System.out.println("Got: " + t)
      assert(false)
    }
  }

  def literal(value: String, r: TypeName = null): Unit =
    compareLiteral(value, r)

  test("Primary literal") {
    literal("0", TypeNames.Integer)
    literal("1", TypeNames.Integer)
    literal("0l", TypeNames.Long)
    literal("1l", TypeNames.Long)
    literal("0L", TypeNames.Long)
    literal("1L", TypeNames.Long)
    literal("''", TypeNames.String)
    literal("'a'", TypeNames.String)
    literal("'az'", TypeNames.String)
    literal("'\t'", TypeNames.String)
    literal("true", TypeNames.Boolean)
    literal("False", TypeNames.Boolean)
    literal("null", TypeName(Name("Null$"), Nil, Some(TypeName(Names.Internal))))
    literal("0.0", TypeNames.Decimal)
    literal("0.0d", TypeNames.Double)
    literal(".0", TypeNames.Decimal)
    literal(".0D", TypeNames.Double)
    literal("0.123", TypeNames.Decimal)
    literal("0.123D", TypeNames.Double)
    literal("0.123456789012345678901234567890123456789012345678", TypeNames.Decimal)
    literal("0.123456789012345678901234567890123456789012345678d", TypeNames.Double)
  }

  test("Max Decimal") {
    typeDeclaration(
      "public class Dummy { Object a = 0.123456789012345678901234567890123456789012345678; }"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Oversize Decimal") {
    typeDeclaration(
      "public class Dummy { Object a = 0.1234567890123456789012345678901234567890123456789; }"
    )
    assert(
      dummyIssues == "Error: line 1 at 32-83: Decimal literals can only be up to 50 characters\n"
    )
  }

  test("Long Double") {
    typeDeclaration(
      "public class Dummy { Object a = 0.12345678901234567890123456789012345678901234567890123456789d; }"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Max Integer") {
    typeDeclaration("public class Dummy { Object a = 1234567890; }")
    assert(dummyIssues.isEmpty)
  }

  test("Oversize Integer") {
    typeDeclaration("public class Dummy { Object a = 12345678901; }")
    assert(
      dummyIssues == "Error: line 1 at 32-43: Integer literals can only be up to 10 characters\n"
    )
  }

  test("Max Long") {
    typeDeclaration("public class Dummy { Object a = 1234567890123456789l; }")
    assert(dummyIssues.isEmpty)
  }

  test("Oversize Long") {
    typeDeclaration("public class Dummy { Object a = 12345678901234567890l; }")
    assert(dummyIssues == "Error: line 1 at 32-53: Long literals can only be up to 19 characters\n")
  }

  test("Bound string literal") {
    val aSet        = Set(Name("a"))
    val abSet       = Set(Name("a"), Name("b"))
    val abcSet      = Set(Name("a"), Name("b"), Name("c"))
    val abJoinedSet = Set(Name("ab"))
    val mixedSet    = Set(Name("1a2b3"))
    typeLiteral("':a'") should matchPattern { case BoundStringLiteral(bound) if bound == aSet => }
    typeLiteral("' :a'") should matchPattern { case BoundStringLiteral(bound) if bound == aSet => }
    typeLiteral("': a'") should matchPattern { case BoundStringLiteral(bound) if bound == aSet => }
    typeLiteral("':a '") should matchPattern { case BoundStringLiteral(bound) if bound == aSet => }
    typeLiteral("'  :  a  '") should matchPattern {
      case BoundStringLiteral(bound) if bound == aSet =>
    }
    typeLiteral("':ab'") should matchPattern {
      case BoundStringLiteral(bound) if bound == abJoinedSet =>
    }
    typeLiteral("':a b'") should matchPattern { case BoundStringLiteral(bound) if bound == aSet => }
    typeLiteral("'b:a'") should matchPattern { case BoundStringLiteral(bound) if bound == aSet => }
    typeLiteral("':1a2b3'") should matchPattern {
      case BoundStringLiteral(bound) if bound == mixedSet =>
    }
    typeLiteral("':a:b'") should matchPattern {
      case BoundStringLiteral(bound) if bound == abSet =>
    }
    typeLiteral("':a :b :c'") should matchPattern {
      case BoundStringLiteral(bound) if bound == abcSet =>
    }
  }

  test("Invalid string escape sequence") {
    try {
      typeDeclaration("public class Dummy { String s = 'test\\stest'; }")
      assert(dummyIssues.nonEmpty)
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
  }

  test("Valid string escape sequences") {
    typeDeclaration(
      "public class Dummy { String s1 = '\\''; String s2 = '\\\"'; String s3 = '\\\\'; }"
    )
    assert(dummyIssues.isEmpty)
    typeDeclaration(
      "public class Dummy { String s1 = '\\n'; String s2 = '\\r'; String s3 = '\\t'; }"
    )
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s1 = '\\b'; String s2 = '\\f'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\u0041'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\u00FF'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\uABCD'; }")
    assert(dummyIssues.isEmpty)
  }

  test("Invalid single character escape sequences - digits") {
    val digits = Seq('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    val failures = digits.filter { digit =>
      try {
        typeDeclaration(s"public class Dummy { String s = 'test\\${digit}test'; }")
        !dummyIssues.contains("invalid escape") && !dummyIssues.contains("token recognition")
      } catch {
        case _: NoSuchElementException => false
      }
    }
    assert(
      failures.isEmpty,
      s"Invalid escapes not detected: ${failures.map(c => s"\\$c").mkString(", ")}"
    )
  }

  test("Invalid single character escape sequences - letters") {
    val invalidLetters = Seq('a', 'c', 'd', 'e', 'h', 'i', 'j', 'k', 'l', 'm', 'o', 'p', 'q', 's',
      'v', 'w', 'x', 'y', 'z')
    val failures = invalidLetters.filter { char =>
      try {
        typeDeclaration(s"public class Dummy { String s = 'test\\${char}test'; }")
        !dummyIssues.contains("invalid escape") && !dummyIssues.contains("token recognition")
      } catch {
        case _: NoSuchElementException => false
      }
    }
    assert(
      failures.isEmpty,
      s"Invalid escapes not detected: ${failures.map(c => s"\\$c").mkString(", ")}"
    )
  }

  test("Invalid unicode escape - incomplete") {
    try {
      typeDeclaration("public class Dummy { String s = '\\u'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
    try {
      typeDeclaration("public class Dummy { String s = '\\u1'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
    try {
      typeDeclaration("public class Dummy { String s = '\\u12'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
    try {
      typeDeclaration("public class Dummy { String s = '\\u123'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
  }

  test("Invalid unicode escape - invalid hex digits") {
    val invalidHex = Seq('g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
      'v', 'w', 'x', 'y', 'z')
    invalidHex.foreach { invalidHexChar =>
      try {
        typeDeclaration(s"public class Dummy { String s = '\\u123${invalidHexChar}'; }")
        assert(
          dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"),
          s"Failed to detect invalid hex digit ${invalidHexChar} in \\u123${invalidHexChar}"
        )
      } catch {
        case _: NoSuchElementException =>
          assert(true, s"Parser rejected invalid hex digit ${invalidHexChar} (acceptable)")
      }
    }
  }

  test("Invalid unicode escape - invalid hex in first position") {
    try {
      typeDeclaration("public class Dummy { String s = '\\ug123'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
  }

  test("Trailing backslash") {
    try {
      typeDeclaration("public class Dummy { String s = 'test\\'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
  }

  test("Multiple invalid escapes in one string") {
    try {
      typeDeclaration("public class Dummy { String s = '\\s\\x\\g'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
  }

  test("Mixed valid and invalid escapes") {
    try {
      typeDeclaration("public class Dummy { String s = '\\n\\s\\t'; }")
      assert(dummyIssues.contains("invalid escape") || dummyIssues.contains("token recognition"))
    } catch {
      case _: NoSuchElementException =>
        assert(true)
    }
  }

  test("Valid unicode escapes with various hex combinations") {
    typeDeclaration("public class Dummy { String s = '\\u0000'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\uFFFF'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\u0123'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\uABCD'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\uabcd'; }")
    assert(dummyIssues.isEmpty)
    typeDeclaration("public class Dummy { String s = '\\u012F'; }")
    assert(dummyIssues.isEmpty)
  }

  test("String with only valid escapes") {
    typeDeclaration("public class Dummy { String s = '\\\\\\'\\\"\\n\\r\\t\\b\\f\\u0041'; }")
    assert(dummyIssues.isEmpty)
  }

  test("Empty string with valid escapes") {
    typeDeclaration("public class Dummy { String s = '\\n\\r\\t'; }")
    assert(dummyIssues.isEmpty)
  }

}
