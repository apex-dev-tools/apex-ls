/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.OutlineParserSingleThreaded
import com.nawforce.pkgforce.path.{Location, PathLike, Positionable}
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.parsers.{Source, SourceData}
import org.antlr.v4.runtime.{CommonToken, ParserRuleContext}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BlockTest extends AnyFunSuite with Matchers with TestHelper {

  test("Outline parser outer block location") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
        |   {
        |     System.debug('');
        |   }
        |}
        |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root, Some(OutlineParserSingleThreaded.shortName))
      val td = unmanagedClass("Dummy").get
      td.blocks.head.asInstanceOf[ApexInitializerBlock].block should matchPattern {
        case blk: OuterBlock if blk.location.location == Location(2, 3, 4, 4) =>
      }
    }
  }

  test("ANTLR parser outer block location") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
            |   {
            |     System.debug('');
            |   }
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      val td = unmanagedClass("Dummy").get
      td.blocks.head.asInstanceOf[ApexInitializerBlock].block should matchPattern {
        case blk: OuterBlock if blk.location.location == Location(2, 3, 4, 4) =>
      }
    }
  }

  test("Outline parser inner block location") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {{
            |   {
            |     System.debug('');
            |   }
            |}}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root, Some(OutlineParserSingleThreaded.shortName))
      val td = unmanagedClass("Dummy").get
      td.blocks.head
        .asInstanceOf[ApexInitializerBlock]
        .block
        .statements()
        .head
        .asInstanceOf[Block] should matchPattern {
        case blk: StatementBlock if blk.location.location == Location(2, 3, 4, 4) =>
      }
    }
  }

  test("ANTLR parser inner block location") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {{
            |   {
            |     System.debug('');
            |   }
            |}}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      val td = unmanagedClass("Dummy").get
      td.blocks.head
        .asInstanceOf[ApexInitializerBlock]
        .block
        .statements()
        .head
        .asInstanceOf[Block] should matchPattern {
        case blk: StatementBlock if blk.location.location == Location(2, 3, 4, 4) =>
      }
    }
  }

  test("Method body parse failure does not cascade to 'Expected return statement'") {
    typeDeclaration("""public class Dummy {
        |  public String getFields() {
        |    String a = '\s';
        |    return a;
        |  }
        |}""".stripMargin)
    val issues = dummyIssues
    // Phase 1: spurious control-flow error from partial AST is suppressed.
    assert(!issues.contains("Expected return statement"))
    assert(!issues.contains("Code path does not return"))
    assert(!issues.contains("Method does not return"))
    // The lexer diagnostic is still reported.
    assert(issues.contains("Invalid escape sequence '\\s' in string"))
  }

  test("Successful method body parse still verifies control flow") {
    // Regression guard for Phase 1: a method that legitimately falls off the end
    // (without a syntax error) must still be flagged.
    typeDeclaration("""public class Dummy {
        |  public String getFields() {
        |  }
        |}""".stripMargin)
    assert(dummyIssues.contains("Method does not return a value"))
  }

  test("OuterBlock.statementsOrErrors returns Left on parse failure") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
            |  public void m() {
            |    String a = '\s';
            |  }
            |}""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      val td = unmanagedClass("Dummy").get
      val block =
        td.methods.find(_.name.value == "m").get.asInstanceOf[ApexMethodDeclaration].block.get
      block.statementsOrErrors() match {
        case Left(ParseErrors(issues)) =>
          assert(issues.nonEmpty)
          assert(issues.exists(_.diagnostic.message.contains("Invalid escape sequence '\\s'")))
        case Right(_) =>
          fail("expected Left(ParseErrors) for a method body with a syntax error")
      }
    }
  }

  test("OuterBlock.statementsOrErrors returns Right when parse succeeds") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
            |  public void m() {
            |    Integer x = 1;
            |  }
            |}""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
      val td = unmanagedClass("Dummy").get
      val block =
        td.methods.find(_.name.value == "m").get.asInstanceOf[ApexMethodDeclaration].block.get
      block.statementsOrErrors() match {
        case Right(stmts) => assert(stmts.size == 1)
        case Left(_)      => fail("expected Right for a well-formed method body")
      }
    }
  }

  test("Cascading lexer recovery errors on same line are collapsed") {
    typeDeclaration("""public class Dummy {
        |  public void m() {
        |    String a = '\s';
        |  }
        |}""".stripMargin)
    val escapeErrors = dummyIssues
      .split("\n")
      .count(_.contains("Invalid escape sequence"))
    // Without the cluster suppression, the lexer reports the same escape error twice
    // on this line as recovery re-enters the string.
    assert(escapeErrors == 1, s"expected a single escape error, got:\n${dummyIssues}")
  }

  test("Parser context without stop location does not crash location stamping") {
    FileSystemHelper.run(Map("Dummy.cls" -> "")) { root: PathLike =>
      val file    = root.join("Dummy.cls")
      val source  = new Source(file, SourceData(""), 0, 0, None)
      val context = new ParserRuleContext()
      val start   = new CommonToken(0, "bad")
      start.setLine(3)
      start.setCharPositionInLine(4)
      context.start = start

      val positioned = new Positionable
      source.stampLocation(positioned, context)

      assert(positioned.location.path == file)
      assert(positioned.location.location == Location(3, 4, 3, 7))
    }
  }

  test("Merge conflict text in method body does not crash") {
    typeDeclaration("""public class Dummy {
        |  public void m() {
        |<<<<<<< HEAD
        |    Integer a = 1;
        |=======
        |    Integer a = 2;
        |>>>>>>> branch
        |  }
        |}""".stripMargin)
    assert(dummyIssues.nonEmpty)
  }

}
