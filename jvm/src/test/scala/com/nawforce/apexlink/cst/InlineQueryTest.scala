/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.pkgforce.names.{Name, TypeName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq

/** Tests for SOSL and SOQL Primaries */
class InlineQueryTest extends AnyFunSuite with Matchers with TestHelper {

  private def soql(query: String): SOQL = {
    primary(query).asInstanceOf[SOQL]
  }

  private def sosl(query: String): SOSL = {
    primary(query).asInstanceOf[SOSL]
  }

  private def primary(query: String): Primary = {
    val statements = typeDeclaration(
      s"public class Dummy {{ Object a = [$query]; }}"
    ).blocks.head match {
      case ApexInitializerBlock(_, block: LazyBlock, _) => block.statements()
      case _                                            => Seq()
    }
    statements.head
      .asInstanceOf[LocalVariableDeclarationStatement]
      .localVariableDeclaration
      .variableDeclarators
      .declarators
      .head
      .init
      .get
      .asInstanceOf[PrimaryExpression]
      .primary
  }

  test("SOQL simple query") {
    val soqlPrimary = soql("Select Id from Account")
    assert(!soqlPrimary.hasAggregateFunctions)
    assert(soqlPrimary.fromNames sameElements Array(TypeName(Name("Account"), Nil, None)))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL multiple from") {
    val soqlPrimary = soql("Select Id from Account, Contact")
    assert(!soqlPrimary.hasAggregateFunctions)
    assert(
      soqlPrimary.fromNames sameElements
        Array(TypeName(Name("Account"), Nil, None), TypeName(Name("Contact"), Nil, None))
    )
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL aggregate") {
    val soqlPrimary = soql("Select Name, Count(Id) from Account")
    assert(soqlPrimary.hasAggregateFunctions)
    assert(soqlPrimary.fromNames sameElements Array(TypeName(Name("Account"), Nil, None)))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL bound WHERE expression") {
    val soqlPrimary = soql("Select Id from Account WHERE Id in :Ids")
    assert(!soqlPrimary.hasAggregateFunctions)
    assert(soqlPrimary.fromNames sameElements Array(TypeName(Name("Account"), Nil, None)))
    soqlPrimary.boundExpressions should matchPattern {
      case ArraySeq(PrimaryExpression(IdPrimary(Id(Name("Ids"))))) =>
    }
  }

  test("SOQL multiple bound WHERE expressions") {
    val soqlPrimary = soql("Select Id from Account WHERE Id in :Ids AND Name like :Name+1")
    assert(!soqlPrimary.hasAggregateFunctions)
    assert(soqlPrimary.fromNames sameElements Array(TypeName(Name("Account"), Nil, None)))
    soqlPrimary.boundExpressions should matchPattern {
      case ArraySeq(
            PrimaryExpression(IdPrimary(Id(Name("Ids")))),
            BinaryExpression(
              PrimaryExpression(IdPrimary(Id(Name("Name")))),
              PrimaryExpression(LiteralPrimary(IntegerLiteral)),
              "+"
            )
          ) =>
    }
  }

  test("SOQL multiple bound LIMIT expressions") {
    val soqlPrimary = soql("Select Id from Account Limit :Limit")
    assert(!soqlPrimary.hasAggregateFunctions)
    assert(soqlPrimary.fromNames sameElements Array(TypeName(Name("Account"), Nil, None)))
    soqlPrimary.boundExpressions should matchPattern {
      case ArraySeq(PrimaryExpression(IdPrimary(Id(Name("Limit"))))) =>
    }
  }

  test("SOQL multiple FROM validate error") {
    typeDeclaration("public class Dummy {{ Object a = [Select Id from Account, Contact]; }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 33-66: Expecting SOQL to query only a single SObject, found 'Account, Contact'\n"
    )
  }

  test("SOQL unknown FROM validate error") {
    typeDeclaration("public class Dummy {{ Object a = [Select Id from Foo]; }}")
    assert(dummyIssues == "Missing: line 1 at 33-53: No type declaration found for 'Foo'\n")
  }

  test("SOSL simple query") {
    val soslPrimary = sosl("Find 'something' RETURNING Account")
    assert(soslPrimary.boundExpressions.isEmpty)
  }

  test("SOSL bound search") {
    val soslPrimary = sosl("Find :Text RETURNING Account")
    soslPrimary.boundExpressions should matchPattern {
      case ArraySeq(PrimaryExpression(IdPrimary(Id(Name("Text"))))) =>
    }
  }

  test("SOSL bound search and limit") {
    val soslPrimary = sosl("Find :Text RETURNING Account LIMIT :1+1")
    soslPrimary.boundExpressions should matchPattern {
      case ArraySeq(
            PrimaryExpression(IdPrimary(Id(Name("Text")))),
            BinaryExpression(
              PrimaryExpression(LiteralPrimary(IntegerLiteral)),
              PrimaryExpression(LiteralPrimary(IntegerLiteral)),
              "+"
            )
          ) =>
    }
  }

}
