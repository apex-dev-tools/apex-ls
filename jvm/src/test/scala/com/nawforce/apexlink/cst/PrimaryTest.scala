/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.pkgforce.names.{DotName, Name, TypeName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq

/** Tests for Primaries */
class PrimaryTest extends AnyFunSuite with Matchers with TestHelper {

  private def primaryOf[T](expr: String): T = {
    primary(expr).asInstanceOf[T]
  }

  private def primary(expr: String): Primary = {
    val statements = typeDeclaration(
      s"public class Dummy {{ Object a = $expr; }}"
    ).blocks.head match {
      case ApexInitializerBlock(_, block: Block, _) => block.statements()
      case _                                        => Seq()
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
    val soqlPrimary = primaryOf[SOQL]("[Select Id from Account]")
    assert(soqlPrimary.queryResultType == LIST_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL multiple from") {
    val soqlPrimary = primaryOf[SOQL]("[Select Id from Account, Contact]")
    assert(soqlPrimary.queryResultType == LIST_RESULT_QUERY)
    assert(
      soqlPrimary.fromNames sameElements
        Array(DotName(Name("Account")), DotName(Name("Contact")))
    )
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL simple count") {
    val soqlPrimary = primaryOf[SOQL]("[Select Count() from Account]")
    assert(soqlPrimary.queryResultType == COUNT_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL count in aggregate") {
    val soqlPrimary = primaryOf[SOQL]("[Select Name, Count() from Account]")
    assert(soqlPrimary.queryResultType == AGGREGATE_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL aggregate") {
    val soqlPrimary = primaryOf[SOQL]("[Select Name, Count(Id) from Account]")
    assert(soqlPrimary.queryResultType == AGGREGATE_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL GROUP BY aggregate") {
    val soqlPrimary =
      primaryOf[SOQL]("[Select Name from Account GROUP BY Name]")
    assert(soqlPrimary.queryResultType == AGGREGATE_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    assert(soqlPrimary.boundExpressions.isEmpty)
  }

  test("SOQL bound WHERE expression") {
    val soqlPrimary = primaryOf[SOQL]("[Select Id from Account WHERE Id in :Ids]")
    assert(soqlPrimary.queryResultType == LIST_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    soqlPrimary.boundExpressions should matchPattern {
      case ArraySeq(PrimaryExpression(IdPrimary(Id(Name("Ids"))))) =>
    }
  }

  test("SOQL multiple bound WHERE expressions") {
    val soqlPrimary =
      primaryOf[SOQL]("[Select Id from Account WHERE Id in :Ids AND Name like :Name+1]")
    assert(soqlPrimary.queryResultType == LIST_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
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
    val soqlPrimary = primaryOf[SOQL]("[Select Id from Account Limit :Limit]")
    assert(soqlPrimary.queryResultType == LIST_RESULT_QUERY)
    assert(soqlPrimary.fromNames sameElements Array(DotName(Name("Account"))))
    soqlPrimary.boundExpressions should matchPattern {
      case ArraySeq(PrimaryExpression(IdPrimary(Id(Name("Limit"))))) =>
    }
  }

  test("SOSL simple query") {
    val soslPrimary = primaryOf[SOSL]("[Find 'something' RETURNING Account]")
    assert(soslPrimary.boundExpressions.isEmpty)
  }

  test("SOSL bound search") {
    val soslPrimary = primaryOf[SOSL]("[Find :Text RETURNING Account]")
    soslPrimary.boundExpressions should matchPattern {
      case ArraySeq(PrimaryExpression(IdPrimary(Id(Name("Text"))))) =>
    }
  }

  test("SOSL bound search and limit") {
    val soslPrimary = primaryOf[SOSL]("[Find :Text RETURNING Account LIMIT :1+1]")
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

  test("Type References") {

    assert(primaryOf[TypeReferencePrimary]("void.class").typeName == TypeName(Name("void")))
    assert(primaryOf[TypeReferencePrimary]("String.class").typeName == TypeName(Name("String")))
    assert(
      primaryOf[TypeReferencePrimary]("System.String.class").typeName == TypeName(
        Name("String"),
        Nil,
        Some(TypeNames.System)
      )
    )
    assert(
      primaryOf[TypeReferencePrimary]("List<String>.class").typeName == TypeName(
        Name("List"),
        Seq(TypeName(Name("String"))),
        None
      )
    )
    assert(
      primaryOf[TypeReferencePrimary]("List<System.String>.class").typeName == TypeName(
        Name("List"),
        Seq(TypeName(Name("String"), Nil, Some(TypeNames.System))),
        None
      )
    )
    assert(
      primaryOf[TypeReferencePrimary]("System.List<System.String>.class").typeName == TypeName(
        Name("List"),
        Seq(TypeName(Name("String"), Nil, Some(TypeNames.System))),
        Some(TypeNames.System)
      )
    )

  }

}
