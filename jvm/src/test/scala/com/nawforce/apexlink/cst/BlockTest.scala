/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.OutlineParserSingleThreaded
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
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

}
