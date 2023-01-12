/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class DatabaseTest extends AnyFunSuite with TestHelper {

  test("QueryLocator from query String") {
    typeDeclaration(
      "public class Dummy { {Database.QueryLocator locator = Database.getQueryLocator('SELECT Id FROM Account'); } }"
    )
    assert(dummyIssues.isEmpty)
  }

  test("QueryLocator from inline SOQL") {
    typeDeclaration(
      "public class Dummy { {Database.QueryLocator locator = Database.getQueryLocator([SELECT Id FROM Account]); } }"
    )
    assert(dummyIssues.isEmpty)
  }

}
