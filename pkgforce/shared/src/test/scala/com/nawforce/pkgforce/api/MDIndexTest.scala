/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api

import com.nawforce.pkgforce.TestHelper
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class MDIndexTest extends AnyFunSuite with TestHelper {

  test("Empty org") {
      virtualFS(Map()) { root: PathLike =>
        val index = MDIndex.create(root)
        assert(index != null)
      }
    }
  }
