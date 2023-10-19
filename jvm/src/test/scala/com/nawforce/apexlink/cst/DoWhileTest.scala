/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class DoWhileTest extends AnyFunSuite with TestHelper {

  test("Bad conditional") {
    typeDeclaration("public class Dummy {{ do {} while (foo); }}")
    assert(
      dummyIssues == "Missing: line 1 at 35-38: No variable or type found for 'foo' on 'Dummy'\n"
    )
  }

  test("Non boolean conditional") {
    typeDeclaration("public class Dummy {{ do {} while (''); }}")
    assert(
      dummyIssues == "Error: line 1 at 35-37: While expression should return a 'System.Boolean' instance, not a 'System.String' instance\n"
    )
  }

  test("Null boolean conditional") {
    typeDeclaration("public class Dummy {{ do {} while (null); }}")
    assert(
      dummyIssues == "Error: line 1 at 35-39: While expression should return a 'System.Boolean' instance, not a 'null' instance\n"
    )
  }

  test("Static boolean conditional") {
    typeDeclaration("public class Dummy {{ do {} while (Boolean); }}")
    assert(
      dummyIssues == "Error: line 1 at 35-42: While expression should return a 'System.Boolean' instance, not a 'System.Boolean' type\n"
    )
  }

  test("Single statement") {
    FileSystemHelper.run(
      Map("Dummy.cls" -> "public class Dummy {{ do System.debug(''); while (true); }}")
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(root.join("Dummy.cls"))
          .startsWith("Syntax: line 1 at 25: missing '{' at 'System'")
      )
    }
  }

  test("Single block") {
    happyTypeDeclaration("public class Dummy {{ do {System.debug('');} while (true);  }}")
  }

}
