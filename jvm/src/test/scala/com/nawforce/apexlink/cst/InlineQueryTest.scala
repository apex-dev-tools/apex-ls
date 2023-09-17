/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class InlineQueryTest extends AnyFunSuite with TestHelper {
  test("SOQL multiple FROM validate error") {
    typeDeclaration("public class Dummy {{ Object a = [Select Id from Account, Contact]; }}")
    assert(
      dummyIssues ==
        "Error: line 1 at 33-66: Expecting SOQL to query only a single SObject, found 'Account, Contact'\n"
    )
  }

  test("SOQL unknown FROM validate error") {
    typeDeclaration("public class Dummy {{ Object a = [Select Id from Foo]; }}")
    assert(dummyIssues == "Missing: line 1 at 33-53: No type declaration found for 'Schema.Foo'\n")
  }

  test("SOQL ghosted FROM") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |"packageDirectories": [{"path": "force-app"}],
            |"plugins": {"dependencies": [{"namespace": "ext"}]}
            |}""".stripMargin,
        "force-app/Dummy.cls" -> "public class Dummy { {{ List<ext__Foo__c> a = [Select Id from ext__Foo__c]; }} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("SOQL list assignment") {
    happyTypeDeclaration("public class Dummy {{ List<Account> a = [Select Id from Account]; }}")
  }

  test("SOQL list assignment to SObject") {
    happyTypeDeclaration("public class Dummy {{ List<SObject> a = [Select Id from Account]; }}")
  }

  test("SOQL list assignment to wrong type") {
    typeDeclaration("public class Dummy {{ List<Contact> a = [Select Id from Account]; }}")
    assert(
      dummyIssues == "Error: line 1 at 36-64: Incompatible types in assignment, from '[Schema.Account Records]' to 'System.List<Schema.Contact>'\n"
    )
  }

  test("SOQL list unknown type") {
    happyTypeDeclaration("public class Dummy {{ List<Account> a = Database.query(''); }}")
  }

  test("SOQL list unknown type to SObject") {
    happyTypeDeclaration("public class Dummy {{ List<SObject> a = Database.query(''); }}")
  }

  test("SOQL single assignment") {
    happyTypeDeclaration("public class Dummy {{ Account a = [Select Id from Account]; }}")
  }

  test("SOQL single assignment to SObject") {
    happyTypeDeclaration("public class Dummy {{ SObject a = [Select Id from Account]; }}")
  }

  test("SOQL single assignment to wrong type") {
    typeDeclaration("public class Dummy {{ Contact a = [Select Id from Account]; }}")
    assert(
      dummyIssues == "Error: line 1 at 30-58: Incompatible types in assignment, from '[Schema.Account Records]' to 'Schema.Contact'\n"
    )
  }

  test("SOQL single unknown type") {
    happyTypeDeclaration("public class Dummy {{ Account a = Database.query(''); }}")
  }

  test("SOQL single unknown type to SObject") {
    happyTypeDeclaration("public class Dummy {{ SObject a = Database.query(''); }}")
  }

  test("SOQL field reference") {
    happyTypeDeclaration("public class Dummy {{ Id a = [Select Id from Account].Id; }}")
  }

  test("SOQL Count assignment") {
    happyTypeDeclaration("public class Dummy {{ Integer a = [Select Count() from Account]; }}")
  }

  test("SOQL Aggregate assignment") {
    happyTypeDeclaration(
      "public class Dummy {{ List<AggregateResult> a = [Select Count(Id) from Account]; }}"
    )
  }

  test("SOQL Aggregate assignment to SObject") {
    happyTypeDeclaration(
      "public class Dummy {{ List<SObject> a = [Select Count(Id) from Account]; }}"
    )
  }

}
