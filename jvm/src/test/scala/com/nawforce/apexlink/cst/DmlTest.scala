/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite

class DmlTest extends AnyFunSuite with TestHelper {

  forAll(Set("Insert", "Update", "Upsert", "Delete", "Undelete")) { statement =>
    val statementLength = statement.length

    test(s"$statement SObject") {
      happyTypeDeclaration(s"public class Dummy {{ $statement new Account(); }}")
    }

    test(s"$statement SObject list") {
      happyTypeDeclaration(s"public class Dummy {{ $statement new List<Account>(); }}")
    }

    test(s"$statement generic SObject") {
      happyTypeDeclaration(s"public class Dummy {{ SObject a; $statement a; }}")
    }

    test(s"$statement generic SObject list") {
      happyTypeDeclaration(s"public class Dummy {{ $statement new List<SObject>(); }}")
    }

    test(s"$statement recordset") {
      happyTypeDeclaration(s"public class Dummy {{ $statement [Select Id from Account]; }}")
    }

    test(s"$statement String") {
      typeDeclaration(s"public class Dummy {{ $statement 'hello'; }}")
      assert(
        dummyIssues == s"Error: line 1 at ${23 + statementLength}-${30 + statementLength}: $statement expression should return an SObject or list of SObjects, not a 'System.String' instance\n"
      )
    }

    test(s"$statement custom class") {
      typeDeclaration(s"public class Dummy {{ $statement new Dummy(); }}")
      assert(
        dummyIssues == s"Error: line 1 at ${23 + statementLength}-${34 + statementLength}: $statement expression should return an SObject or list of SObjects, not a 'Dummy' instance\n"
      )
    }
  }

  test(s"Merge Account to Account") {
    happyTypeDeclaration(s"public class Dummy {{ Account a; merge a a; }}")
  }

  test(s"Merge List<Account> to Account") {
    happyTypeDeclaration(s"public class Dummy {{ Account a; merge a new List<Account>(); }}")
  }

  test(s"Merge RecordSet<Account> to Account") {
    happyTypeDeclaration(s"public class Dummy {{ Account a; merge a [Select Id from Account]; }}")
  }

  test(s"Merge Account to Opportunity") {
    typeDeclaration(s"public class Dummy {{ Opportunity a; Account b; merge a b; }}")
    assert(
      dummyIssues == s"Error: line 1 at 54-55: Merge expression should return a Lead or Contact or Case or Account SObject, not a 'Schema.Opportunity' instance\n"
    )
  }

  test(s"Merge Opportunity to Account") {
    typeDeclaration(s"public class Dummy {{ Account a; Opportunity b; merge a b; }}")
    assert(
      dummyIssues == s"Error: line 1 at 56-57: Merge expression should return a Lead or Contact or Case or Account SObject or list of SObjects, not a 'Schema.Opportunity' instance\n"
    )
  }

  test(s"Merge Opportunity list to Account") {
    typeDeclaration(s"public class Dummy {{ Account a; List<Opportunity> b; merge a b; }}")
    assert(
      dummyIssues == s"Error: line 1 at 62-63: Merge expression should return a Lead or Contact or Case or Account SObject or list of SObjects, not a 'System.List<Schema.Opportunity>' instance\n"
    )
  }

  test(s"Merge Contact to Account") {
    typeDeclaration(s"public class Dummy {{ Account a; Contact b; merge a b; }}")
    assert(
      dummyIssues == s"Error: line 1 at 44-54: Incompatible types used in merge, 'Schema.Account' and 'Schema.Contact'\n"
    )
  }

  test(s"Merge List<Contact> to Account") {
    typeDeclaration(s"public class Dummy {{ Account a; List<Contact> b; merge a b; }}")
    assert(
      dummyIssues == s"Error: line 1 at 50-60: Incompatible types used in merge, 'Schema.Account' and 'Schema.Contact'\n"
    )
  }

}
