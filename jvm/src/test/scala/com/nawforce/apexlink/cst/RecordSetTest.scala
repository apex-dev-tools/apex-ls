/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class RecordSetTest extends AnyFunSuite with TestHelper {

  private val warning = "RecordSet coerced to"

  test("Standard relationship warns for scalar SObject parameters") {
    typeDeclaration("""public class Dummy {
        |  void takeContact(Contact value) {}
        |  void takeSObject(SObject value) {}
        |  void verify(Account parent) {
        |    List<Contact> contacts = new List<Contact>();
        |    contacts.add(parent.Contacts);
        |    contacts.add(0, parent.Contacts);
        |    Set<Contact> contactSet = new Set<Contact>();
        |    contactSet.add(parent.Contacts);
        |    takeContact(parent.Contacts);
        |    takeSObject(parent.Contacts);
        |  }
        |}""".stripMargin)

    assert(dummyIssues.linesIterator.count(_.contains(warning)) == 5)
    assert(dummyIssues.linesIterator.count(_.contains("'Schema.Contact'")) == 4)
    assert(dummyIssues.linesIterator.count(_.contains("'System.SObject'")) == 1)
    assert(!dummyIssues.contains("Missing:"))
  }

  test("Custom relationship warns for scalar SObject parameter") {
    FileSystemHelper.run(
      Map(
        "objects/Parent__c.object" -> customObject("Parent", Seq()),
        "objects/Child__c.object" -> customObject(
          "Child",
          Seq(("Parent__c", Some("Lookup"), Some("Parent__c")))
        ),
        "Dummy.cls" ->
          """public class Dummy {
            |  void takeChild(Child__c value) {}
            |  void verify(Parent__c parent) {
            |    List<Child__c> children = new List<Child__c>();
            |    children.add(parent.Parent__r);
            |    takeChild(parent.Parent__r);
            |  }
            |}""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      val issues = getMessages(root.join("Dummy.cls"))
      assert(issues.linesIterator.count(_.contains(warning)) == 2)
      assert(issues.linesIterator.forall(!_.contains("Missing:")))
    }
  }

  test("Collection and Object parameters preserve RecordSet") {
    typeDeclaration("""public class Dummy {
        |  void takeObject(Object value) {}
        |  void takeList(List<Contact> value) {}
        |  void verify(Account parent) {
        |    List<Contact> assigned = parent.Contacts;
        |    List<Contact> contacts = new List<Contact>();
        |    contacts.addAll(parent.Contacts);
        |    contacts.contains(parent.Contacts);
        |    List<Object> objects = new List<Object>();
        |    objects.add(parent.Contacts);
        |    takeObject(parent.Contacts);
        |    takeList(parent.Contacts);
        |  }
        |}""".stripMargin)
    assert(dummyIssues.isEmpty, dummyIssues)
  }

  test("Collection overload wins over scalar overload") {
    happyTypeDeclaration("""public class Dummy {
        |  void take(Account value) {}
        |  void take(SObject value) {}
        |  void take(Object value) {}
        |  void take(List<Account> value) {}
        |  void verify() {
        |    take([SELECT Id FROM Account]);
        |  }
        |}""".stripMargin)
  }

  test("Direct SOQL warns for scalar SObject method and constructor parameters") {
    typeDeclaration("""public class Dummy {
        |  class Holder { Holder(Account value) {} }
        |  void take(Account value) {}
        |  void verify() {
        |    take([SELECT Id FROM Account]);
        |    new Holder([SELECT Id FROM Account]);
        |  }
        |}""".stripMargin)

    assert(dummyIssues.linesIterator.count(_.contains(warning)) == 2)
    assert(!dummyIssues.contains("Missing:"))
  }

  test("Ordinary List remains incompatible with scalar SObject parameter") {
    typeDeclaration("""public class Dummy {
        |  void take(Contact value) {}
        |  void verify(List<Contact> contacts) {
        |    take(contacts);
        |  }
        |}""".stripMargin)

    assert(dummyIssues.contains("No matching method found for 'take'"))
    assert(!dummyIssues.contains(warning))
  }
}
