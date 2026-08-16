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

  private def assertDummyIssues(source: String, expected: String): Unit = {
    typeDeclaration(source)
    assert(dummyIssues == expected)
  }

  private def assertCustomIssues(source: String, expected: String): Unit = {
    FileSystemHelper.run(
      Map(
        "objects/Parent__c.object" -> customObject("Parent", Seq()),
        "objects/Child__c.object" -> customObject(
          "Child",
          Seq(("Parent__c", Some("Lookup"), Some("Parent__c")))
        ),
        "Dummy.cls" -> source
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(getMessages(root.join("Dummy.cls")) == expected)
    }
  }

  private def assertProjectIssues(files: Map[String, String], expected: String): Unit = {
    FileSystemHelper.run(files) { root: PathLike =>
      createOrg(root)
      assert(getMessages(root.join("Dummy.cls")) == expected)
    }
  }

  test("List.add(T) warns for a standard relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    List<Contact> values = new List<Contact>();
        |    values.add(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 15-30: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("List.add(Integer, T) warns for a standard relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    List<Contact> values = new List<Contact>();
        |    values.add(0, parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 18-33: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Set.add(T) warns for a standard relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    Set<Contact> values = new Set<Contact>();
        |    values.add(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 15-30: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Contact parameter warns for a standard relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact value) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 9-24: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("SObject parameter warns for a standard relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(SObject value) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 9-24: RecordSet coerced to 'System.SObject'; runtime requires exactly one row\n"
    )
  }

  test("List.add(T) warns for a custom relationship RecordSet") {
    assertCustomIssues(
      """public class Dummy {
        |  void verify(Parent__c parent) {
        |    List<Child__c> values = new List<Child__c>();
        |    values.add(parent.Parent__r);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 15-31: RecordSet coerced to 'Schema.Child__c'; runtime requires exactly one row\n"
    )
  }

  test("Custom SObject parameter warns for a custom relationship RecordSet") {
    assertCustomIssues(
      """public class Dummy {
        |  void take(Child__c value) {}
        |  void verify(Parent__c parent) {
        |    take(parent.Parent__r);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 9-25: RecordSet coerced to 'Schema.Child__c'; runtime requires exactly one row\n"
    )
  }

  test("Direct SOQL warns for a scalar SObject method parameter") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Account value) {}
        |  void verify() {
        |    take([SELECT Id FROM Account]);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 9-33: RecordSet coerced to 'Schema.Account'; runtime requires exactly one row\n"
    )
  }

  test("Direct SOQL warns for a scalar SObject constructor parameter") {
    assertDummyIssues(
      """public class Dummy {
        |  class Holder { Holder(Account value) {} }
        |  void verify() {
        |    new Holder([SELECT Id FROM Account]);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 15-39: RecordSet coerced to 'Schema.Account'; runtime requires exactly one row\n"
    )
  }

  test("Relationship RecordSet receiver contains warns for a RecordSet argument") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    parent.Contacts.contains(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 3 at 29-44: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Relationship RecordSet receiver indexOf warns for a RecordSet argument") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    parent.Contacts.indexOf(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 3 at 28-43: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Direct SOQL RecordSet receiver contains warns for a RecordSet argument") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify() {
        |    [SELECT Id FROM Contact].contains([SELECT Id FROM Contact]);
        |  }
        |}""".stripMargin,
      "Warning: line 3 at 38-62: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Direct SOQL RecordSet receiver indexOf warns for a RecordSet argument") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify() {
        |    [SELECT Id FROM Contact].indexOf([SELECT Id FROM Contact]);
        |  }
        |}""".stripMargin,
      "Warning: line 3 at 37-61: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Assignment and addAll preserve a relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    List<Contact> assigned = parent.Contacts;
        |    List<Contact> values = new List<Contact>();
        |    values.addAll(parent.Contacts);
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Object parameters preserve a relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void takeObject(Object value) {}
        |  void verify(Account parent) {
        |    List<Object> objects = new List<Object>();
        |    objects.add(parent.Contacts);
        |    takeObject(parent.Contacts);
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Multiple RecordSet arguments warn at their respective locations") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact first, SObject second) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts, parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 9-24: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n" +
        "Warning: line 4 at 26-41: RecordSet coerced to 'System.SObject'; runtime requires exactly one row\n"
    )
  }

  test("This constructor call warns for a RecordSet argument") {
    assertDummyIssues(
      """public class Dummy {
        |  Dummy(Contact value) {}
        |  Dummy(Account parent) {
        |    this(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 4 at 9-24: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Super constructor call warns for a RecordSet argument") {
    assertProjectIssues(
      Map(
        "Base.cls" -> "virtual class Base { public Base(Contact value) {} }",
        "Dummy.cls" ->
          """public class Dummy extends Base {
            |  public Dummy(Account parent) {
            |    super(parent.Contacts);
            |  }
            |}""".stripMargin
      ),
      "Warning: line 3 at 10-25: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Standard child relationship RecordSet cannot receive chained field access") {
    assertDummyIssues(
      """public class Dummy {
        |  void verify(Account parent) {
        |    Object value = parent.Contacts.Account;
        |  }
        |}""".stripMargin,
      "Missing: line 3 at 19-42: Unknown field 'Account' on SObject '[Schema.Contact Records]'\n"
    )
  }

  test("Custom child relationship RecordSet cannot receive chained field access") {
    assertCustomIssues(
      """public class Dummy {
        |  void verify(Parent__c parent) {
        |    Object value = parent.Parent__r.Parent__c;
        |  }
        |}""".stripMargin,
      "Missing: line 3 at 19-45: Unknown field 'Parent__c' on SObject '[Schema.Child__c Records]'\n"
    )
  }

  test("Direct SOQL RecordSet may access a child relationship") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact value) {}
        |  void verify() {
        |    List<Contact> assigned = [SELECT Id, (SELECT Id FROM Contacts) FROM Account].Contacts;
        |    take([SELECT Id, (SELECT Id FROM Contacts) FROM Account].Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 5 at 9-69: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Standard RecordSets preserve Iterable compatibility across type flows") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Iterable<Contact> values) {}
        |  Iterable<Contact> queryValues() { return [SELECT Id FROM Contact]; }
        |  Iterable<Contact> relationshipValues(Account parent) { return parent.Contacts; }
        |  void verify(Account parent, Boolean choose, Iterable<Contact> values) {
        |    Iterable<Contact> assigned = parent.Contacts;
        |    take(parent.Contacts);
        |    take([SELECT Id FROM Contact]);
        |    Iterable<Contact> common = choose ? parent.Contacts : new List<Contact>();
        |    Boolean comparable = values == parent.Contacts;
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Custom RecordSets preserve Iterable compatibility") {
    assertCustomIssues(
      """public class Dummy {
        |  void take(Iterable<Child__c> values) {}
        |  Iterable<Child__c> queryValues() { return [SELECT Id FROM Child__c]; }
        |  void verify(Parent__c parent) {
        |    Iterable<Child__c> assigned = parent.Parent__r;
        |    take(parent.Parent__r);
        |    take([SELECT Id FROM Child__c]);
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Method overload dominance allows equal and better RecordSet ranks") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact first, Contact second) {}
        |  void take(Contact first, SObject second) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts, parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 5 at 9-24: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n" +
        "Warning: line 5 at 26-41: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Constructor overload dominance allows equal and better RecordSet ranks") {
    assertDummyIssues(
      """public class Dummy {
        |  class Holder {
        |    Holder(Contact first, Contact second) {}
        |    Holder(Contact first, SObject second) {}
        |  }
        |  void verify(Account parent) {
        |    new Holder(parent.Contacts, parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 7 at 15-30: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n" +
        "Warning: line 7 at 32-47: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Crossed method RecordSet ranks remain ambiguous") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(List<Contact> first, Contact second) {}
        |  void take(Contact first, List<Contact> second) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts, parent.Contacts);
        |  }
        |}""".stripMargin,
      "Missing: line 5 at 4-42: Ambiguous method call for 'take' on 'Dummy' taking arguments '[Schema.Contact Records], [Schema.Contact Records]', wrong argument types for calling 'void take(Schema.Contact first, System.List<Schema.Contact> second)'\n"
    )
  }

  test("Crossed constructor RecordSet ranks remain ambiguous") {
    assertDummyIssues(
      """public class Dummy {
        |  class Holder {
        |    Holder(List<Contact> first, Contact second) {}
        |    Holder(Contact first, List<Contact> second) {}
        |  }
        |  void verify(Account parent) {
        |    new Holder(parent.Contacts, parent.Contacts);
        |  }
        |}""".stripMargin,
      "Error: line 7 at 14-48: Ambiguous constructor call: Dummy.Holder.<constructor>([Schema.Contact Records],[Schema.Contact Records])\n"
    )
  }

  test("Collection parameter preserves a relationship RecordSet") {
    assertDummyIssues(
      """public class Dummy {
        |  void takeList(List<Contact> value) {}
        |  void verify(Account parent) {
        |    takeList(parent.Contacts);
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Collection overload wins over scalar overload") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Account value) {}
        |  void take(SObject value) {}
        |  void take(Object value) {}
        |  void take(List<Account> value) {}
        |  void verify() {
        |    take([SELECT Id FROM Account]);
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Iterable overload wins over scalar overload") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact value) {}
        |  void take(Iterable<Contact> values) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts);
        |    take([SELECT Id FROM Contact]);
        |  }
        |}""".stripMargin,
      ""
    )
  }

  test("Concrete scalar overload wins over SObject and Object overloads") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact value) {}
        |  void take(SObject value) {}
        |  void take(Object value) {}
        |  void verify(Account parent) {
        |    take(parent.Contacts);
        |  }
        |}""".stripMargin,
      "Warning: line 6 at 9-24: RecordSet coerced to 'Schema.Contact'; runtime requires exactly one row\n"
    )
  }

  test("Ordinary List remains incompatible with scalar SObject parameter") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact value) {}
        |  void verify(List<Contact> values) {
        |    take(values);
        |  }
        |}""".stripMargin,
      "Missing: line 4 at 4-16: No matching method found for 'take' on 'Dummy' taking arguments 'System.List<Schema.Contact>', wrong argument types for calling 'void take(Schema.Contact value)'\n"
    )
  }

  test("Ordinary Set remains incompatible with scalar SObject parameter") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(Contact value) {}
        |  void verify(Set<Contact> values) {
        |    take(values);
        |  }
        |}""".stripMargin,
      "Missing: line 4 at 4-16: No matching method found for 'take' on 'Dummy' taking arguments 'System.Set<Schema.Contact>', wrong argument types for calling 'void take(Schema.Contact value)'\n"
    )
  }

  test("Ordinary List<SObject> remains incompatible with SObject parameter") {
    assertDummyIssues(
      """public class Dummy {
        |  void take(SObject value) {}
        |  void verify(List<SObject> values) {
        |    take(values);
        |  }
        |}""".stripMargin,
      "Missing: line 4 at 4-16: No matching method found for 'take' on 'Dummy' taking arguments 'System.List<System.SObject>', wrong argument types for calling 'void take(System.SObject value)'\n"
    )
  }
}
