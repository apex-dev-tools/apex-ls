/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.Name
import org.scalatest.funsuite.AnyFunSuite

class FieldTest extends AnyFunSuite with TestHelper {

  test("Empty class has no fields") {
    assert(typeDeclaration("public class Dummy {}").fields.isEmpty)
    assert(!hasIssues)
  }

  test("Field visible") {
    val field = typeDeclaration("public class Dummy {String foo;}").fields.head
    assert(!hasIssues)
    assert(field.name == Name("foo"))
    assert(field.readAccess == PRIVATE_MODIFIER)
    assert(field.writeAccess == PRIVATE_MODIFIER)
  }

  test("Multiple fields visible") {
    val fields = typeDeclaration("public class Dummy {String foo; Integer bar;}").fields
    assert(!hasIssues)
    assert(fields.map(_.name).toSet == Set(Name("foo"), Name("bar")))
  }

  test("Duplicate field reports error on duplicate") {
    val td = typeDeclaration("public class Dummy {String foo; String foo;}")
    val fields = withOrg { _ =>
      td.fields
    }
    assert(fields.length == 1)
    assert(fields.head.name == Name("foo"))
    assert(
      dummyIssues ==
        "Error: line 1 at 39-42: Duplicate field/property: 'foo'\n"
    )
  }

  test("More than one duplicate field reports error on duplicates") {
    val td = typeDeclaration("public class Dummy {String foo; Integer foo; String foo;}")
    val fields = withOrg { _ =>
      td.fields
    }
    assert(fields.length == 1)
    assert(fields.head.name == Name("foo"))
    assert(
      dummyIssues ==
        "Error: line 1 at 40-43: Duplicate field/property: 'foo'\nError: line 1 at 52-55: Duplicate field/property: 'foo'\n"
    )
  }

  test("Super declaration shadowed field name") {
    typeDeclarations(
      Map(
        "Foo.cls" -> "public virtual class Foo {  public final static Bar Account {get {return new Bar();}} }",
        "Bar.cls" -> "public class Bar extends Foo { public void method(){SObjectField a = Account.Name;} }"
      )
    )
    assert(getMessages(root.join("Bar.cls")).isEmpty)
  }

  test("protected static field") {
    typeDeclarations(Map("Dummy.cls" -> "public class Dummy { protected static String foo; }"))
    assert(dummyIssues == "Error: line 1 at 38-49: protected field 'foo' cannot be static\n")
  }

  test("Unrelated class can not access private static field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public class Target {private static String helper;}",
        "Dummy.cls" -> "public class Dummy {public static void f2() {String value = Target.helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).contains("Field is not visible"))
  }

  test("Unrelated class can not access private instance field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public class Target {private String helper;}",
        "Dummy.cls" -> "public class Dummy {public void f2(Target target) {String value = target.helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).contains("Field is not visible"))
  }

  test("Unrelated class can not access protected instance field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public virtual class Target {protected String helper;}",
        "Dummy.cls" -> "public class Dummy {public void f2(Target target) {String value = target.helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).contains("Field is not visible"))
  }

  test("Subclass can access protected instance field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public virtual class Target {protected String helper;}",
        "Dummy.cls" -> "public class Dummy extends Target {public void f2() {String value = helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).isEmpty)
  }

  test("Subclass can not access private instance field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public virtual class Target {private String helper;}",
        "Dummy.cls" -> "public class Dummy extends Target {public void f2() {String value = helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).contains("Field is not visible"))
  }

  test("Same file class can access private field") {
    typeDeclaration(
      "public class Dummy {private static String helper; public class Inner {public void f2() {String value = Dummy.helper;}}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Potentially recursive field lookup") {
    // The search order for 'a' here is Inner -> (Outer)Dummy -> (Superclass)Inner -> (Outer)Dummy
    // Unless we step in to stop the recursion by avoiding a repeat search of Inner
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          """public class Dummy extends Inner {
        |  public virtual class Inner {
        |     { Integer b=a; }
        |  }
        |}""".stripMargin
      )
    )
    assert(
      dummyIssues == "Missing: line 3 at 17-18: No variable or type found for 'a' on 'Dummy.Inner'\n"
    )
  }

  test("IntegrationTest can not access private TestVisible field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public class Target {@TestVisible private static String helper;}",
        "Dummy.cls" -> "@IntegrationTest public class Dummy {@IntegrationTest public static void f2() {String value = Target.helper;}}"
      )
    )
    assert(
      getMessages(
        root.join("Dummy.cls")
      ) == "Error: line 1 at 94-107: Private @TestVisible fields can only be accessed from @IsTest classes\n"
    )
  }

  test("Non-test class can not access private TestVisible field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public class Target {@TestVisible private static String helper;}",
        "Dummy.cls" -> "public class Dummy {public static void f2() {String value = Target.helper;}}"
      )
    )
    assert(
      getMessages(
        root.join("Dummy.cls")
      ) == "Error: line 1 at 60-73: Private @TestVisible fields can only be accessed from @IsTest classes\n"
    )
  }

  test("IsTest class can access private TestVisible field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public class Target {@TestVisible private static String helper;}",
        "Dummy.cls" -> "@IsTest public class Dummy {@IsTest public static void f2() {String value = Target.helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).isEmpty)
  }

  test("IsTest class can access protected TestVisible field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public virtual class Target {@TestVisible protected static String helper;}",
        "Dummy.cls" -> "@IsTest public class Dummy {@IsTest public static void f2() {String value = Target.helper;}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).isEmpty)
  }

  test("Nested class in IsTest class can access private TestVisible field") {
    typeDeclarations(
      Map(
        "Target.cls" -> "public class Target {@TestVisible private static String helper;}",
        "Dummy.cls" -> "@IsTest public class Dummy {private class Inner {public void f2() {String value = Target.helper;}}}"
      )
    )
    assert(getMessages(root.join("Dummy.cls")).isEmpty)
  }

  test("Same file class can access private TestVisible field") {
    typeDeclaration(
      "public class Dummy {@TestVisible private static String helper; public class Inner {public void f2() {String value = Dummy.helper;}}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Nested class resolves enclosing static field over superclass enclosing field") {
    typeDeclarations(
      Map(
        "BaseFramework.cls" ->
          """public class BaseFramework {
            |  private static final String NAMESPACE = 'base__';
            |
            |  public abstract class BaseConfig {
            |    public abstract void setup();
            |  }
            |}""".stripMargin,
        "Consumer.cls" ->
          """public class Consumer {
            |  private static final String NAMESPACE = 'acme__';
            |
            |  public class Configuration extends BaseFramework.BaseConfig {
            |    public override void setup() {
            |      String objName = NAMESPACE + 'MyObject__c';
            |      System.debug(objName);
            |    }
            |  }
            |}""".stripMargin
      )
    )
    assert(getMessages(root.join("Consumer.cls")).isEmpty)
  }

  test("Nested subclass resolves enclosing static field over inherited private field") {
    typeDeclarations(
      Map(
        "Base1.cls" ->
          """public virtual class Base1 {
            |  private final Integer bec;
            |  public Base1(Integer bec) {
            |    this.bec = bec;
            |  }
            |}""".stripMargin,
        "Outer1.cls" ->
          """public class Outer1 {
            |  private static final Integer BEC = 5;
            |
            |  public class Inner1 extends Base1 {
            |    public Inner1() {
            |      super(BEC);
            |    }
            |
            |    public Integer value() {
            |      return BEC;
            |    }
            |  }
            |}""".stripMargin
      )
    )
    assert(getMessages(root.join("Outer1.cls")).isEmpty)
  }

  test("Nested class resolves unqualified static through superclass enclosing type") {
    // Verified against the platform: a nested class extending an externally nested base may
    // resolve an unqualified static field name through the base class's enclosing type when
    // there is no name in scope to shadow it (e.g. ffhttp_OAuthClient referencing
    // ffhttp_Client.REQUEST_METHOD_POST from a subclass of ffhttp_Client.AbstractClientRequest).
    typeDeclarations(
      Map(
        "BaseFramework.cls" ->
          """public class BaseFramework {
            |  public static final String REQUEST_METHOD_POST = 'POST';
            |
            |  public abstract class BaseRequest {
            |    public String method;
            |    public BaseRequest(String requestMethod) {
            |      this.method = requestMethod;
            |    }
            |  }
            |}""".stripMargin,
        "Consumer.cls" ->
          """public class Consumer {
            |  public class ExchangeRequest extends BaseFramework.BaseRequest {
            |    public ExchangeRequest() {
            |      super(REQUEST_METHOD_POST);
            |    }
            |  }
            |}""".stripMargin
      )
    )
    assert(getMessages(root.join("Consumer.cls")).isEmpty)
  }

}
