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
import org.scalatest.funsuite.AnyFunSuite

/** Annotation parameter validation over the outline parser, which is the primary path. The rules
  * are covered in full against the ANTLR adapter by AnnotationValidationTest, this shows the two
  * agree on each of them and records where they cannot.
  */
class AnnotationValidationOutlineTest extends AnyFunSuite with TestHelper {

  private def issuesFor(source: String): String = {
    typeDeclaration(source)
    dummyIssues
  }

  test("Whitespace separated parameters are accepted") {
    assert(issuesFor("@IsTest(SeeAllData=true IsParallel=false) public class Dummy {}").isEmpty)
  }

  test("Comma separated parameters are rejected") {
    assert(
      issuesFor("@IsTest(SeeAllData=true, IsParallel=false) public class Dummy {}")
        == "Error: line 1 at 25-41: Expecting ')' but was: ','\n"
    )
  }

  /* The grammar rejects this outright, so the ANTLR path reports a parse error and this path a
   * targeted one. Both are errors, only the wording differs. */
  test("Trailing comma is rejected") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=true,) public static String m() {return null;}}"
      ) == "Error: line 1 at 20-49: Expecting ')' but was: ','\n"
    )
  }

  test("Bare value is rejected where a name is required") {
    assert(
      issuesFor("public class Dummy {@AuraEnabled(true) public static String m() {return null;}}")
        == "Error: line 1 at 33-37: Annotation parameter on AuraEnabled must be written as name=value\n"
    )
  }

  test("Known parameter name is accepted") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=true) public static String m() {return null;}}"
      ).isEmpty
    )
  }

  test("Unknown parameter name is rejected") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(bogus=true) public static String m() {return null;}}"
      ) == "Error: line 1 at 33-38: No such property, bogus, defined on this annotation: AuraEnabled\n"
    )
  }

  test("Integer is rejected where a Boolean is expected") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=0) public static String m() {return null;}}"
      ) == "Error: line 1 at 43-44: Invalid value for property cacheable expected type Boolean\n"
    )
  }

  test("Unknown enum value is rejected") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=true scope='bogus') public static String m() {return null;}}"
      ) == "Error: line 1 at 54-61: Annotation property, scope on AuraEnabled, unknown value: bogus\n"
    )
  }

  test("Parameter not allowed on fields is rejected") {
    assert(
      issuesFor("public class Dummy {@AuraEnabled(cacheable=true) public String f;}")
        == "Error: line 1 at 33-42: Annotation property, cacheable on AuraEnabled, is not allowed on fields\n"
    )
  }

  test("Parameter not allowed on properties is rejected") {
    assert(
      issuesFor("public class Dummy {@AuraEnabled(cacheable=true) public String p {get; set;}}")
        == "Error: line 1 at 33-42: Annotation property, cacheable on AuraEnabled, is not allowed on properties\n"
    )
  }

  test("Scope without cacheable is rejected") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(scope='global') public static String m() {return null;}}"
      ) == "Error: line 1 at 33-47: Invalid combination of values for properties cacheable and scope on AuraEnabled\n"
    )
  }

  test("Empty parameter list is rejected where one is required") {
    assert(
      issuesFor("@JsonAccess() public class Dummy {}")
        == "Error: line 1 at 0-13: At least one JSON serialization control parameter must be specified\n"
    )
  }

  test("Rest resource url must begin with a slash") {
    assert(
      issuesFor("@RestResource(urlMapping='x') public class Dummy {}")
        == "Error: line 1 at 25-28: Rest Resource url must begin with a forward slash, '/'\n"
    )
  }

  test("Suppress warnings unknown parameter name is rejected") {
    assert(
      issuesFor("@SuppressWarnings(bogus='PMD') public class Dummy {}")
        == "Error: line 1 at 18-23: No such property, bogus, defined on this annotation: SuppressWarnings\n"
    )
  }

  test("Suppress warnings named value is accepted") {
    assert(issuesFor("@SuppressWarnings(value='PMD') public class Dummy {}").isEmpty)
  }

  test("Duplicate parameters are accepted") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=true cacheable=false) public static String m() {return null;}}"
      ).isEmpty
    )
  }

  test("Unknown annotation is not validated") {
    assert(issuesFor("@Bogus(anything=1) public class Dummy {}").isEmpty)
  }

  test("Required with default value is rejected") {
    assert(
      issuesFor(
        "public class Dummy {@InvocableVariable(required=true defaultValue='z') public String f;}"
      )
        == "Error: line 1 at 53-69: Invalid combination of values for properties required and defaultValue on InvocableVariable\n"
    )
  }

  test("Parallel test seeing all data is rejected") {
    assert(
      issuesFor("@IsTest(SeeAllData=true IsParallel=true) public class Dummy {}")
        == "Error: line 1 at 24-39: Test class annotated with @isTest(IsParallel=true) cannot also be annotated with @isTest(SeeAllData=true)\n"
    )
  }

  test("Parallel test not seeing all data is accepted") {
    assert(issuesFor("@IsTest(SeeAllData=false IsParallel=true) public class Dummy {}").isEmpty)
  }

  test("Comma is rejected on an unknown annotation") {
    assert(
      issuesFor("@Bogus(a=1, b=2) public class Dummy {}")
        == "Error: line 1 at 12-15: Expecting ')' but was: ','\n"
    )
  }

  test("Trailing comma is rejected on an unknown annotation") {
    assert(
      issuesFor("@Bogus(cacheable=true,) public class Dummy {}")
        == "Error: line 1 at 0-23: Expecting ')' but was: ','\n"
    )
  }

  test("Comma is rejected on an annotation with an unestablished parameter set") {
    assert(
      issuesFor("@IntegrationTest(SeeAllData=true, IsParallel=false) public class Dummy {}")
        == "Error: line 1 at 34-50: Expecting ')' but was: ','\n"
    )
  }

  test("Property target rule does not fire where the annotation itself is rejected") {
    assert(
      issuesFor("@AuraEnabled(cacheable=true) public class Dummy {}")
        == "Error: line 1 at 0-28: Annotation '@AuraEnabled' is not supported on classes\n"
    )
  }

  test("Continuation carries no target restriction") {
    assert(
      issuesFor("public class Dummy {@AuraEnabled(continuation=true) public String f;}").isEmpty
    )
  }

  /* A parameter diagnostic must not suppress the rules that both report on and correct the
   * modifier set, they guard on the logger being empty. */
  test("Parameter error does not suppress the inner test class rule") {
    assert(
      issuesFor("public class Dummy {@IsTest(SeeAllData=true, IsParallel=false) class Inner {}}")
        == "Error: line 1 at 45-61: Expecting ')' but was: ','\n" +
        "Error: line 1 at 69-74: Test annotations can only be used on outer classes\n"
    )
  }

  test("Parameter error does not suppress the outer class visibility rule") {
    assert(
      issuesFor("@RestResource(urlMapping='x') class Dummy {}")
        == "Error: line 1 at 25-28: Rest Resource url must begin with a forward slash, '/'\n" +
        "Error: line 1 at 36-41: Outer classes must be declared either 'global' or 'public'\n"
    )
  }

  test("Parameter error does not suppress the duplicate visibility warning") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=0) public private static String m() {return null;}}"
      ) == "Error: line 1 at 43-44: Invalid value for property cacheable expected type Boolean\n" +
        "Warning: line 1 at 75-76: Only one visibility modifier from 'global', 'public' & 'private' should be used on methods\n"
    )
  }

  /* Only this path sees it, the grammar requires a separator. The value is reported rather than
   * the missing separator because that is all the outline parser can see. */
  test("Missing separator is reported against the value") {
    assert(
      issuesFor(
        "public class Dummy {@AuraEnabled(cacheable=truescope='global') public static String m() {return null;}}"
      ) == "Error: line 1 at 43-61: Invalid value for property cacheable expected type Boolean\n"
    )
  }
}
