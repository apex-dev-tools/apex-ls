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
