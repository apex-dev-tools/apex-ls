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
package com.nawforce.pkgforce.modifiers

import com.nawforce.pkgforce.parsers.ApexNode
import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.parsers.{CodeParser, SourceData}
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite

/** Annotation parameter validation over the ANTLR adapter, which is used on outline parser failure,
  * on incremental refresh, for triggers and by ApexClassVisitor. The outline parser path is covered
  * by AnnotationValidationOutlineTest, the two must agree.
  */
class AnnotationValidationTest extends AnyFunSuite {

  private def onClass(annotation: String): String = s"$annotation public class Dummy {}"
  private def onMethod(annotation: String): String =
    s"public class Dummy {$annotation public static String m() {return null;}}"
  private def onField(annotation: String): String =
    s"public class Dummy {$annotation public String f;}"
  private def onProperty(annotation: String): String =
    s"public class Dummy {$annotation public String p {get; set;}}"

  /* Messages reported for the source, parse errors included so that a form the grammar rejects can
   * be told apart from one it accepts and we report on. */
  private def messages(source: String): Seq[String] = {
    val parser = CodeParser(Path("Dummy.cls"), SourceData(source))
    val result = parser.parseClass()
    if (result.issues.nonEmpty)
      result.issues.map(_.diagnostic.message).toSeq
    else
      ApexNode(parser, result.value).get.collectIssues().map(_.diagnostic.message).toSeq
  }

  private def message(source: String): String = messages(source).mkString("\n")

  /* The source each diagnostic was anchored on, which must be the parameter at fault. */
  private def anchors(source: String): Seq[String] = {
    val parser = CodeParser(Path("Dummy.cls"), SourceData(source))
    val result = parser.parseClass()
    assert(result.issues.isEmpty, result.issues.mkString("\n"))
    ApexNode(parser, result.value).get
      .collectIssues()
      .map(issue => Location.extract(source, issue.diagnostic.location))
      .toSeq
  }

  test("Whitespace separated parameters are accepted") {
    assert(messages(onClass("@IsTest(SeeAllData=true IsParallel=false)")).isEmpty)
  }

  test("Comma separated parameters are rejected") {
    assert(
      message(onClass("@IsTest(SeeAllData=true, IsParallel=false)")) == "Expecting ')' but was: ','"
    )
  }

  test("Comma without surrounding space is rejected") {
    assert(
      message(onClass("@IsTest(SeeAllData=true,IsParallel=false)")) == "Expecting ')' but was: ','"
    )
  }

  test("Comma is reported once however many are written") {
    assert(
      messages(onMethod("@InvocableMethod(label='a', description='b', category='c')")) == Seq(
        "Expecting ')' but was: ','"
      )
    )
  }

  test("Comma is anchored on the parameter that follows it") {
    assert(
      anchors(onClass("@IsTest(SeeAllData=true, IsParallel=false)")) == Seq("IsParallel=false")
    )
  }

  /* Forms the grammar rejects outright, they never reach validation. Held here so that a grammar
   * change that started accepting one of them is visible rather than silent. */
  test("Trailing comma is rejected by the grammar") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true,)")).nonEmpty)
  }

  test("Qualified annotation name is rejected by the grammar") {
    assert(messages(onClass("@Schema.AuraEnabled")).nonEmpty)
  }

  test("Bare identifier value is rejected by the grammar") {
    assert(messages(onMethod("@AuraEnabled(cacheable=foo)")).nonEmpty)
  }

  test("Array initialiser value is rejected by the grammar") {
    assert(messages(onMethod("@InvocableMethod(label={'a','b'})")).nonEmpty)
  }

  test("Double quoted value is rejected by the lexer") {
    assert(messages(onMethod("@AuraEnabled(scope=\"global\")")).nonEmpty)
  }

  test("Bare value is rejected where a name is required") {
    assert(
      message(onMethod("@AuraEnabled(true)"))
        == "Annotation parameter on AuraEnabled must be written as name=value"
    )
  }

  test("Bare value is accepted where the annotation takes one") {
    assert(messages(onClass("@SuppressWarnings('PMD')")).isEmpty)
  }

  test("Known parameter name is accepted") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true)")).isEmpty)
  }

  test("Unknown parameter name is rejected") {
    assert(
      message(onMethod("@AuraEnabled(bogus=true)"))
        == "No such property, bogus, defined on this annotation: AuraEnabled"
    )
  }

  test("Unknown parameter name is rejected on an annotation with no parameters") {
    assert(
      message(onClass("@TestVisible(foo='x')"))
        == "No such property, foo, defined on this annotation: TestVisible"
    )
  }

  test("Unknown parameter name is anchored on the name") {
    assert(anchors(onMethod("@AuraEnabled(bogus=true)")) == Seq("bogus"))
  }

  test("Parameter names are case insensitive") {
    assert(messages(onMethod("@AuraEnabled(Cacheable=true)")).isEmpty)
  }

  test("Boolean values are case insensitive") {
    assert(messages(onMethod("@AuraEnabled(cacheable=TRUE)")).isEmpty)
  }

  test("Quoted string is coerced where a Boolean is expected") {
    assert(messages(onMethod("@AuraEnabled(cacheable='yes')")).isEmpty)
  }

  test("Integer is rejected where a Boolean is expected") {
    assert(
      message(onMethod("@AuraEnabled(cacheable=0)"))
        == "Invalid value for property cacheable expected type Boolean"
    )
  }

  test("Null is rejected where a Boolean is expected") {
    assert(
      message(onMethod("@AuraEnabled(cacheable=null)"))
        == "Invalid value for property cacheable expected type Boolean"
    )
  }

  test("Boolean is rejected where a String is expected") {
    assert(
      message(onMethod("@AuraEnabled(cacheable=true scope=true)"))
        == "Invalid value for property scope expected type String"
    )
  }

  test("Wrong value type is anchored on the value") {
    assert(anchors(onMethod("@AuraEnabled(cacheable=0)")) == Seq("0"))
  }

  test("Known enum value is accepted") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true scope='global')")).isEmpty)
  }

  test("Enum values are case insensitive") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true scope='GLOBAL')")).isEmpty)
  }

  test("Unknown enum value is rejected") {
    assert(
      message(onMethod("@AuraEnabled(cacheable=true scope='bogus')"))
        == "Annotation property, scope on AuraEnabled, unknown value: bogus"
    )
  }

  test("Empty enum value is rejected") {
    assert(
      message(onMethod("@AuraEnabled(cacheable=true scope='')"))
        == "Annotation property, scope on AuraEnabled, unknown value: "
    )
  }

  test("Parameter allowed on its target is accepted") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true)")).isEmpty)
  }

  test("Parameter not allowed on fields is rejected") {
    assert(
      message(onField("@AuraEnabled(cacheable=true)"))
        == "Annotation property, cacheable on AuraEnabled, is not allowed on fields"
    )
  }

  test("Parameter not allowed on properties is rejected") {
    assert(
      message(onProperty("@AuraEnabled(cacheable=true)"))
        == "Annotation property, cacheable on AuraEnabled, is not allowed on properties"
    )
  }

  test("Valid combination is accepted") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true scope='global')")).isEmpty)
  }

  test("Scope without cacheable is rejected") {
    assert(
      message(onMethod("@AuraEnabled(scope='global')"))
        == "Invalid combination of values for properties cacheable and scope on AuraEnabled"
    )
  }

  test("Scope with cacheable false is rejected") {
    assert(
      message(onMethod("@AuraEnabled(cacheable=false scope='global')"))
        == "Invalid combination of values for properties cacheable and scope on AuraEnabled"
    )
  }

  test("Combination is anchored on the offending parameter") {
    assert(anchors(onMethod("@AuraEnabled(scope='global')")) == Seq("scope='global'"))
  }

  test("Required with default value is rejected") {
    assert(
      message(onField("@InvocableVariable(required=true defaultValue='z')"))
        == "Invalid combination of values for properties required and defaultValue on InvocableVariable"
    )
  }

  test("Required without default value is accepted") {
    assert(messages(onField("@InvocableVariable(required=true label='z')")).isEmpty)
  }

  test("Parallel test seeing all data is rejected") {
    assert(
      message(onClass("@IsTest(SeeAllData=true IsParallel=true)"))
        == "Test class annotated with @isTest(IsParallel=true) cannot also be annotated with @isTest(SeeAllData=true)"
    )
  }

  test("Parallel test not seeing all data is accepted") {
    assert(messages(onClass("@IsTest(SeeAllData=false IsParallel=true)")).isEmpty)
  }

  test("Duplicate parameters are accepted") {
    assert(messages(onMethod("@AuraEnabled(cacheable=true cacheable=false)")).isEmpty)
  }

  test("No parameter list is accepted") {
    assert(messages(onClass("@JsonAccess")).isEmpty)
  }

  test("Empty parameter list is accepted where nothing is required") {
    assert(messages(onMethod("@AuraEnabled()")).isEmpty)
  }

  test("Empty parameter list is rejected where one is required") {
    assert(
      message(onClass("@JsonAccess()"))
        == "At least one JSON serialization control parameter must be specified"
    )
  }

  test("Json access with a control parameter is accepted") {
    assert(messages(onClass("@JsonAccess(serializable='always')")).isEmpty)
  }

  test("Rest resource url must begin with a slash") {
    assert(
      message(onClass("@RestResource(urlMapping='x')"))
        == "Rest Resource url must begin with a forward slash, '/'"
    )
  }

  test("Rest resource url beginning with a slash is accepted") {
    assert(messages(onClass("@RestResource(urlMapping='/x')")).isEmpty)
  }

  test("Suppress warnings named value is accepted") {
    assert(messages(onClass("@SuppressWarnings(value='PMD')")).isEmpty)
  }

  test("Suppress warnings combined value is accepted") {
    assert(messages(onClass("@SuppressWarnings('PMD,Unused')")).isEmpty)
  }

  test("Suppress warnings unknown parameter name is rejected") {
    assert(
      message(onClass("@SuppressWarnings(bogus='PMD')"))
        == "No such property, bogus, defined on this annotation: SuppressWarnings"
    )
  }

  test("Unknown annotation is not validated") {
    assert(messages(onClass("@Bogus(anything=1)")).isEmpty)
  }

  test("Annotation with an unestablished parameter set is not validated") {
    assert(messages(onClass("@IntegrationTest(anything=1)")).isEmpty)
  }
}
