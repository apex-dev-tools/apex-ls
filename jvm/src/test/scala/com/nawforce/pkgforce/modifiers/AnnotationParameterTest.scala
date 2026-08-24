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

import scala.collection.compat.immutable.ArraySeq

/** Coverage of the ANTLR adapter, which is used on outline parser failure, on incremental refresh,
  * for triggers and by ApexClassVisitor.
  */
class AnnotationParameterTest extends AnyFunSuite {

  private def source(annotation: String): String = s"$annotation public class Dummy {}"

  private def parse(annotation: String): (String, CodeParser) = {
    val content = source(annotation)
    val parser  = CodeParser(Path("Dummy.cls"), SourceData(content))
    (content, parser)
  }

  private def parameters(annotation: String): Option[ArraySeq[AnnotationParameter]] = {
    val (_, parser) = parse(annotation)
    val result      = parser.parseClass()
    assert(result.issues.isEmpty, result.issues.mkString("\n"))
    ApexModifiers.toAnnotationParameters(
      parser,
      result.value.typeDeclaration().modifier(0).annotation()
    )
  }

  private def modifiers(annotation: String): ArraySeq[Modifier] = {
    val (_, parser) = parse(annotation)
    val result      = parser.parseClass()
    assert(result.issues.isEmpty, result.issues.mkString("\n"))
    ApexNode(parser, result.value).get.modifiers
  }

  private def written(annotation: String, location: Option[Location]): String =
    Location.extract(source(annotation), location.get)

  test("No parentheses has no parameter list") {
    assert(parameters("@IsTest").isEmpty)
  }

  test("Empty parentheses has an empty parameter list") {
    assert(parameters("@IsTest()").contains(AnnotationParameter.emptyArraySeq))
  }

  test("Bare value is unnamed") {
    assert(
      parameters("@SuppressWarnings('PMD')").get.map(p => (p.name, p.value, p.precedingSeparator))
        == ArraySeq((None, "'PMD'", None))
    )
  }

  test("Named value keeps its name") {
    assert(
      parameters("@IsTest(SeeAllData=true)").get.map(p => (p.name, p.value, p.precedingSeparator))
        == ArraySeq((Some("SeeAllData"), "true", None))
    )
  }

  test("Whitespace separated parameters") {
    assert(
      parameters("@IsTest(SeeAllData=true IsParallel=false)").get
        .map(p => (p.name, p.value, p.precedingSeparator))
        == ArraySeq(
          (Some("SeeAllData"), "true", None),
          (Some("IsParallel"), "false", Some(AnnotationParameterSeparator.Whitespace))
        )
    )
  }

  test("Comma separated parameters are retained, not rejected") {
    assert(
      parameters("@IsTest(SeeAllData=true, IsParallel=false)").get
        .map(p => (p.name, p.value, p.precedingSeparator))
        == ArraySeq(
          (Some("SeeAllData"), "true", None),
          (Some("IsParallel"), "false", Some(AnnotationParameterSeparator.Comma))
        )
    )
  }

  test("Bare value is located") {
    val annotation = "@SuppressWarnings('PMD')"
    val parameter  = parameters(annotation).get.head
    assert(parameter.nameLocation.isEmpty)
    assert(written(annotation, parameter.valueLocation) == "'PMD'")
    assert(written(annotation, parameter.location) == "'PMD'")
  }

  test("Named value is located") {
    val annotation = "@IsTest(SeeAllData=true)"
    val parameter  = parameters(annotation).get.head
    assert(written(annotation, parameter.nameLocation) == "SeeAllData")
    assert(written(annotation, parameter.valueLocation) == "true")
    assert(written(annotation, parameter.location) == "SeeAllData=true")
  }

  test("Annotation with parameters still resolves by name") {
    assert(modifiers("@IsTest(SeeAllData=true)").contains(ISTEST_ANNOTATION))
  }

  test("Suppress warnings bare value") {
    assert(modifiers("@SuppressWarnings('PMD')").contains(SUPPRESS_WARNINGS_ANNOTATION_PMD))
  }

  test("Suppress warnings combined values") {
    val mods = modifiers("@SuppressWarnings('PMD,Unused')")
    assert(mods.contains(SUPPRESS_WARNINGS_ANNOTATION_PMD))
    assert(mods.contains(SUPPRESS_WARNINGS_ANNOTATION_UNUSED))
  }

  test("Suppress warnings named value") {
    assert(modifiers("@SuppressWarnings(value='PMD')").contains(SUPPRESS_WARNINGS_ANNOTATION_PMD))
  }

  test("Suppress warnings without a value suppresses nothing") {
    val mods = modifiers("@SuppressWarnings")
    assert(!mods.contains(SUPPRESS_WARNINGS_ANNOTATION_PMD))
    assert(!mods.contains(SUPPRESS_WARNINGS_ANNOTATION_UNUSED))
  }

  test("Suppress warnings with an unknown name suppresses nothing") {
    assert(!modifiers("@SuppressWarnings(bogus='PMD')").contains(SUPPRESS_WARNINGS_ANNOTATION_PMD))
  }
}
