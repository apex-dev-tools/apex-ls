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
import com.nawforce.apexlink.api.ServerOps
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

/** Accessibility of nested Apex types written explicitly in source, see issue #341.
  *
  * Both parser adapters are exercised for every case as declaration level types are constructed by
  * the outline parser when it is in use, while statement and expression types always come from the
  * ANTLR parser.
  */
class NestedTypeVisibilityTest extends AnyFunSuite with TestHelper {

  private val parsers = Seq("outlinemulti", "antlr")

  private val hiddenClass = "global class Target { private class Hidden {} }"
  private val hiddenClassWithStatic =
    "global class Target { private class Hidden { public static Integer get() { return 1; } } }"
  private val shownClass  = "global class Target { public class Shown {} }"
  private val hiddenIface = "global class Target { private interface Hidden {} }"
  private val shownIface  = "global class Target { public interface Shown {} }"
  private val hiddenEnum  = "global class Target { private enum Hidden { A, B } }"
  private val testVisible = "global class Target { @TestVisible private class Hidden {} }"
  private val hiddenExcept =
    "global class Target { private class HiddenException extends Exception {} }"

  private def messagesFor(classes: Map[String, String], parser: String, file: String): String = {
    try {
      ServerOps.setAutoFlush(false)
      FileSystemHelper.run(classes) { root: PathLike =>
        this.root = root
        createOrg(root, Some(parser))
        getMessages(root.join(file))
      }
    } finally {
      ServerOps.setAutoFlush(true)
    }
  }

  private def visibilityIssues(classes: Map[String, String], parser: String): Seq[String] =
    messagesFor(classes, parser, "Dummy.cls").linesIterator
      .filter(_.contains("Type is not visible"))
      .toSeq

  /** Expected diagnostic for each place the given identifier is written in the source. This keeps
    * the expected ranges tied to the source rather than to hand counted offsets.
    */
  private def expectedAt(source: String, identifier: String, typeName: String): Seq[String] = {
    val matcher = new Regex(s"\\b$identifier\\b")
    source
      .split("\n", -1)
      .zipWithIndex
      .flatMap { case (line, index) =>
        matcher
          .findAllMatchIn(line)
          .map(m =>
            s"Error: line ${index + 1} at ${m.start}-${m.end}: Type is not visible: $typeName"
          )
      }
      .toSeq
  }

  /** Assert the visibility diagnostics reported for Dummy.cls, in both parser adapters. */
  private def assertHidden(target: String, dummy: String, identifier: String = "Hidden"): Unit = {
    val classes  = Map("Target.cls" -> target, "Dummy.cls" -> dummy)
    val expected = expectedAt(dummy, identifier, s"Target.$identifier")
    assert(expected.nonEmpty, "test source does not mention the hidden type")
    parsers.foreach(parser =>
      assert(visibilityIssues(classes, parser) == expected, s"parser '$parser'")
    )
  }

  /** Assert Dummy.cls has no diagnostics at all, in both parser adapters. */
  private def assertClean(target: String, dummy: String): Unit = {
    val classes = Map("Target.cls" -> target, "Dummy.cls" -> dummy)
    parsers.foreach(parser =>
      assert(messagesFor(classes, parser, "Dummy.cls").isEmpty, s"parser '$parser'")
    )
  }

  /** Assert only the visibility diagnostics, ignoring any other analysis of the same source. */
  private def assertHiddenCount(target: String, dummy: String, count: Int): Unit = {
    val classes = Map("Target.cls" -> target, "Dummy.cls" -> dummy)
    parsers.foreach(parser =>
      assert(visibilityIssues(classes, parser).size == count, s"parser '$parser'")
    )
  }

  // Declared types

  test("Field type") {
    assertHidden(hiddenClass, "global class Dummy { Target.Hidden a; }")
  }

  test("Field type (accessible)") {
    assertClean(shownClass, "global class Dummy { Target.Shown a; }")
  }

  test("Static field type") {
    assertHidden(hiddenClass, "global class Dummy { static Target.Hidden a; }")
  }

  test("Property type") {
    assertHidden(hiddenClass, "global class Dummy { Target.Hidden a { get; set; } }")
  }

  test("Property type (accessible)") {
    assertClean(shownClass, "global class Dummy { Target.Shown a { get; set; } }")
  }

  test("Local variable type") {
    assertHidden(hiddenClass, "global class Dummy { void func() { Target.Hidden a; } }")
  }

  test("Local variable type (accessible)") {
    assertClean(shownClass, "global class Dummy { void func() { Target.Shown a; } }")
  }

  test("Method return type") {
    assertHidden(hiddenClass, "global class Dummy { Target.Hidden func() { return null; } }")
  }

  test("Method return type (accessible)") {
    assertClean(shownClass, "global class Dummy { Target.Shown func() { return null; } }")
  }

  test("Method parameter type") {
    assertHidden(hiddenClass, "global class Dummy { void func(Target.Hidden a) {} }")
  }

  test("Method parameter type (accessible)") {
    assertClean(shownClass, "global class Dummy { void func(Target.Shown a) {} }")
  }

  test("Constructor parameter type") {
    assertHidden(hiddenClass, "global class Dummy { public Dummy(Target.Hidden a) {} }")
  }

  test("Interface method return & parameter types") {
    assertHidden(hiddenClass, "global interface Dummy { Target.Hidden func(Target.Hidden a); }")
  }

  // Construction & expressions

  test("Object construction") {
    assertHidden(
      hiddenClass,
      "global class Dummy { void func() { Object a = new Target.Hidden(); } }"
    )
  }

  test("Object construction (accessible)") {
    assertClean(shownClass, "global class Dummy { void func() { Object a = new Target.Shown(); } }")
  }

  test("Generic construction") {
    assertHidden(
      hiddenClass,
      "global class Dummy { void func() { Object a = new List<Target.Hidden>(); } }"
    )
  }

  test("Cast type") {
    assertHidden(
      hiddenClass,
      "global class Dummy { void func(Object o) { Object a = (Target.Hidden)o; } }"
    )
  }

  test("instanceof type") {
    assertHidden(
      hiddenClass,
      "global class Dummy { void func(Object o) { Boolean a = o instanceof Target.Hidden; } }"
    )
  }

  test("Type literal") {
    assertHidden(
      hiddenClass,
      "global class Dummy { void func() { Type a = Target.Hidden.class; } }"
    )
  }

  test("Static type qualifier") {
    assertHidden(
      hiddenClassWithStatic,
      "global class Dummy { void func() { Integer a = Target.Hidden.get(); } }"
    )
  }

  test("Static type qualifier (accessible)") {
    assertClean(
      "global class Target { public class Shown { public static Integer get() { return 1; } } }",
      "global class Dummy { void func() { Integer a = Target.Shown.get(); } }"
    )
  }

  // Statements

  test("Basic for loop variable type") {
    assertHidden(
      hiddenClass,
      "global class Dummy { void func() { for (Target.Hidden a = null; a == null;) {} } }"
    )
  }

  test("Enhanced for loop variable type") {
    assertHiddenCount(
      hiddenClass,
      "global class Dummy { void func(List<Object> l) { for (Target.Hidden a : l) {} } }",
      1
    )
  }

  // Apex only enforces the visibility of a caught type when the handler uses the exception, see
  // issue #549. A handler that ignores the exception compiles against a type it cannot otherwise
  // name, so the diagnostic is withheld unless the variable is referenced.

  test("Catch exception type") {
    assertHidden(
      hiddenExcept,
      "global class Dummy { void func() { try {} " +
        "catch (Target.HiddenException e) { System.debug(e); } } }",
      "HiddenException"
    )
  }

  test("Catch exception type (accessible)") {
    assertClean(
      "global class Target { public class ShownException extends Exception {} }",
      "global class Dummy { void func() { try {} " +
        "catch (Target.ShownException e) { System.debug(e); } } }"
    )
  }

  test("Catch exception type is not diagnosed when the exception is unused") {
    Seq(
      "try {} catch (Target.HiddenException e) {}",
      "try {} catch (Target.HiddenException e) { System.debug('unrelated'); }",
      "try {} catch (Target.HiddenException e) { throw new IllegalArgumentException('x'); }"
    ).foreach(body =>
      assertHiddenCount(hiddenExcept, s"global class Dummy { void func() { $body } }", 0)
    )
  }

  test("Catch exception type is diagnosed for any use of the exception") {
    Seq(
      "try {} catch (Target.HiddenException e) { throw e; }",
      "try {} catch (Target.HiddenException e) { System.debug(e.getMessage()); }",
      "try {} catch (Target.HiddenException e) { Object o = e; }",
      "try {} catch (Target.HiddenException e) { if (true) { System.debug(e); } }",
      "try {} catch (Target.HiddenException e) { for (Integer i = 0; i < 1; i++) { throw e; } }"
    ).foreach(body =>
      assertHiddenCount(hiddenExcept, s"global class Dummy { void func() { $body } }", 1)
    )
  }

  test("Catch exception type is diagnosed per clause that uses its exception") {
    assertHiddenCount(
      "global class Target { private class HiddenException extends Exception {} " +
        "private class OtherHiddenException extends Exception {} }",
      "global class Dummy { void func() { try {} " +
        "catch (Target.HiddenException e) {} " +
        "catch (Target.OtherHiddenException e2) { System.debug(e2); } } }",
      1
    )
  }

  test("Enum qualified switch case") {
    assertHiddenCount(
      hiddenEnum,
      "global class Dummy { void func(Target.Hidden e) { switch on e { when A {} when else {} } } }",
      1
    )
  }

  // Extends & implements

  test("Implements a hidden interface") {
    assertHidden(hiddenIface, "global class Dummy implements Target.Hidden {}")
  }

  test("Implements a visible interface (accessible)") {
    assertClean(shownIface, "global class Dummy implements Target.Shown {}")
  }

  test("Interface extends a hidden interface") {
    assertHidden(hiddenIface, "global interface Dummy extends Target.Hidden {}")
  }

  // Natures

  test("Nested interface as a declared type") {
    assertHidden(hiddenIface, "global class Dummy { Target.Hidden a; }")
  }

  test("Nested enum as a declared type") {
    assertHidden(hiddenEnum, "global class Dummy { Target.Hidden a; }")
  }

  test("Nested enum as a parameter type") {
    assertHidden(hiddenEnum, "global class Dummy { void func(Target.Hidden a) {} }")
  }

  test("Nested enum as a return type") {
    assertHidden(hiddenEnum, "global class Dummy { Target.Hidden func() { return null; } }")
  }

  // Generics and arrays

  test("Generic argument is reported at the component") {
    assertHidden(hiddenClass, "global class Dummy { List<Target.Hidden> a; }")
  }

  test("Recursive generic argument is reported at the component") {
    assertHidden(hiddenClass, "global class Dummy { Map<String, List<Target.Hidden>> a; }")
  }

  test("Array element type is reported at the component") {
    assertHidden(hiddenClass, "global class Dummy { Target.Hidden[] a; }")
  }

  test("Array within a generic is reported at the component") {
    assertHidden(hiddenClass, "global class Dummy { Map<String, Target.Hidden[]> a; }")
  }

  test("Multiple declarators of one written type report once") {
    assertHiddenCount(hiddenClass, "global class Dummy { Target.Hidden a, b, c; }", 1)
  }

  test("Multiple written references each report") {
    assertHidden(hiddenClass, "global class Dummy { Target.Hidden a; Target.Hidden b; }")
  }

  // Same file access

  test("Same file nested access is allowed") {
    assert(
      typeDeclarations(
        Map(
          "Dummy.cls" ->
            "global class Dummy { private class Hidden {} class Peer { Hidden a; Dummy.Hidden b; } }"
        )
      ).nonEmpty
    )
    assert(dummyIssues.isEmpty)
  }

  test("Same file nested access from the outer type is allowed") {
    assert(
      typeDeclarations(
        Map("Dummy.cls" -> "global class Dummy { private class Hidden {} Hidden a; }")
      ).nonEmpty
    )
    assert(dummyIssues.isEmpty)
  }

  // Test context

  test("@TestVisible private type from a test class") {
    assertClean(testVisible, "@isTest global class Dummy { Target.Hidden a; }")
  }

  test("@TestVisible private type from a nested type of a test class") {
    assertClean(testVisible, "@isTest global class Dummy { class Inner { Target.Hidden a; } }")
  }

  test("@TestVisible private type from a normal class") {
    assertHidden(testVisible, "global class Dummy { Target.Hidden a; }")
  }

  test("Private type without @TestVisible from a test class") {
    assertHidden(hiddenClass, "@isTest global class Dummy { Target.Hidden a; }")
  }

  test("@TestVisible private interface from a test class") {
    assertClean(
      "global class Target { @TestVisible private interface Hidden {} }",
      "@isTest global class Dummy { Target.Hidden a; }"
    )
  }

  test("@TestVisible private enum from a test class") {
    assertClean(
      "global class Target { @TestVisible private enum Hidden { A, B } }",
      "@isTest global class Dummy { Target.Hidden a; }"
    )
  }

  // Subtypes get no extra access

  test("Subclass of the outer type can not see a private nested type") {
    assertHidden(
      "global virtual class Target { private class Hidden {} }",
      "global class Dummy extends Target { Target.Hidden a; }"
    )
  }

  // Exclusions

  test("Derived field type is not diagnosed") {
    assertClean(
      "global class Target { private class Hidden { public Integer value; } " +
        "@TestVisible private static Hidden instance; }",
      "@isTest global class Dummy { static void func() { Integer a = Target.instance.value; } }"
    )
  }

  test("Derived method return type is not diagnosed") {
    assertClean(
      "global class Target { private class Hidden { public Integer value; } " +
        "@TestVisible private static Hidden make() { return null; } }",
      "@isTest global class Dummy { static void func() { Integer a = Target.make().value; } }"
    )
  }

  test("Top level types are never diagnosed") {
    assertClean("global class Target {}", "global class Dummy { Target a; }")
  }

  // Unqualified names inherited from a superclass in another file
  //
  // Occurrences are only collected for qualified names, since an unqualified name is either a top
  // level type or a nested type that is visible from where it is written -- with one exception,
  // a nested type Apex resolves through the superclass. These are not diagnosed at type reference
  // sites. The tests below pin that gap; it is a false negative, never a false positive, and the
  // qualified form is unaffected. Sites that check an already resolved declaration rather than a
  // written type reference, construction and catch clauses, do still report.

  private val inheritedTarget =
    "global virtual class Target { private class Hidden { Hidden() {} } " +
      "private class HiddenException extends Exception {} }"

  test("Qualified inherited private nested type is diagnosed") {
    assertHidden(inheritedTarget, "global class Dummy extends Target { Target.Hidden a; }")
  }

  test("Unqualified inherited private nested type is not diagnosed at type reference sites") {
    Seq(
      "Hidden a;",
      "List<Hidden> a;",
      "void func() { Hidden a; }",
      "void func(Hidden a) {}",
      "Hidden func() { return null; }",
      "void func(Object o) { Object a = (Hidden)o; }",
      "void func(Object o) { Boolean a = o instanceof Hidden; }",
      "void func() { Type a = Hidden.class; }"
    ).foreach(body =>
      assertHiddenCount(inheritedTarget, s"global class Dummy extends Target { $body }", 0)
    )
  }

  test("Unqualified inherited private nested type is diagnosed on construction") {
    assertHiddenCount(
      inheritedTarget,
      "global class Dummy extends Target { void func() { Object a = new Hidden(); } }",
      1
    )
  }

  test("Unqualified inherited private nested type is diagnosed in a catch clause") {
    assertHiddenCount(
      inheritedTarget,
      "global class Dummy extends Target { void func() { try {} " +
        "catch (HiddenException e) { System.debug(e); } } }",
      1
    )
  }

  test("Unqualified same file nested type is never diagnosed") {
    assert(
      typeDeclarations(
        Map(
          "Dummy.cls" ->
            "global class Dummy { private class Hidden {} class Peer extends Dummy { Hidden a; } }"
        )
      ).nonEmpty
    )
    assert(!dummyIssues.contains("not visible"))
  }

  test("Nested non-Apex declarations are never diagnosed") {
    // A Visualforce component is a nested type of the Component declaration but is not an Apex
    // declaration, applying the rule to it caused false positives during investigation
    parsers.foreach(parser =>
      FileSystemHelper.run(
        Map(
          "Test.component" -> "<apex:component/>",
          "Page.page"      -> "<apex:page/>",
          "Dummy.cls" ->
            "global class Dummy { Component.Test c; PageReference p = Page.Page; }"
        )
      ) { root: PathLike =>
        this.root = root
        createOrg(root, Some(parser))
        assert(getMessages(root.join("Dummy.cls")).isEmpty, s"parser '$parser'")
      }
    )
  }

  test("SObject switch value type is checked") {
    assertHiddenCount(
      hiddenClass,
      "global class Dummy { void func(SObject s) { switch on s { when Target.Hidden h {} " +
        "when else {} } } }",
      1
    )
  }
}
