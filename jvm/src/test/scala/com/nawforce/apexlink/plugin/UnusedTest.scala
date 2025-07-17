/*
 Copyright (c) 2017 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.plugin

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.org.{OPM, OrgInfo}
import com.nawforce.apexlink.plugins.UnusedPlugin
import com.nawforce.apexlink.plugins.UnusedPlugin.onlyTestCodeReferenceText
import com.nawforce.apexlink.types.apex.{FullDeclaration, SummaryDeclaration}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class UnusedTest extends AnyFunSuite with TestHelper {

  private val testUsedOuterClassWarning =
    "only referenced by test code, consider using @isTest or @SuppressWarnings('Unused') if needed"
  private val testUsedOuterInterfaceOrEnumWarning =
    "only referenced by test code, consider using @SuppressWarnings('Unused') if needed"

  def createOrgWithUnused(root: PathLike): OPM.OrgImpl = {
    createOrgWithPlugin(root, classOf[UnusedPlugin])
  }

  def orgIssuesFor(org: OPM.OrgImpl, path: PathLike): String = {
    val messages = org.issueManager.issuesForFileInternal(path).map(_.asString()).mkString("\n")
    if (messages.nonEmpty) messages + "\n" else ""
  }

  test("Unused method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == "Unused: line 1 at 32-35: Unused public method 'void foo()'\n"
      )
    }
  }

  test("Unused global method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "global class Dummy {global void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused public method in global class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "global class Dummy {public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 32-35: Unused public method 'void foo()'\n"
      )
    }
  }

  test("Unused public method in page controller") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls"     -> "global class Dummy {public void foo() {}}",
        "TestPage.page" -> "<apex:page standardController=\"Account\" extensions=\"Dummy\"/>"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused method in 'Unused' suppressed class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "@SuppressWarnings('Unused') public class Dummy {public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused method in 'PMD' suppressed class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "@SuppressWarnings('PMD') public class Dummy {public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused method with 'Unused' suppress") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {@SuppressWarnings('Unused') public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused method with 'PMD' suppress") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {@SuppressWarnings('PMD') public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused @AuraEnabled method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {@AuraEnabled public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Used prod method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {new Dummy().foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Used prod method from test") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public void foo() {}}",
        "Foo.cls"   -> "@isTest public class Foo{ {new Dummy().foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          s"Unused: line 1 at 13-18: Unused class 'Dummy', $testUsedOuterClassWarning\n"
      )
    }
  }

  test("Used prod method from test (suppressed)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "@SuppressWarnings('Unused') public class Dummy {public void foo() {}}",
        "Foo.cls"   -> "@isTest public class Foo{ {new Dummy().foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Used prod interface method from test") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public interface Dummy {void foo();}",
        "Foo.cls"   -> "@isTest public class Foo{ {Dummy d; d.foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          s"Unused: line 1 at 17-22: Unused interface 'Dummy', $testUsedOuterInterfaceOrEnumWarning\n"
      )
    }
  }

  test("Used prod enum constant from test") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public enum Dummy {A}",
        "Foo.cls"   -> "@isTest public class Foo{ {Dummy d; d = Dummy.A;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          s"Unused: line 1 at 12-17: Unused enum 'Dummy', $testUsedOuterInterfaceOrEnumWarning\n"
      )
    }
  }

  test("Nested unused method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class Inner {public void foo() {}} }",
        "Foo.cls"   -> "public class Foo{ {Type t1 = Dummy.class; Type t2 = Dummy.Inner.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 52-55: Unused public method 'void foo()'\n"
      )
    }
  }

  test("Nested global unused method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "global class Dummy {global class Inner {global void foo() {}} }",
        "Foo.cls"   -> "public class Foo{ {Type t1 = Dummy.class; Type t2 = Dummy.Inner.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Nested @AuraEnabled method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class Inner {public @AuraEnabled void foo() {}} }",
        "Foo.cls" -> "public class Foo{ {Type t1 = Dummy.class; Type t2 = Dummy.Inner.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Method used from same method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public void foo() {foo();}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 32-35: Unused public method 'void foo()'\n"
      )
    }
  }

  test("Method used from block") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {{foo();} public void foo() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused field") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {Object a;}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 27-28: Unused field 'a'\n"
      )
    }
  }

  test("Unused global field") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "global class Dummy {global Object a;}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused @AuraEnabled field") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {@AuraEnabled Object a;}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Nested unused field") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class Inner {Object a;} }",
        "Foo.cls"   -> "public class Foo{ {Type t1 = Dummy.class; Type t2 = Dummy.Inner.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 47-48: Unused field 'a'\n"
      )
    }
  }

  test("Nested global unused field") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "global class Dummy {global class Inner {global Object a;} }",
        "Foo.cls"   -> "public class Foo{ {Type t1 = Dummy.class; Type t2 = Dummy.Inner.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Nested @AuraEnabled unused field") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class Inner {public @AuraEnabled Object a;} }",
        "Foo.cls"   -> "public class Foo{ {Type t1 = Dummy.class; Type t2 = Dummy.Inner.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Field used from method") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {Object a; void foo(){foo(); a = null;}}",
        "Foo.cls"   -> "public class Foo{ {new Dummy().foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Field used from block") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {{a = null;} Object a;}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused empty class") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused empty interface") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public interface Dummy {}")) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused empty interface extending another interface ") {
    FileSystemHelper.run(
      Map("A.cls" -> "public interface A extends B {}", "B.cls" -> "public interface B {}")
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("A.cls")).isEmpty)
      assert(orgIssuesFor(org, root.join("B.cls")).isEmpty)
    }
  }

  test("Unused empty enum") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public enum Dummy {}")) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused empty test class") {
    FileSystemHelper.run(Map("Dummy.cls" -> "@isTest public class Dummy {}")) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused class") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {Object a; void func() {}}")) {
      root: PathLike =>
        val org = createOrgWithUnused(root)
        assert(
          orgIssuesFor(org, root.join("Dummy.cls")) ==
            "Unused: line 1 at 13-18: Unused class 'Dummy'\n"
        )
    }
  }

  test("Unused interface") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public interface Dummy {void func();}")) {
      root: PathLike =>
        val org = createOrgWithUnused(root)
        assert(
          orgIssuesFor(org, root.join("Dummy.cls")) ==
            "Unused: line 1 at 17-22: Unused interface 'Dummy'\n"
        )
    }
  }

  test("Unused interface extending another interface") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public interface Dummy extends Foo {void func();}",
        "Foo.cls"   -> "public interface Foo {void func();}"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 17-22: Unused interface 'Dummy'\n"
      )
      assert(
        orgIssuesFor(org, root.join("Foo.cls")) ==
          "Unused: line 1 at 27-31: Unused public method 'void func()'\n"
      )
    }
  }

  test("Unused interface extending an interface extending another interface") {
    // This is rather specific test case to cover a bug that caused a stack overflow due to
    // incorrectly setup shadows relationship between the methods
    FileSystemHelper.run(
      Map(
        "C.cls" -> "public interface C extends B {void other();}",
        "B.cls" -> "public interface B extends A {void func();}",
        "A.cls" -> "public interface A {void func();}"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("C.cls")) ==
          "Unused: line 1 at 17-18: Unused interface 'C'\n"
      )
      assert(
        orgIssuesFor(org, root.join("B.cls")) ==
          "Unused: line 1 at 35-39: Unused public method 'void func()'\n"
      )
      assert(
        orgIssuesFor(org, root.join("A.cls")) ==
          "Unused: line 1 at 25-29: Unused public method 'void func()'\n"
      )
    }
  }

  test("Unused enum") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public enum Dummy {A}")) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 12-17: Unused enum 'Dummy'\n"
      )
    }
  }

  test("Non-empty class used from prod code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {void func() {}}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 25-29: Unused private method 'void func()'\n"
      )
    }
  }

  test("Non-empty interface used from prod code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public interface Dummy {void func();}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 29-33: Unused public method 'void func()'\n"
      )
    }
  }

  test("Non-empty enum used from prod code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public enum Dummy {A}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 19-20: Unused field 'A'\n"
      )
    }
  }

  test("Non-empty class used from test code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {void func() {}}",
        "Foo.cls"   -> "@isTest public class Foo{ @isTest void func() {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          s"Unused: line 1 at 13-18: Unused class 'Dummy', $testUsedOuterClassWarning\n"
      )
    }
  }

  test("Non-empty interface used from test code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public interface Dummy {void func();}",
        "Foo.cls"   -> "@isTest public class Foo{ @isTest void func() {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          s"Unused: line 1 at 17-22: Unused interface 'Dummy', $testUsedOuterInterfaceOrEnumWarning\n"
      )
    }
  }

  test("Non-empty enum used from test code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public enum Dummy {A}",
        "Foo.cls"   -> "@isTest public class Foo{ @isTest void func() {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          s"Unused: line 1 at 12-17: Unused enum 'Dummy', $testUsedOuterInterfaceOrEnumWarning\n"
      )
    }
  }

  test("Nested empty unused class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class Inner {} }",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 33-38: Unused class 'Dummy.Inner'\n"
      )
    }
  }

  test("Nested empty unused test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "@isTest public class Dummy {public class Inner {} }",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 41-46: Unused class 'Dummy.Inner'\n"
      )
    }
  }

  test("Nested empty test class used from test code") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "@isTest public class Dummy {{Inner i; i.toString();} public class Inner {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Nested unused class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class Inner {Object a; void func() {}} }",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls")) ==
          "Unused: line 1 at 33-38: Unused class 'Dummy.Inner'\n"
      )
    }
  }

  test("Unused catch") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {{ try {} catch(Exception e) {} }}",
        "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused this call argument") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public Dummy(Object a) {this(a, null);} public Dummy(Object a, Object b) {a = b;} }",
        "Foo.cls" -> "public class Foo{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused super call argument") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy extends Foo {public Dummy(Object a) {super(a, null);}  }",
        "Foo.cls" -> "virtual public class Foo{ {Type t = Dummy.class;} public Foo(Object a, Object b) {a = b;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  def assertIsFullDeclaration(
    pkg: OPM.PackageImpl,
    name: String,
    namespace: Option[Name] = None
  ): Unit = {
    assert(
      pkg.orderedModules.head
        .findModuleType(TypeName(Name(name)).withNamespace(namespace))
        .head
        .isInstanceOf[FullDeclaration]
    )
  }

  def assertIsSummaryDeclaration(
    pkg: OPM.PackageImpl,
    name: String,
    namespace: Option[Name] = None
  ): Unit = {
    assert(
      pkg.orderedModules.head
        .findModuleType(TypeName(Name(name)).withNamespace(namespace))
        .head
        .isInstanceOf[SummaryDeclaration]
    )
  }

  test("Used method on summary type") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "Dummy.cls"  -> "public class Dummy {public static void foo() {}}",
          "Caller.cls" -> "public class Caller {{Dummy.Foo();}}"
        )
      ) { root: PathLike =>
        // Setup as cached
        val org = createOrgWithUnused(root)
        val pkg = org.unmanaged
        assertIsFullDeclaration(pkg, "Dummy")
        assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
        org.flush()

        val org2 = createOrgWithUnused(root)
        val pkg2 = org2.unmanaged
        assertIsSummaryDeclaration(pkg2, "Dummy")
        OrgInfo.current.withValue(org2) {
          assert(orgIssuesFor(org2, root.join("Dummy.cls")).isEmpty)
        }
      }
    }
  }

  test("Unused method on summary type") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "Dummy.cls" -> "public class Dummy {public void foo() {}}",
          "Foo.cls"   -> "public class Foo{ {Type t = Dummy.class;} }"
        )
      ) { root: PathLike =>
        // Setup as cached
        val org = createOrgWithUnused(root)
        val pkg = org.unmanaged
        assertIsFullDeclaration(pkg, "Dummy")
        assert(
          orgIssuesFor(org, root.join("Dummy.cls")) ==
            "Unused: line 1 at 32-35: Unused public method 'void foo()'\n"
        )
        org.flush()

        val org2 = createOrgWithUnused(root)
        val pkg2 = org2.unmanaged
        assertIsSummaryDeclaration(pkg2, "Dummy")
        OrgInfo.current.withValue(org2) {
          assert(
            orgIssuesFor(org2, root.join("Dummy.cls")) ==
              "Unused: line 1 at 32-35: Unused public method 'void foo()'\n"
          )
        }
      }
    }
  }

  test("Trigger referencing class") {
    FileSystemHelper.run(
      Map(
        "Foo.cls" -> "public class Foo {public static String bar;}",
        "Dummy.trigger" ->
          """trigger Dummy on Account (before insert) {
            |  System.debug(Foo.bar);
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(org.issues.isEmpty)

      OrgInfo.current.withValue(org) {
        assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
      }
    }
  }

  test("Method referenced from external function call ") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |"namespace": "pkg",
          |"packageDirectories": [{"path": "pkg"}],
          |"plugins": {"dependencies": [{"namespace": "ext"}]}
          |}""".stripMargin,
        "pkg/Dummy.cls" -> "public class Dummy {public static void foo() {}}",
        "pkg/Other.cls" -> "public class Other {public void bar() {ext.func(Dummy.foo());}}"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Unused local var") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy { {Object a;} }")) {
      root: PathLike =>
        createOrgWithUnused(root)
        assert(
          getMessages(root.join("Dummy.cls")) ==
            "Unused: line 1 at 29-30: Unused local variable 'a'\n"
        )
    }
  }

  test("Unused local var assignment") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy { {Object a; a=null;} }")) {
      root: PathLike =>
        createOrgWithUnused(root)
        assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Used local var for-loop bug") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |"packageDirectories": [{"path": "force-app"}],
            |"plugins": {"dependencies": [{"namespace": "ext"}]}
            |}""".stripMargin,
        "force-app/Dummy.cls" -> "public class Dummy { { List<ext__Something__c> myList; for(ext__Something__c a : myList) {} } }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("force-app/Dummy.cls")).isEmpty)
    }
  }

  test("Page controller & extension is used") {
    FileSystemHelper.run(
      Map(
        "Test.page"      -> "<apex:page controller='Controller' extensions='Extension'/>",
        "Controller.cls" -> "public class Controller { }",
        "Extension.cls"  -> "public class Extension { }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(org.issues.isEmpty)

      OrgInfo.current.withValue(org) {
        assert(orgIssuesFor(org, root.join("Controller.cls")).isEmpty)
        assert(orgIssuesFor(org, root.join("Extension.cls")).isEmpty)
      }
    }
  }

  test("Component controller is used") {
    FileSystemHelper.run(
      Map(
        "Test.component" -> "<apex:component controller='Controller'/>",
        "Controller.cls" -> "public class Controller { }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(org.issues.isEmpty)

      OrgInfo.current.withValue(org) {
        assert(orgIssuesFor(org, root.join("Controller.cls")).isEmpty)
      }
    }
  }

  test("Queueable via abstract") {
    FileSystemHelper.run(
      Map(
        "Base.cls" -> "public abstract class Base implements Queueable { {Type t = Dummy.class;} }",
        "Dummy.cls" -> "public class Dummy extends Base {public void execute(QueueableContext context) {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      OrgInfo.current.withValue(org) {
        assert(getMessages(root.join("Dummy.cls")).isEmpty)
      }
    }
  }

  test("Unused method only referenced in test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public static void foo() {}}",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }",
        "Foo.cls"   -> "@isTest public class Foo{ {Dummy.foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == s"Unused: line 1 at 39-42: Unused public method 'void foo()', $onlyTestCodeReferenceText\n"
      )
    }
  }

  test("Unused method suppress by @TestVisible") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {@TestVisible public static void foo() {}}",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")) == "")
    }
  }

  test("Unused field only referenced in test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public static String foo;}",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }",
        "Foo.cls"   -> "@isTest public class Foo{ {String b = Dummy.foo;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == s"Unused: line 1 at 41-44: Unused field 'foo', $onlyTestCodeReferenceText\n"
      )
    }
  }

  test("Unused field suppress by @TestVisible") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {@TestVisible public static String foo;}",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")) == "")
    }
  }

  test("Unused inner class only referenced in test class") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class foo {}}",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }",
        "Foo.cls"   -> "@isTest public class Foo{ {Type t = Dummy.foo;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == "Unused: line 1 at 33-36: Unused class 'Dummy.foo'\n"
      )
    }
  }

  test("Unused method only referenced in test class rolls up") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public static void foo() {}}",
        "Foo.cls"   -> "@isTest public class Foo{ {Dummy.foo();} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == s"Unused: line 1 at 13-18: Unused class 'Dummy', $testUsedOuterClassWarning\n"
      )
    }
  }

  test("Unused field only referenced in test class rolls up") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public static String foo;}",
        "Foo.cls"   -> "@isTest public class Foo{ {String b = Dummy.foo;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == s"Unused: line 1 at 13-18: Unused class 'Dummy', $testUsedOuterClassWarning\n"
      )
    }
  }

  test("Unused inner class only referenced in test class rolls up") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public class foo {}}",
        "Foo.cls"   -> "@isTest public class Foo{ {Type t = Dummy.foo;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(
        orgIssuesFor(org, root.join("Dummy.cls"))
          == s"Unused: line 1 at 13-18: Unused class 'Dummy', $testUsedOuterClassWarning\n"
      )
    }
  }

  test("Unused enum constant bug") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {public enum A { B } {switch on a { when B {}} }}",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithUnused(root)
      assert(orgIssuesFor(org, root.join("Dummy.cls")) == "")
    }
  }

  test("Local var not unused when bound in string literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { {Object a; System.debug(':a');} }",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Instance field not unused when bound in string literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {Object a; { System.debug(':a');} }",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Static field not unused when bound in string literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {static Object a; { System.debug(':a');} }",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Outer static field not unused when bound in string literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {static Object a; class Foo { { System.debug(':a');} } }",
        "Bar.cls" -> "public class Bar{ {Type t = Dummy.Foo.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Local var not unused when bound in SOQL literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { {Object a; System.debug([Select Id from Account where Id=:a]); } }",
        "Bar.cls" -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Local var not unused when in upsert stmt") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { {Account a; upsert new List<Account>{a}; } }",
        "Bar.cls"   -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Instance field not unused when bound in SOQL literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {Object a; { System.debug([Select Id from Account where Id=:a]);} }",
        "Bar.cls" -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Static field not unused when bound in SOQL literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {static Object a; { System.debug([Select Id from Account where Id=:a]);} }",
        "Bar.cls" -> "public class Bar{ {Type t = Dummy.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

  test("Outer static field not unused when bound in SOQL literal") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy {static Object a; class Foo { { System.debug([Select Id from Account where Id=:a]);} } }",
        "Bar.cls" -> "public class Bar{ {Type t = Dummy.Foo.class;} }"
      )
    ) { root: PathLike =>
      createOrgWithUnused(root)
      assert(getMessages(root.join("Dummy.cls")).isEmpty)
    }
  }

}
