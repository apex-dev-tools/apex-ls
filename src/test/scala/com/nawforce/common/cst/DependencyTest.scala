/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.common.cst

import com.nawforce.common.api.{Name, Org, ServerOps, TypeName}
import com.nawforce.common.documents.{ApexClassDocument, MetadataDocument}
import com.nawforce.common.names.TypeNames
import com.nawforce.common.org.OrgImpl
import com.nawforce.common.path.PathLike
import com.nawforce.common.types.core.TypeDeclaration
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class DependencyTest extends AnyFunSuite with BeforeAndAfter {
  private var defaultOrg: OrgImpl = new OrgImpl
  private var root: PathLike = _

  def typeDeclarations(classes: Map[String, String]): Seq[TypeDeclaration] = {
    FileSystemHelper.run(classes) { root: PathLike =>
      this.root = root
      OrgImpl.current.withValue(defaultOrg) {
        defaultOrg.unmanaged.deployClasses(
          classes.map(p => MetadataDocument(root.join(p._1)).get.asInstanceOf[ApexClassDocument]).toSeq)
        defaultOrg.unmanaged.findTypes(classes.keys.map(k => TypeName(Name(k.replaceAll("\\.cls$", "")))).toSeq)
      }
    }
  }

  before {
    defaultOrg = new OrgImpl
    root = null
    ServerOps.setParsedDataCaching(false)
  }

  test("Empty class has no imports") {
    val tds = typeDeclarations(Map("Dummy.cls" -> "public class Dummy {}"))
    assert(tds.head.dependencies().isEmpty)
  }

  test("Class depends on superclass") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy extends A {}",
      "A.cls" -> "public virtual class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Class depends on interface") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy implements A, B {}",
      "A.cls" -> "public interface A {}",
      "B.cls" -> "public interface B {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Interface depends on interface") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public interface Dummy extends A, B {}",
      "A.cls" -> "public interface A {}",
      "B.cls" -> "public interface B {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Empty inner class has no dependencies") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner {} }"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.dependencies().isEmpty)
  }

  test("Inner class depends on superclass") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner extends A {} }",
      "A.cls" -> "public virtual class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Inner class depends on interface") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner implements A,B {} }",
      "A.cls" -> "public interface A {}",
      "B.cls" -> "public interface B {}",
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Inner interface depends on interface") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { interface Inner extends A, B {} }",
      "A.cls" -> "public interface A {}",
      "B.cls" -> "public interface B {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Class reference creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { {Type t = A.class;} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.blocks.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Class self-reference creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { {Type t = Dummy.class;} }"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.blocks.head.dependencies().toSet == Set(tds.head))
  }

  test("Class reference via super types create dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy extends A { {Type t = A.C.class;} }",
      "A.cls" -> "public virtual class A extends B {}",
      "B.cls" -> "public virtual class B {public class C {} }"
    ))
    assert(tds.head.blocks.head.dependencies().toSet == Set(tds(2).nestedTypes.head))
  }

  test("Class reference with ambiguous name") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Database {Type t = Database.QueryLocator.class;} }",
    ))
    assert(!defaultOrg.issues.hasMessages)
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.nestedTypes.head.fields.head.dependencies().isEmpty)
    }
  }

  test("Class reference for component") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { Type t = Component.Apex.OutputText.class; }",
    ))
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Method return creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { A func() {return null;} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Unknown method return") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { A func() {return null;} }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 23-27: No type declaration found for 'A'\n")
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().isEmpty)
  }

  test("Method parameter creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func(A a) {} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Unknown method parameter") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func(A a) {} }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 31-34: No type declaration found for 'A'\n")
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().isEmpty)
  }

  test("Field type creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {A a;}",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.fields.head.dependencies().toSet == tds.tail.toSet)
    }
  }

  test("Unknown Field type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {A a;}"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 22-23: No type declaration found for 'A'\n")
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.fields.head.dependencies().isEmpty)
    }
  }

  test("Property type creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {A a {get;} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.fields.head.dependencies().toSet == tds.tail.toSet)
    }
  }

  test("Unknown Property type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {A a {get;} }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 22-23: No type declaration found for 'A'\n")
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.fields.head.dependencies().isEmpty)
    }
  }

  test("Local var creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {static {A a;} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.blocks.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Unknown local var type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {static {A a;} }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 30-31: No type declaration found for 'A'\n")
    assert(tds.head.blocks.head.dependencies().isEmpty)
  }

  test("Cast creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {static {Object a=(A)null;} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.blocks.head.dependencies().toSet == Set(tds.tail.head))
  }

  test("Unknown cast type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {static {Object a=(A)null;} }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 37-44: No type declaration found for 'A'\n")
    assert(tds.head.blocks.head.dependencies().isEmpty)
  }

  test("For control creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { for(A a;;) {}} }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Unknown for control type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { for(A a;;) {}} }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 41-42: No type declaration found for 'A'\n")
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().isEmpty)
  }

  test("Catch creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { try {} catch(A a){} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Unknown catch type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { try {} catch(A a){} } }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 48-49: No type declaration found for 'A'\n")
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().isEmpty)
  }

  test("New creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { Object a = new A(); } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Complex New creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { Object a = new List<A>(); } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().toSet == Set(tds.tail.head))
  }

  test("Unknown new type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { void func() { Object a = new A(); } }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 50-51: No type declaration found for 'A'\n")
    assert(tds.head.methods.find(_.name == Name("func")).get.dependencies().isEmpty)
  }

  test("InstanceOf creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { { Boolean a = null instanceOf A; } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.blocks.head.dependencies().toSet == Set(tds.tail.head))
  }

  test("Unknown instanceOf type") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { { Boolean a = null instanceOf A; } }"
    ))
    assert(defaultOrg.issues.getMessages("/Dummy.cls") == "Missing: line 1 at 35-52: No type declaration found for 'A'\n")
    assert(tds.head.blocks.head.dependencies().isEmpty)
  }

  test("Class reference in Inner creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner { {Type t = A.class;} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)

    assert(tds.head.nestedTypes.head.blocks.head.dependencies().toSet == Set(tds.tail.head))
  }

  test("Method return in Inner creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner { A func() {return null;} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Method parameter in Inner creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner { void func(A a) {} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Inner Field type creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {class Inner {A a;}}",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.nestedTypes.head.fields.head.dependencies().toSet == tds.tail.toSet)
    }
  }

  test("Inner Property type creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {class Inner {A a {get;}}}",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    OrgImpl.current.withValue(defaultOrg) {
      assert(tds.head.nestedTypes.head.fields.head.dependencies().toSet == tds.tail.toSet)
    }
  }

  test("Inner Local var creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {class Inner {static {A a;} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.blocks.head.dependencies().toSet == tds.tail.toSet)
  }

  test("Inner Cast creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy {class Inner {static {Object a=(A)null;} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.blocks.head.dependencies().toSet == Set(tds.tail.head))
  }

  test("Inner For control creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner {void func() { for(A a;;) {}} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Inner Catch creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner {void func() { try {} catch(A a){} } } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.methods.find(_.name == Name("func")).get.dependencies().toSet == tds.tail.toSet)
  }

  test("Inner New creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner {void func() { Object a = new A(); } } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.methods.find(_.name == Name("func")).get.dependencies().toSet == Set(tds.tail.head))
  }

  test("Inner Complex New creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner {void func() { Object a = new List<A>(); } } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.methods.find(_.name == Name("func")).get.dependencies().toSet == Set(tds.tail.head))
  }

  test("Inner instanceOf creates dependency") {
    val tds = typeDeclarations(Map(
      "Dummy.cls" -> "public class Dummy { class Inner {{ Boolean a = null instanceOf A;} } }",
      "A.cls" -> "public class A {}"
    ))
    assert(!defaultOrg.issues.hasMessages)
    assert(tds.head.nestedTypes.head.blocks.head.dependencies().toSet == Set(tds.tail.head))
  }

  test("Label creates dependency") {
    FileSystemHelper.run(Map(
      "CustomLabels.labels" ->
        """<?xml version="1.0" encoding="UTF-8"?>
          |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
          |    <labels>
          |        <fullName>TestLabel</fullName>
          |        <language>en_US</language>
          |        <protected>true</protected>
          |        <shortDescription>TestLabel Description</shortDescription>
          |        <value>TestLabel Value</value>
          |    </labels>
          |</CustomLabels>
          |""".stripMargin,
      "Dummy.cls" -> "public class Dummy { {String a = label.TestLabel;} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg = org.addMDAPITestPackage(None, Seq(root), Seq())
      assert(!org.issues.hasMessages)

      val labelsType = pkg.packageType(TypeNames.Label).get
      val labelField = labelsType.fields.find(_.name.value == "TestLabel").head
      val dummyType = pkg.packageType(TypeName(Name("Dummy"))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(labelField, labelsType))
      assert(labelField.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Packaged label creates dependency") {
    FileSystemHelper.run(Map(
      "pkg1/CustomLabels.labels" ->
        """<?xml version="1.0" encoding="UTF-8"?>
          |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
          |    <labels>
          |        <fullName>TestLabel</fullName>
          |        <language>en_US</language>
          |        <protected>false</protected>
          |        <shortDescription>TestLabel Description</shortDescription>
          |        <value>TestLabel Value</value>
          |    </labels>
          |</CustomLabels>
          |""".stripMargin,
      "pkg2/Dummy.cls" -> "public class Dummy { {String a = label.pkg1.TestLabel;} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg1 = org.addMDAPITestPackage(Some(Name("pkg1")), Seq(root.join("pkg1")), Seq())
      val pkg2 = org.addMDAPITestPackage(Some(Name("pkg2")), Seq(root.join("pkg2")), Seq(pkg1))
      assert(!org.issues.hasMessages)

      val labels1Type = pkg1.packageType(TypeNames.Label).get
      val labels2Type = pkg2.packageType(TypeNames.Label).get
      val labelField = labels1Type.fields.find(_.name.value == "TestLabel").head
      val dummyType = pkg2.packageType(TypeName(Name("Dummy"), Nil, Some(TypeName(Name("pkg2"))))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(labelField, labels2Type))
      assert(labelField.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Flow creates dependency") {
    FileSystemHelper.run(Map(
      "Test.flow-meta.xml" -> "",
      "Dummy.cls" -> "public class Dummy { {Flow.Interview i = new Flow.Interview.Test(new Map<String, Object>());} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg = org.addMDAPITestPackage(None, Seq(root), Seq())
      assert(!org.issues.hasMessages)

      val interviewType = pkg.packageType(TypeNames.Interview).get
      val flowType = interviewType.findNestedType(Name("Test")).head
      val dummyType = pkg.packageType(TypeName(Name("Dummy"))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(flowType))
      assert(flowType.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Packaged flow creates dependency") {
    FileSystemHelper.run(Map(
      "pkg1/Test.flow-meta.xml" -> "",
      "pkg2/Dummy.cls" -> "public class Dummy { {Flow.Interview i = new Flow.Interview.pkg1.Test(new Map<String, Object>());} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg1 = org.addMDAPITestPackage(Some(Name("pkg1")), Seq(root.join("pkg1")), Seq())
      val pkg2 = org.addMDAPITestPackage(Some(Name("pkg2")), Seq(root.join("pkg2")), Seq(pkg1))
      assert(!org.issues.hasMessages)

      val interviewType1 = pkg1.packageType(TypeNames.Interview).get
      val flowType = interviewType1.findNestedType(Name("Test")).head
      val dummyType = pkg2.packageType(TypeName(Name("Dummy"), Nil, Some(TypeName(Name("pkg2"))))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(flowType))
      assert(flowType.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Page creates dependency") {
    FileSystemHelper.run(Map(
      "TestPage.page" -> "",
      "Dummy.cls" -> "public class Dummy { {PageReference a = Page.TestPage;} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg = org.addMDAPITestPackage(None, Seq(root), Seq())
      assert(!org.issues.hasMessages)

      val pageType = pkg.packageType(TypeNames.Page).get
      val pageField = pageType.fields.find(_.name.value == "TestPage").get
      val dummyType = pkg.packageType(TypeName(Name("Dummy"))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(pageType, pageField))
      assert(pageType.getDependencyHolders == Set(dummyType.blocks.head))
      assert(pageField.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Packaged page creates dependency") {
    FileSystemHelper.run(Map(
      "pkg1/TestPage.page" -> "",
      "pkg2/Dummy.cls" -> "public class Dummy { {PageReference a = Page.pkg1__TestPage;} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg1 = org.addMDAPITestPackage(Some(Name("pkg1")), Seq(root.join("pkg1")), Seq())
      val pkg2 = org.addMDAPITestPackage(Some(Name("pkg2")), Seq(root.join("pkg2")), Seq(pkg1))
      assert(!org.issues.hasMessages)

      val page2Type = pkg2.packageType(TypeNames.Page).get
      val page2Field = page2Type.fields.find(_.name.value == "pkg1__TestPage").get
      val dummyType = pkg2.packageType(TypeName(Name("Dummy"), Nil, Some(TypeName(Name("pkg2"))))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(page2Type, page2Field))
      assert(page2Type.getDependencyHolders == Set(dummyType.blocks.head))
      assert(page2Field.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Component creates dependency") {
    FileSystemHelper.run(Map(
      "Test.component" -> "",
      "Dummy.cls" -> "public class Dummy { {Component c = new Component.Test();} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg = org.addMDAPITestPackage(None, Seq(root), Seq())
      assert(!org.issues.hasMessages)

      val componentsType = pkg.packageType(TypeNames.Component).get
      val componentType = componentsType.findNestedType(Name("Test")).head
      val dummyType = pkg.packageType(TypeName(Name("Dummy"))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(componentType))
      assert(componentType.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

  test("Packaged component creates dependency") {
    FileSystemHelper.run(Map(
      "pkg1/Test.component" -> "",
      "pkg2/Dummy.cls" -> "public class Dummy { {Component c = new Component.pkg1.Test();} }"
    )) { root: PathLike =>
      val org = Org.newOrg().asInstanceOf[OrgImpl]
      val pkg1 = org.addMDAPITestPackage(Some(Name("pkg1")), Seq(root.join("pkg1")), Seq())
      val pkg2 = org.addMDAPITestPackage(Some(Name("pkg2")), Seq(root.join("pkg2")), Seq(pkg1))
      assert(!org.issues.hasMessages)

      val componentsType1 = pkg1.packageType(TypeNames.Component).get
      val componentType = componentsType1.findNestedType(Name("Test")).head
      val dummyType = pkg2.packageType(TypeName(Name("Dummy"), Nil, Some(TypeName(Name("pkg2"))))).get

      assert(dummyType.blocks.head.dependencies().toSet == Set(componentType))
      assert(componentType.getDependencyHolders == Set(dummyType.blocks.head))
    }
  }

}
