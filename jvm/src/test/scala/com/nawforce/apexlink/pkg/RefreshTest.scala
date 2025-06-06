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
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.org.{OPM, RefreshListener}
import com.nawforce.pkgforce.PathInterpolator.PathInterpolator
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class RefreshTest extends AnyFunSuite with TestHelper {

  private def refresh(
    pkg: OPM.PackageImpl,
    path: PathLike,
    source: String,
    highPriority: Boolean = false
  ): Unit = {
    path.write(source)
    pkg.refresh(path, highPriority)
  }

  private def refreshAll(pkg: OPM.PackageImpl, paths: Map[PathLike, String]): Unit = {
    paths.foreach(p => p._1.write(p._2))
    pkg.refreshAll(paths.keys.toArray)
  }

  class Listener(val capture: mutable.ArrayBuffer[PathLike]) extends RefreshListener {
    def onRefreshOne(orgPath: PathLike, updatedPath: PathLike): Unit = capture.addOne(updatedPath)
    def onRefreshMany(orgPath: PathLike, updatedPaths: Seq[PathLike]): Unit =
      capture.addAll(updatedPaths)
  }

  test("Valid refresh") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {}")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid refresh (new)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg").join("Foo.cls"), "public class Foo {}")
        assert(org.flush())
        assert(org.issues.isEmpty)
        assert(pkg.getTypeOfPathInternal(root.join("pkg").join("Foo.cls")) != null)
      }
    }
  }

  test("Valid refresh with changes") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {Object a;}")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid refresh with non-significant changes") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {/* A change */}")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh creates missing") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.cls" -> "public class Foo {Bar.Inner b;}",
          "pkg/Bar.cls" -> "public class Bar {public class Inner {}}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("pkg/Bar.cls"), "public class Bar {}")
        assert(org.flush())
        assert(
          getMessages(root.join("pkg").join("Foo.cls"))
            == "Missing: line 1 at 28-29: No type declaration found for 'Bar.Inner'\n"
        )
      }
    }
  }

  test("Refresh resolves missing") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.cls" -> "public class Foo {Bar.Inner b;}",
          "pkg/Bar.cls" -> "public class Bar {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages(root.join("pkg").join("Foo.cls"))
            == "Missing: line 1 at 28-29: No type declaration found for 'Bar.Inner'\n"
        )

        refresh(pkg, root.join("pkg/Bar.cls"), "public class Bar {public class Inner {}}")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh indirect missing") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/A.cls" -> "public interface A {void func(Foo aFoo);}",
          "pkg/B.cls" -> "public virtual class B {public void func(Foo aFoo) {}}",
          "pkg/C.cls" -> "public class C extends B implements A {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages(root.join("pkg").join("A.cls"))
            == "Missing: line 1 at 34-38: No type declaration found for 'Foo'\n"
        )
        assert(
          getMessages(root.join("pkg").join("B.cls"))
            == "Missing: line 1 at 45-49: No type declaration found for 'Foo'\n"
        )
        assert(
          getMessages(root.join("pkg").join("C.cls"))
            == "Missing: line 1 at 13-14: Non-abstract class must implement method 'void func(Foo)' from interface 'A'\n"
        )

        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {}")
        refresh(pkg, root.join("pkg/Foo.cls-meta.xml"), "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Dependencies created") {
    withManualFlush {
      FileSystemHelper.run(
        Map("pkg/Foo.cls" -> "public class Foo {}", "pkg/Bar.cls" -> "public class Bar {}")
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {Bar b;}")
        assert(org.flush())
        assert(org.issues.isEmpty)

        val fooTypeId =
          pkg.getTypeOfPathInternal(root.join("pkg").join("Foo.cls")).get.asTypeIdentifier
        val barTypeId =
          pkg.getTypeOfPathInternal(root.join("pkg").join("Bar.cls")).get.asTypeIdentifier

        assert(pkg.getDependencyHolders(fooTypeId, apexOnly = false).isEmpty)
        assert(
          pkg
            .getDependencies(fooTypeId, outerInheritanceOnly = false, apexOnly = false)
            .sameElements(Array(barTypeId))
        )

        assert(pkg.getDependencyHolders(barTypeId, apexOnly = false).sameElements(Array(fooTypeId)))
        assert(
          pkg.getDependencies(barTypeId, outerInheritanceOnly = false, apexOnly = false).isEmpty
        )
      }
    }
  }

  test("Dependencies created cross package") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
              |"namespace": "pkg2",
              |"packageDirectories": [{"path": "pkg2"}],
              |"plugins": {"dependencies": [{"namespace": "pkg1", "path": "pkg1"}]}
              |}""".stripMargin,
          "pkg1/Bar.cls" -> "global class Bar {}",
          "pkg2/Foo.cls" -> "public class Foo {}"
        )
      ) { root: PathLike =>
        val org  = createOrg(root)
        val pkg1 = org.packagesByNamespace(Some(Name("pkg1")))
        val pkg2 = org.packagesByNamespace(Some(Name("pkg2")))
        refresh(pkg2, root.join("pkg2/Foo.cls"), "public class Foo {pkg1.Bar b;}")
        assert(org.flush())
        assert(org.issues.isEmpty)

        val barTypeId =
          pkg1.getTypeOfPathInternal(root.join("pkg1").join("Bar.cls")).get.asTypeIdentifier
        val fooTypeId =
          pkg2.getTypeOfPathInternal(root.join("pkg2").join("Foo.cls")).get.asTypeIdentifier

        assert(pkg2.getDependencyHolders(fooTypeId, apexOnly = false).isEmpty)
        assert(
          pkg2
            .getDependencies(fooTypeId, outerInheritanceOnly = false, apexOnly = false)
            .sameElements(Array(barTypeId))
        )

        assert(
          pkg1.getDependencyHolders(barTypeId, apexOnly = false).sameElements(Array(fooTypeId))
        )
        assert(
          pkg1.getDependencies(barTypeId, outerInheritanceOnly = false, apexOnly = false).isEmpty
        )
      }
    }
  }

  test("Deferred issue reporting") {
    withManualFlush {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Dummy.cls"), "public class Dummy {")
        assert(org.issues.isEmpty)

        assert(org.flush())
        assert(
          getMessages(root.join("Dummy.cls"))
            .startsWith("Syntax: line 1 at 20: mismatched input '<EOF>' expecting {")
        )
      }
    }
  }

  test("Extends field access") {
    withManualFlush {
      FileSystemHelper.run(
        Map("Dummy.cls" -> "public class Dummy extends Foo { {Integer a = b;} }")
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages(
            root.join("Dummy.cls")
          ) == "Missing: line 1 at 13-18: No type declaration found for 'Foo'\n"
        )

        refresh(pkg, root.join("Foo.cls-meta.xml"), "")
        refresh(pkg, root.join("Foo.cls"), "public virtual class Foo {public Integer b;}")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Abstract superclass missing handling") {
    withManualFlush {
      FileSystemHelper.run(
        // This replicates a failure case where the method map on C would be cached
        // and not refreshed when A is added, this should now be identified when D
        // is refreshed due it having a 'Missing' diagnostic
        Map(
          "I.cls" -> "public interface I {void func();}",
          "B.cls" -> "public abstract class B extends A implements I {}",
          "C.cls" -> "public abstract class C extends B {}",
          "D.cls" -> "public class D extends C implements I {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages(
            root.join("B.cls")
          ) == "Missing: line 1 at 22-23: No type declaration found for 'A'\n"
        )

        refresh(
          pkg,
          root.join("A.cls"),
          "public abstract class A implements I {public void func() {}}"
        )
        refresh(pkg, root.join("A.cls-meta.xml"), "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid trigger refresh") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.trigger" -> "trigger Foo on Account (before insert) {}")) {
        root: PathLike =>
          val org = createOrg(root)
          val pkg = org.unmanaged
          refresh(pkg, root.join("pkg/Foo.trigger"), "trigger Foo on Account (before insert) {}")
          assert(org.flush())
          assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid trigger refresh (new)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.trigger"), "trigger Foo on Account (before insert) {}")
        assert(org.flush())
        assert(org.issues.isEmpty)
        assert(pkg.getTypeOfPathInternal(root.join("pkg").join("Foo.trigger")) != null)
      }
    }
  }

  test("Valid trigger refresh with changes") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.trigger" -> "trigger Foo on Account (before insert) {}")) {
        root: PathLike =>
          val org = createOrg(root)
          val pkg = org.unmanaged
          refresh(
            pkg,
            root.join("pkg/Foo.trigger"),
            "trigger Foo on Account (before insert) {Object a;}"
          )
          assert(org.flush())
          assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh creates trigger missing") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.trigger" -> "trigger Foo on Account (before insert) {Bar.Inner b;}",
          "pkg/Bar.cls"     -> "public class Bar {public class Inner {}}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("pkg/Bar.cls"), "public class Bar {}")
        assert(org.flush())
        assert(
          getMessages(root.join("pkg").join("Foo.trigger"))
            == "Missing: line 1 at 50-51: No type declaration found for 'Bar.Inner'\n"
        )
      }
    }
  }

  test("Refresh resolves trigger missing") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.trigger" -> "trigger Foo on Account (before insert) {Bar.Inner b;}",
          "pkg/Bar.cls"     -> "public class Bar {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages(root.join("pkg").join("Foo.trigger"))
            == "Missing: line 1 at 50-51: No type declaration found for 'Bar.Inner'\n"
        )

        refresh(pkg, root.join("pkg").join("Bar.cls"), "public class Bar {public class Inner {}}")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Trigger dependencies created") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.trigger" -> "trigger Foo on Account (before insert) {}",
          "pkg/Bar.cls"     -> "public class Bar {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(
          pkg,
          root.join("pkg/Foo.trigger"),
          "trigger Foo on Account (before insert) {Bar b;}"
        )
        assert(org.flush())

        val fooTypeId =
          pkg.getTypeOfPathInternal(root.join("pkg").join("Foo.trigger")).get.asTypeIdentifier
        val barTypeId =
          pkg.getTypeOfPathInternal(root.join("pkg").join("Bar.cls")).get.asTypeIdentifier

        assert(pkg.getDependencyHolders(fooTypeId, apexOnly = false).isEmpty)
        assert(
          pkg
            .getDependencies(fooTypeId, outerInheritanceOnly = false, apexOnly = false)
            .sameElements(Array(barTypeId))
        )

        assert(pkg.getDependencyHolders(barTypeId, apexOnly = false).sameElements(Array(fooTypeId)))
        assert(
          pkg.getDependencies(barTypeId, outerInheritanceOnly = false, apexOnly = false).isEmpty
        )
      }
    }
  }

  test("Trigger dependencies created cross package") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
          |"namespace": "pkg2",
          |"packageDirectories": [{"path": "pkg2"}],
          |"plugins": {"dependencies": [{"namespace": "pkg1", "path": "pkg1"}]}
          |}""".stripMargin,
          "pkg1/Bar.cls"     -> "global class Bar {}",
          "pkg2/Foo.trigger" -> "trigger Foo on Account (before insert) {}"
        )
      ) { root: PathLike =>
        val org  = createOrg(root)
        val pkg1 = org.packagesByNamespace(Some(Name("pkg1")))
        val pkg2 = org.packagesByNamespace(Some(Name("pkg2")))
        refresh(
          pkg2,
          root.join("pkg2/Foo.trigger"),
          "trigger Foo on Account (before insert) {pkg1.Bar b;}"
        )
        assert(org.flush())

        val fooTypeId =
          pkg2.getTypeOfPathInternal(root.join("pkg2").join("Foo.trigger")).get.asTypeIdentifier
        val barTypeId =
          pkg1.getTypeOfPathInternal(root.join("pkg1").join("Bar.cls")).get.asTypeIdentifier

        assert(pkg2.getDependencyHolders(fooTypeId, apexOnly = false).isEmpty)
        assert(
          pkg2
            .getDependencies(fooTypeId, outerInheritanceOnly = false, apexOnly = false)
            .sameElements(Array(barTypeId))
        )

        assert(
          pkg1.getDependencyHolders(barTypeId, apexOnly = false).sameElements(Array(fooTypeId))
        )
        assert(
          pkg1.getDependencies(barTypeId, outerInheritanceOnly = false, apexOnly = false).isEmpty
        )
      }
    }
  }

  test("Valid label upsert") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "CustomLabels.labels" -> "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(
          pkg,
          root.join("CustomLabels.labels"),
          "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
        )
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid label upsert (new)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(
          pkg,
          root.join("CustomLabels.labels"),
          "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
        )
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid label upsert (changed)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "CustomLabels.labels" ->
            """<?xml version="1.0" encoding="UTF-8"?>
          |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
          |    <labels>
          |        <fullName>TestLabel</fullName>
          |        <protected>false</protected>
          |    </labels>
          |</CustomLabels>
          |""".stripMargin
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(
          pkg,
          root.join("CustomLabels.labels"),
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel2</fullName>
            |        <protected>false</protected>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin
        )
        assert(org.flush())
        val labels = pkg.orderedModules.head.labels
        assert(labels.fields.length == 1)
        assert(labels.fields.exists(_.name.value == "TestLabel2"))
      }
    }
  }

  test("Valid label upsert (alt file)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "CustomLabels.labels" ->
            """<?xml version="1.0" encoding="UTF-8"?>
          |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
          |    <labels>
          |        <fullName>TestLabel</fullName>
          |        <protected>false</protected>
          |    </labels>
          |</CustomLabels>
          |""".stripMargin
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(
          pkg,
          root.join("Alt.labels"),
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel2</fullName>
            |        <protected>false</protected>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin
        )
        org.flush()
        val labels = pkg.orderedModules.head.labels
        assert(labels.fields.length == 2)
        assert(labels.fields.exists(_.name.value == "TestLabel"))
        assert(labels.fields.exists(_.name.value == "TestLabel2"))
      }
    }
  }

  test("Valid label class dependent") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "CustomLabels.labels" ->
            """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel</fullName>
            |        <protected>false</protected>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin,
          "Dummy.cls" -> "public class Dummy { {String a = Label.TestLabel;}}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(
          pkg,
          root.join("CustomLabels.labels"),
          "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
        )
        assert(org.flush())
        assert(
          getMessages() == path"/Dummy.cls: Missing: line 1 at 33-48: Unknown field or type 'TestLabel' on 'System.Label'" + "\n"
        )
      }
    }
  }

  test("Valid label class dependent (reversed)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "CustomLabels.labels" -> "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>",
          "Dummy.cls" -> "public class Dummy { {String a = Label.TestLabel;}}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages() == path"/Dummy.cls: Missing: line 1 at 33-48: Unknown field or type 'TestLabel' on 'System.Label'" + "\n"
        )

        refresh(
          pkg,
          root.join("CustomLabels.labels"),
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel</fullName>
            |        <protected>false</protected>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin
        )
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid flow upsert") {
    withManualFlush {
      FileSystemHelper.run(Map("Test.flow-meta.xml" -> "")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test.flow-meta.xml"), "")
        assert(org.flush())
        assert(pkg.orderedModules.head.interviews.findNestedType(Name("Test")).nonEmpty)
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid flow upsert (new)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test.flow-meta.xml"), "")
        assert(org.flush())
        assert(pkg.orderedModules.head.interviews.findNestedType(Name("Test")).nonEmpty)
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid flow upsert (changed)") {
    withManualFlush {
      FileSystemHelper.run(Map("Test.flow-meta.xml" -> "")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test.flow-meta.xml"), "Changed")
        assert(org.flush())
        assert(pkg.orderedModules.head.interviews.findNestedType(Name("Test")).nonEmpty)
      }
    }
  }

  test("Valid flow upsert (new flow)") {
    withManualFlush {
      FileSystemHelper.run(Map("Test.flow-meta.xml" -> "")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test2.flow-meta.xml"), "")
        assert(org.flush())
        assert(
          pkg.orderedModules.head.interviews.nestedTypes.map(_.name).toSet == Set(
            Name("Test"),
            Name("Test2")
          )
        )
      }
    }
  }

  test("Valid page upsert") {
    withManualFlush {
      FileSystemHelper.run(Map("TestPage.page" -> "<apex:page/>")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("TestPage.page"), "<apex:page/>")
        assert(org.flush())
        assert(pkg.orderedModules.head.pages.findField(Name("TestPage"), Some(true)).nonEmpty)
      }
    }
  }

  test("Valid page upsert (new)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("TestPage.page"), "<apex:page/>")
        assert(org.flush())
        assert(pkg.orderedModules.head.pages.findField(Name("TestPage"), Some(true)).nonEmpty)
      }
    }
  }

  test("Valid page upsert (changed)") {
    withManualFlush {
      FileSystemHelper.run(Map("TestPage.page" -> "<apex:page/>")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("TestPage.page"), "<apex:page/> ")
        assert(org.flush())
        assert(pkg.orderedModules.head.pages.findField(Name("TestPage"), Some(true)).nonEmpty)
      }
    }
  }

  test("Valid page upsert (new page)") {
    withManualFlush {
      FileSystemHelper.run(Map("TestPage.page" -> "<apex:page/>")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("TestPage2.page"), "<apex:page/> ")
        assert(org.flush())
        assert(
          pkg.orderedModules.head.pages.fields.map(_.name).toSet == Set(
            Name("TestPage"),
            Name("TestPage2")
          )
        )
      }
    }
  }

  test("Page controller added later") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "TestPage.page" -> "<apex:page standardController=\"Account\" extensions=\"TestController\"/>"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages() == path"/TestPage.page: Missing: line 1 at 40-67: No type declaration found for 'TestController'" + "\n"
        )

        refresh(pkg, root.join("TestController.cls"), "public class TestController {}")
        refresh(pkg, root.join("TestController.cls-meta.xml"), "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Page controller added/removed/added (no namespace)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
              |"packageDirectories": [{"path": "pkg"}]
              |}""".stripMargin,
          "pkg/TestPage.page" -> "<apex:page standardController=\"Account\" extensions=\"TestController\"/>"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages() == path"/pkg/TestPage.page: Missing: line 1 at 40-67: No type declaration found for 'TestController'" + "\n"
        )

        val controller     = root.join("pkg/TestController.cls")
        val controllerMeta = root.join("pkg/TestController.cls-meta.xml")

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)

        controller.delete()
        controllerMeta.delete()
        pkg.refresh(controller, highPriority = false)
        pkg.refresh(controllerMeta, highPriority = false)
        assert(org.flush())
        assert(
          getMessages() == path"/pkg/TestPage.page: Missing: line 1 at 40-67: No type declaration found for 'TestController'" + "\n"
        )

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Page controller added/removed/added (with namespace)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
              |"namespace": "ns1",
              |"packageDirectories": [{"path": "pkg"}]
              |}""".stripMargin,
          "pkg/TestPage.page" -> "<apex:page standardController=\"Account\" extensions=\"TestController\"/>"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.packages.find(_.namespace.contains(Name("ns1"))).head
        assert(
          getMessages() == path"/pkg/TestPage.page: Missing: line 1 at 40-67: No type declaration found for 'TestController'" + "\n"
        )

        val controller     = root.join("pkg/TestController.cls")
        val controllerMeta = root.join("pkg/TestController.cls-meta.xml")

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)

        controller.delete()
        controllerMeta.delete()
        pkg.refresh(controller, highPriority = false)
        pkg.refresh(controllerMeta, highPriority = false)
        assert(org.flush())
        assert(
          getMessages() == path"/pkg/TestPage.page: Missing: line 1 at 40-67: No type declaration found for 'TestController'" + "\n"
        )

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid component upsert") {
    withManualFlush {
      FileSystemHelper.run(Map("Test.component" -> "<apex:component/>")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test.component"), "<apex:component/> ")
        assert(org.flush())
        assert(pkg.orderedModules.head.components.findNestedType(Name("Test")).nonEmpty)
      }
    }
  }

  test("Valid component upsert (new)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test.component"), "<apex:component/>")
        assert(org.flush())
        assert(pkg.orderedModules.head.components.findNestedType(Name("Test")).nonEmpty)
      }
    }
  }

  test("Valid component upsert (changed)") {
    withManualFlush {
      FileSystemHelper.run(Map("Test.component" -> "<apex:component/>")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test.component"), "<apex:component/> ")
        assert(org.flush())
        assert(pkg.orderedModules.head.components.findNestedType(Name("Test")).nonEmpty)
      }
    }
  }

  test("Valid component upsert (new component)") {
    withManualFlush {
      FileSystemHelper.run(Map("Test.component" -> "<apex:component/>")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("Test2.component"), "<apex:component/> ")
        assert(org.flush())
        assert(
          pkg.orderedModules.head.components.nestedTypes.map(_.name).toSet ==
            Set(Name("Test"), Name("Test2"), Names.c, Names.Apex, Names.Chatter)
        )
      }
    }
  }

  test("Component controller added/removed/added (no namespace)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
              |"packageDirectories": [{"path": "pkg"}]
              |}""".stripMargin,
          "pkg/TestComponent.component" -> "<apex:component controller=\"TestController\"/>"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages() == path"/pkg/TestComponent.component: Missing: line 1 at 16-43: No type declaration found for 'TestController'" + "\n"
        )

        val controller     = root.join("pkg/TestController.cls")
        val controllerMeta = root.join("pkg/TestController.cls-meta.xml")

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)

        controller.delete()
        controllerMeta.delete()
        pkg.refresh(controller, highPriority = false)
        pkg.refresh(controllerMeta, highPriority = false)
        assert(org.flush())
        assert(
          getMessages() == path"/pkg/TestComponent.component: Missing: line 1 at 16-43: No type declaration found for 'TestController'" + "\n"
        )

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Component controller added/removed/added (with namespace)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
              |"namespace": "ns1",
              |"packageDirectories": [{"path": "pkg"}]
              |}""".stripMargin,
          "pkg/TestComponent.component" -> "<apex:component controller=\"TestController\"/>"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.packages.find(_.namespace.contains(Name("ns1"))).head
        assert(
          getMessages() == path"/pkg/TestComponent.component: Missing: line 1 at 16-43: No type declaration found for 'TestController'" + "\n"
        )

        val controller     = root.join("pkg/TestController.cls")
        val controllerMeta = root.join("pkg/TestController.cls-meta.xml")

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)

        controller.delete()
        controllerMeta.delete()
        pkg.refresh(controller, highPriority = false)
        pkg.refresh(controllerMeta, highPriority = false)
        assert(org.flush())
        assert(
          getMessages() == path"/pkg/TestComponent.component: Missing: line 1 at 16-43: No type declaration found for 'TestController'" + "\n"
        )

        refresh(pkg, controller, "public class TestController {}")
        refresh(pkg, controllerMeta, "")
        assert(org.flush())
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid refresh (high priority)") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {}", highPriority = true)
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Valid refresh (new high priority)") {
    withManualFlush {
      FileSystemHelper.run(Map()) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg").join("Foo.cls"), "public class Foo {}", highPriority = true)
        assert(org.issues.isEmpty)
        assert(pkg.getTypeOfPathInternal(root.join("pkg").join("Foo.cls")) != null)
      }
    }
  }

  test("Valid refresh with changes (high priority)") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {Object a;}", highPriority = true)
        assert(org.issues.isEmpty)
        assert(unmanagedClass("Foo").exists(c => c.fields.exists(_.name.value == "a")));
      }
    }
  }

  test("Valid refresh with non-significant changes (high priority)") {
    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        refresh(
          pkg,
          root.join("pkg/Foo.cls"),
          "public class Foo {/* A change */}",
          highPriority = true
        )
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh creates missing (high priority)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.cls" -> "public class Foo {Bar.Inner b;}",
          "pkg/Bar.cls" -> "public class Bar {public class Inner {}}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(org.issues.isEmpty)

        refresh(pkg, root.join("pkg/Bar.cls"), "public class Bar {}", highPriority = true)
        assert(
          getMessages(root.join("pkg").join("Foo.cls"))
            == "Missing: line 1 at 28-29: No type declaration found for 'Bar.Inner'\n"
        )
      }
    }
  }

  test("Refresh resolves missing (high priority)") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.cls" -> "public class Foo {Bar.Inner b;}",
          "pkg/Bar.cls" -> "public class Bar {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        val pkg = org.unmanaged
        assert(
          getMessages(root.join("pkg").join("Foo.cls"))
            == "Missing: line 1 at 28-29: No type declaration found for 'Bar.Inner'\n"
        )

        refresh(
          pkg,
          root.join("pkg/Bar.cls"),
          "public class Bar {public class Inner {}}",
          highPriority = true
        )
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh class meta leaves class valid") {
    withManualFlush {
      FileSystemHelper.run(Map("Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createHappyOrg(root)

        refresh(org.unmanaged, root.join("Foo.cls-meta.xml"), "", highPriority = true)

        assert(unmanagedClass("Foo").nonEmpty)
        assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh trigger meta leaves trigger valid") {
    withManualFlush {
      FileSystemHelper.run(Map("Foo.trigger" -> "trigger Foo on Account (before insert) {}")) {
        root: PathLike =>
          val org = createHappyOrg(root)

          refresh(org.unmanaged, root.join("Foo.trigger-meta.xml"), "", highPriority = true)

          assert(unmanagedType(TypeName(Name("__sfdc_trigger/Foo"))).nonEmpty)
          assert(org.issues.isEmpty)
      }
    }
  }

  test("Refresh listener with refresh") {
    val capture = mutable.ArrayBuffer[PathLike]()

    withManualFlush {
      FileSystemHelper.run(Map("pkg/Foo.cls" -> "public class Foo {}")) { root: PathLike =>
        val org = createOrg(root)
        org.setRefreshListener(Some(new Listener(capture)))
        val pkg = org.unmanaged
        refresh(pkg, root.join("pkg/Foo.cls"), "public class Foo {}")
        assert(org.flush())
        assert(org.issues.isEmpty)
        assert(capture.map(_.toString).toSeq.sorted.equals(Seq("/pkg/Foo.cls")))
      }
    }
  }

  test("Refresh listener with refreshAll") {
    val capture = mutable.ArrayBuffer[PathLike]()

    withManualFlush {
      FileSystemHelper.run(
        Map(
          "pkg/Foo.cls" -> "public class Foo {}",
          "pkg/Bar.cls" -> "public class Bar {}",
          "pkg/Baz.cls" -> "public class Baz {}"
        )
      ) { root: PathLike =>
        val org = createOrg(root)
        org.setRefreshListener(Some(new Listener(capture)))
        val pkg = org.unmanaged
        refreshAll(
          pkg,
          Map(
            root.join("pkg/Foo.cls") -> "public class Foo {}",
            root.join("pkg/Bar.cls") -> "public class Bar {}"
          )
        )
        assert(org.flush())
        assert(org.issues.isEmpty)
        assert(capture.map(_.toString).toSeq.sorted.equals(Seq("/pkg/Bar.cls", "/pkg/Foo.cls")))
      }
    }
  }

}
