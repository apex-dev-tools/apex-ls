/*
 Copyright (c) 2023 Kevin Jones, All rights reserved.
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
import com.nawforce.pkgforce.PathInterpolator.PathInterpolator
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class GulpTest extends AnyFunSuite with TestHelper {

  test("Missing unmanaged gulp directory") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Missing namespaced gulp directory") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Add standard object via $platform") {
    withManualFlush {
      FileSystemHelper.run(
        Map(
          "sfdx-project.json" ->
            """{
              |  "packageDirectories": [{"path": "foo"}],
              |  "plugins": {"additionalNamespaces": ["unmanaged"]}
              |}""".stripMargin,
          ".apexlink/gulp/$platform/objects/Foo.object" -> customObject(
            "Foo",
            Seq(("MyField", Some("Text"), None))
          ),
          "foo/Dummy.cls" -> "public class Dummy { {Foo a; a.MyField=null;} }"
        )
      ) { root: PathLike =>
        val org = createHappyOrg(root)
        val pkg = org.packages.find(_.isPlatformExtension).get
        pkg.refresh(root.join(".apexlink/gulp/$platform/objects/Foo.object"), highPriority = false)
        assert(org.flush())
      }
    }
  }

  test("Extend standard object with standard field") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}]
            |}""".stripMargin,
        ".apexlink/gulp/$platform/objects/Account/fields/Foo.field-meta.xml" ->
          customField("Foo", "Text", None),
        "foo/Dummy.cls" -> "public class Dummy { {Account a; a.Foo=null;} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Extend standard object with custom field") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns/objects/Account/fields/Foo__c.field-meta.xml" ->
          customField("Foo__c", "Text", None),
        "foo/Dummy.cls" -> "public class Dummy { {Account a; a.ns__Foo__c=null;} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Extend standard object twice with custom fields") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1", "ns2"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/objects/Account/fields/Foo__c.field-meta.xml" ->
          customField("Foo__c", "Text", None),
        ".apexlink/gulp/ns2/objects/Account/fields/Bar__c.field-meta.xml" ->
          customField("Bar__c", "Text", None),
        "foo/Dummy.cls" -> "public class Dummy { {Account a; a.ns1__Foo__c=null;a.ns2__Bar__c=null;} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Extends $platform custom object with custom field") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns"]}
            |}""".stripMargin,
        ".apexlink/gulp/$platform/objects/Foo__c.object" -> customObject("Foo__c", Seq()),
        ".apexlink/gulp/ns/objects/Foo__c/fields/MyField__c.field-meta.xml" -> customField(
          "MyField__c",
          "Text",
          None
        )
      )
    ) { root: PathLike =>
      createOrg(root)
      // Should fail as there are no platform custom objects
      assert(
        getMessages(
          root.join(".apexlink/gulp/$platform/objects/Foo__c.object")
        ) == "Error: line 1: Custom Object for 'Schema.Foo__c' can not be created in $platform module\n"
      )
      assert(
        getMessages(
          root.join(".apexlink/gulp/ns/objects/Foo__c/fields/MyField__c.field-meta.xml")
        ) == "Error: line 1: SObject appears to be extending an unknown SObject, 'Schema.ns__Foo__c'\n"
      )
    }
  }

  test("Extend managed custom object with managed field") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1", "ns2"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/objects/Foo__c.object" -> customObject("Foo__c", Seq()),
        ".apexlink/gulp/ns2/objects/ns1__Foo__c/fields/MyField__c.field-meta.xml" -> customField(
          "MyField__c",
          "Text",
          None
        ),
        "foo/Dummy.cls" -> "public class Dummy { {ns1__Foo__c a; a.ns2__MyField__c=null;} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Extend managed custom object twice with managed fields") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |  "packageDirectories": [{"path": "foo"}],
          |  "plugins": {"additionalNamespaces": ["ns1", "ns2", "ns3"]}
          |}""".stripMargin,
        ".apexlink/gulp/ns1/objects/Foo__c.object" -> customObject("Foo__c", Seq()),
        ".apexlink/gulp/ns2/objects/ns1__Foo__c/fields/Bar__c.field-meta.xml" -> customField(
          "Bar__c",
          "Text",
          None
        ),
        ".apexlink/gulp/ns3/objects/ns1__Foo__c/fields/Baz__c.field-meta.xml" -> customField(
          "Baz__c",
          "Text",
          None
        ),
        "foo/Dummy.cls" -> "public class Dummy { {ns1__Foo__c a; a.ns2__Bar__c=null;a.ns3__Baz__c=null;} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform class") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |  "packageDirectories": [{"path": "foo"}],
          |  "plugins": {"additionalNamespaces": ["unmanaged"]}
          |}""".stripMargin,
        ".apexlink/gulp/$platform/classes/Foo.cls" -> "global class Foo {public void func(){}}",
        ".apexlink/gulp/$platform/classes/Bar.cls" -> "public class Bar {public void func(){}}",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Foo a; a.func();}
            |  {Bar b; b.func();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform class from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |  "packageDirectories": [{"path": "foo"}],
          |  "namespace": "ns",
          |  "plugins": {"additionalNamespaces": ["unmanaged"]}
          |}""".stripMargin,
        ".apexlink/gulp/$platform/classes/Foo.cls" -> "global class Foo {public void func(){}}",
        ".apexlink/gulp/$platform/classes/Bar.cls" -> "public class Bar {public void func(){}}",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Foo a; a.func();}
            |  {Bar b; b.func();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged class") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/classes/Foo.cls" -> "global class Foo {public void func(){}}",
        ".apexlink/gulp/unmanaged/classes/Bar.cls" -> "public class Bar {public void func(){}}",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Foo a; a.func();}
            |  {Bar b; b.func();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged class from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/classes/Foo.cls" -> "global class Foo {public void func(){}}",
        ".apexlink/gulp/unmanaged/classes/Bar.cls" -> "public class Bar {public void func(){}}",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Foo a; a.func();}
            |  {Bar b; b.func();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed class") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |  "packageDirectories": [{"path": "foo"}],
          |  "plugins": {"additionalNamespaces": ["ns1"]}
          |}""".stripMargin,
        ".apexlink/gulp/ns1/classes/Foo.cls" -> "global class Foo {public void func(){}}",
        ".apexlink/gulp/ns1/classes/Bar.cls" -> "public class Bar {public void func(){}}",
        "foo/Dummy.cls" ->
          """public class Dummy {
          |  {ns1.Foo a; a.func();}
          |  {ns1.Bar b; b.func();}
          |}
          |""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages() == path"/foo/Dummy.cls: Missing: line 3 at 11-12: No type declaration found for 'ns1.Bar'" + "\n"
      )
    }
  }

  test("Managed class from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/classes/Foo.cls" -> "global class Foo {public void func(){}}",
        ".apexlink/gulp/ns1/classes/Bar.cls" -> "public class Bar {public void func(){}}",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {ns1.Foo a; a.func();}
            |  {ns1.Bar b; b.func();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages() == path"/foo/Dummy.cls: Missing: line 3 at 11-12: No type declaration found for 'ns1.Bar'" + "\n"
      )
    }
  }

  test("Platform labels") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}]
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.labels-meta.xml" ->
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
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {String a = Label.TestLabel;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform labels from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns1"
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.labels-meta.xml" ->
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
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {String a = Label.TestLabel;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged labels") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.labels-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel</fullName>
            |        <language>en_US</language>
            |        <protected>false</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |    <labels>
            |        <fullName>TestLabel2</fullName>
            |        <language>en_US</language>
            |        <protected>true</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin,
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {String a = Label.TestLabel;}
            |  {String b = Label.TestLabel2;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged labels from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns1",
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.labels-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel</fullName>
            |        <language>en_US</language>
            |        <protected>false</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |    <labels>
            |        <fullName>TestLabel2</fullName>
            |        <language>en_US</language>
            |        <protected>true</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin,
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {String a = Label.TestLabel;}
            |  {String b = Label.TestLabel2;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed labels") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.labels-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel</fullName>
            |        <language>en_US</language>
            |        <protected>false</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |    <labels>
            |        <fullName>TestLabel2</fullName>
            |        <language>en_US</language>
            |        <protected>true</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin,
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {String a = Label.TestLabel;}
            |  {String b = Label.TestLabel2;}
            |  {String c = Label.ns1.TestLabel;}
            |  {String d = Label.ns1.TestLabel2;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages() ==
          path"/foo/Dummy.cls: Missing: line 2 at 14-29: Unknown field or type 'TestLabel' on 'System.Label'" + "\n" +
          path"/foo/Dummy.cls: Missing: line 3 at 14-30: Unknown field or type 'TestLabel2' on 'System.Label'" + "\n" +
          path"/foo/Dummy.cls: Missing: line 5 at 14-34: Unknown field or type 'TestLabel2' on 'System.Label.ns1'" + "\n"
      )
    }
  }

  test("Managed labels from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.labels-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
            |    <labels>
            |        <fullName>TestLabel</fullName>
            |        <language>en_US</language>
            |        <protected>false</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |    <labels>
            |        <fullName>TestLabel2</fullName>
            |        <language>en_US</language>
            |        <protected>true</protected>
            |        <shortDescription>TestLabel Description</shortDescription>
            |        <value>TestLabel Value</value>
            |    </labels>
            |</CustomLabels>
            |""".stripMargin,
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {String a = Label.TestLabel;}
            |  {String b = Label.TestLabel2;}
            |  {String c = Label.ns1.TestLabel;}
            |  {String d = Label.ns1.TestLabel2;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages() ==
          path"/foo/Dummy.cls: Missing: line 2 at 14-29: Unknown field or type 'TestLabel' on 'System.Label'" + "\n" +
          path"/foo/Dummy.cls: Missing: line 3 at 14-30: Unknown field or type 'TestLabel2' on 'System.Label'" + "\n" +
          path"/foo/Dummy.cls: Missing: line 5 at 14-34: Unknown field or type 'TestLabel2' on 'System.Label.ns1'" + "\n"
      )
    }
  }

  test("Platform Flow") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}]
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.flow-meta.xml" -> "",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Flow.Interview i = new Flow.Interview.custom(new Map<String, Object>());}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform Flow from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns"
            |
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.flow-meta.xml" -> "",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Flow.Interview i = new Flow.Interview.custom(new Map<String, Object>());}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged Flow") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.flow-meta.xml" -> "",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Flow.Interview i = new Flow.Interview.custom(new Map<String, Object>());}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged Flow from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.flow-meta.xml" -> "",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Flow.Interview i = new Flow.Interview.custom(new Map<String, Object>());}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed Flow") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.flow-meta.xml" -> "",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Flow.Interview i = new Flow.Interview.ns1.custom(new Map<String, Object>());}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed Flow from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.flow-meta.xml" -> "",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Flow.Interview i = new Flow.Interview.ns1.custom(new Map<String, Object>());}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform Component") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}]
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.component" -> "<apex:component/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Component.custom c = new Component.custom();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform Component from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns"
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.component" -> "<apex:component/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Component.custom c = new Component.custom();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged Component") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.component" -> "<apex:component/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Component.custom c = new Component.custom();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged Component from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.component" -> "<apex:component/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Component.custom c = new Component.custom();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed Component") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.component" -> "<apex:component/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Component.ns1.custom c = new Component.ns1.custom();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed Component from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.component" -> "<apex:component/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {Component.ns1.custom c = new Component.ns1.custom();}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform Page") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}]
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.page" -> "<apex:page/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {PageReference p = Page.custom;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Platform Page from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns"
            |}""".stripMargin,
        ".apexlink/gulp/$platform/custom.page" -> "<apex:page/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {PageReference p = Page.custom;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged Page") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.page" -> "<apex:page/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {PageReference p = Page.custom;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Unmanaged Page from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/custom.page" -> "<apex:page/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {PageReference p = Page.custom;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed Page") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.page" -> "<apex:page/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {PageReference p = Page.ns1__custom;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Managed Page from namespace") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "namespace": "ns",
            |  "plugins": {"additionalNamespaces": ["ns1"]}
            |}""".stripMargin,
        ".apexlink/gulp/ns1/custom.page" -> "<apex:page/>",
        "foo/Dummy.cls" ->
          """public class Dummy {
            |  {PageReference p = Page.ns1__custom;}
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

}
