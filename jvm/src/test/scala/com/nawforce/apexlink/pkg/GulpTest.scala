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
      createHappyOrg(root)
    }
  }

  test("Extend standard object with $platform field") {
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

  test("Extend standard object with managed field") {
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

  test("Extend standard object twice with managed fields") {
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

  // TODO: unmanaged in additionalNamespaces should always be last
  test("Extends custom object") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged", "ns"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/objects/Foo__c.object" -> customObject("Foo__c", Seq()),
        ".apexlink/gulp/ns/objects/Foo__c/fields/MyField__c.field-meta.xml" -> customField(
          "MyField__c",
          "Text",
          None
        )
      )
    ) { root: PathLike =>
      createOrg(root)
      // Fails because a managed field can not extend an unmanaged custom object
      assert(
        getMessages() == path"/.apexlink/gulp/ns/objects/Foo__c/fields/MyField__c.field-meta.xml: Error: line 1: SObject appears to be extending an unknown SObject, 'Schema.ns__Foo__c'" + "\n"
      )
    }
  }

  test("Managed field added to managed custom object") {
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

  test("Managed field added to managed custom object extended twice") {
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

  test("Unmanaged class") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "packageDirectories": [{"path": "foo"}],
            |  "plugins": {"additionalNamespaces": ["unmanaged"]}
            |}""".stripMargin,
        ".apexlink/gulp/unmanaged/classes/Foo.cls" -> "public class Foo {public void func(){}}",
        "foo/Dummy.cls"                            -> "public class Dummy { {Foo a; a.func();} }"
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
        ".apexlink/gulp/unmanaged/classes/Foo.cls" -> "public class Foo {public void func(){}}",
        "foo/Dummy.cls"                            -> "public class Dummy { {Foo a; a.func();} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      // Fails because a managed class can not reference an unmanaged class
      assert(
        getMessages() == path"/foo/Dummy.cls: Missing: line 1 at 26-27: No type declaration found for 'Foo'" + "\n"
      )
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

}
