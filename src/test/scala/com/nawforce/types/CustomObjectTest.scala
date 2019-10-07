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
package com.nawforce.types

import java.nio.file.Files

import com.google.common.jimfs.{Configuration, Jimfs}
import com.nawforce.api.Org
import com.nawforce.names.Name
import org.scalatest.FunSuite

class CustomObjectTest extends FunSuite {

  def customObject(label: String, fields: Seq[(String, String, Option[String])], fieldSets: Set[String]=Set()): String = {
    val fieldMetadata = fields.map(field => {
      s"""
         |    <fields>
         |        <fullName>${field._1}</fullName>
         |        <type>${field._2}</type>
         |        ${if (field._3.nonEmpty) s"<referenceTo>${field._3.get}</referenceTo>" else ""}
         |        ${if (field._3.nonEmpty) s"<relationshipName>${field._1.replaceAll("__c$","")}</relationshipName>" else ""}
         |    </fields>
         |""".stripMargin
    })

    val fieldSetMetadata = fieldSets.map(fieldSet => {
      s"""
         |    <fieldSets>
         |        <fullName>$fieldSet</fullName>
         |    </fieldSets>
         |""".stripMargin
    })


    s"""<?xml version="1.0" encoding="UTF-8"?>
      |<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
      |    <fullName>$label</fullName>
      |    $fieldMetadata
      |    $fieldSetMetadata
      |</CustomObject>
      |""".stripMargin
  }

  def customField(name: String, fieldType: String, relationshipName: Option[String]): String = {
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<CustomField xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$name</fullName>
       |    <type>$fieldType</type>
       |    ${if (relationshipName.nonEmpty) s"<referenceTo>${relationshipName.get}</referenceTo>" else ""}
       |    ${if (relationshipName.nonEmpty) s"<relationshipName>${name.replaceAll("__c$","")}</relationshipName>" else ""}
       |</CustomField>
       |""".stripMargin
  }

  def customFieldSet(name: String): String = {
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<FieldSet xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$name</fullName>
       |</FieldSet>
       |""".stripMargin
  }

  test("Bad field type") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Silly", None))).getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Foo__c.object")) ==
      "line 5 to 6: Unexpected type 'Silly' on custom field\n")
  }

  test("Illegal Map construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObject a = new Foo__c{'a' => 'b'};} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 38-56: Map construction not supported on SObject type 'Schema.Foo__c'\n")
  }

  test("Illegal Set construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObject a = new Foo__c{'a', 'b'};} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 38-54: Set construction not supported on SObject type 'Schema.Foo__c'\n")
  }

  test("No-arg construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObject a = new Foo__c();} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Single arg construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Bar__c = 'A');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Bad arg construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Baz__c = 'A');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 44-50: Unknown field 'Baz__c' on SObject type 'Schema.Foo__c'\n")
  }

  test("Multi arg construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None), ("Baz__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Baz__c = 'A', Bar__c = 'B');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Duplicate arg construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Bar__c = 'A', Bar__c = 'A');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 58-64: Duplicate assignment to field 'Bar__c' on SObject type 'Schema.Foo__c'\n")
  }

  test("None name=value construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c('Silly');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 44-51: SObject type 'Schema.Foo__c' construction needs '<field name> = <value>' arguments\n")
  }

  test("Id & Name construction") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Id='', Name='');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Lookup construction Id") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Lookup", Some("Account")))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Bar__c = '');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Lookup construction relationship") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Lookup", Some("Account")))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Bar__r = null);} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("MasterDetail construction Id") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "MasterDetail", Some("Account")))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Bar__c = '');} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("MasterDetail construction relationship") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "MasterDetail", Some("Account")))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Object a = new Foo__c(Bar__r = null);} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Cross package new") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.createDirectory(fs.getPath("pkg1"))
    Files.write(fs.getPath("pkg1/Foo__c.object"), customObject("Foo__c", Seq(("Bar__c", "Text", None))).getBytes())
    Files.createDirectory(fs.getPath("pkg2"))
    Files.write(fs.getPath("pkg2/Dummy.cls"),"public class Dummy { {Object a = new pkg1__Foo__c();} }".getBytes())

    val org = new Org()
    val pkg1 = org.addPackageInternal(Some(Name("pkg1")), Seq(fs.getPath("/work/pkg1")), Seq())
    val pkg2 = org.addPackageInternal(Some(Name("pkg2")), Seq(fs.getPath("/work/pkg2")), Seq(pkg1))
    pkg1.deployAll()
    pkg2.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("RecordTypeId field") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Foo__c a; a.RecordTypeId = '';} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Standard field reference") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectField a = Foo__c.Name;} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Custom field reference") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectField a = Foo__c.Bar__c;} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Invalid field reference") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectField a = Foo__c.Baz__c;} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 39-52: Unknown field or type 'Baz__c' on 'Schema.Foo__c'\n")
  }

  test("Cross package field reference") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.createDirectory(fs.getPath("pkg1"))
    Files.write(fs.getPath("pkg1/Foo__c.object"), customObject("Foo__c", Seq(("Bar__c", "Text", None))).getBytes())
    Files.createDirectory(fs.getPath("pkg2"))
    Files.write(fs.getPath("pkg2/Dummy.cls"),"public class Dummy { {pkg1__Foo__c a = null;} }".getBytes())

    val org = new Org()
    val pkg1 = org.addPackageInternal(Some(Name("pkg1")), Seq(fs.getPath("/work/pkg1")), Seq())
    val pkg2 = org.addPackageInternal(Some(Name("pkg2")), Seq(fs.getPath("/work/pkg2")), Seq(pkg1))
    pkg1.deployAll()
    pkg2.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("UserRecordAccess available") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo__c", Seq()).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {Foo__c a; Boolean x = a.UserRecordAccess.HasDeleteAccess;} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Lookup related list") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Bar__c.object"), customObject("Bar", Seq()).getBytes())
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Lookup__c", "Lookup", Some("Bar__c")))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectField a = Bar__c.Lookup__r;} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    org.issues.dumpMessages(false)
    assert(!org.issues.hasMessages)
  }

  test("Lookup related list (ghosted target)") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Lookup__c", "Lookup", Some("ghosted__Bar__c")))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectField a = ghosted__Bar__c.Lookup__r;} }".getBytes())

    val org = new Org()
    val pkg1 = org.addPackageInternal(Some(Name("ghosted")), Seq(), Seq())
    val pkg2 = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq(pkg1))
    pkg2.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Object describable") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Unknown Object describe error") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 48-66: Unknown field or type 'Foo__c' on 'Schema.SObjectType'\n")
  }

  test("Field describable") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c.Fields.Bar__c;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Field describable via Object") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeFieldResult a = Foo__c.SObjectType.Fields.Bar__c;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    org.issues.dumpMessages(false)
    assert(!org.issues.hasMessages)
  }

  test("Field describable via Object (without Fields)") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeFieldResult a = Foo__c.SObjectType.Bar__c;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Unknown Field describe error") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c.Fields.Baz__c;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 48-80: Unknown field or type 'Baz__c' on 'Schema.SObjectType.Foo__c.Fields'\n")
  }

  test("FieldSet describable") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(), Set("TestFS")).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c.FieldSets.TestFS;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Unknown FieldSet describe error") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(), Set("TestFS")).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c.FieldSets.OtherFS;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(org.issues.getMessages(fs.getPath("/work/Dummy.cls")) ==
      "line 1 at 48-84: Unknown field or type 'OtherFS' on 'Schema.SObjectType.Foo__c.FieldSets'\n")
  }

  test("Sfdx field reference") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.createDirectory(fs.getPath("Foo__c"))
    Files.createDirectory(fs.getPath("Foo__c/fields"))
    Files.write(fs.getPath("Foo__c/Foo__c.object-meta.xml"), customObject("Foo", Seq()).getBytes())
    Files.write(fs.getPath("Foo__c/fields/Bar__c.field-meta.xml"), customField("Bar__c", "Text", None).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectField a = Foo__c.Bar__c;} }".getBytes())

    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Sfdx FieldSet describable") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.createDirectory(fs.getPath("Foo__c"))
    Files.createDirectory(fs.getPath("Foo__c/fieldSets"))
    Files.write(fs.getPath("Foo__c/Foo__c.object-meta.xml"), customObject("Foo", Seq()).getBytes())
    Files.write(fs.getPath("Foo__c/fieldSets/TestFS.fieldSet-meta.xml"), customFieldSet("TestFS").getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {DescribeSObjectResult a = SObjectType.Foo__c.FieldSets.TestFS;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }

  test("Schema sObject access describable") {
    val fs = Jimfs.newFileSystem(Configuration.unix)
    Files.write(fs.getPath("Foo__c.object"), customObject("Foo", Seq(("Bar__c", "Text", None))).getBytes())
    Files.write(fs.getPath("Dummy.cls"),"public class Dummy { {SObjectType a = Schema.Foo__c.SObjectType;} }".getBytes())
    val org = new Org()
    val pkg = org.addPackageInternal(None, Seq(fs.getPath("/")), Seq())
    pkg.deployAll()
    assert(!org.issues.hasMessages)
  }
}
