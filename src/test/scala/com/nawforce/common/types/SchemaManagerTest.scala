package com.nawforce.common.types

import com.nawforce.common.api.Org
import com.nawforce.common.names.Name
import com.nawforce.common.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class SchemaManagerTest extends AnyFunSuite {

  def customObject(label: String, fields: Seq[(String, String, Option[String])]): String = {
    val fieldMetadata = fields.map(field => {
      s"""
         |    <fields>
         |        <fullName>${field._1}</fullName>
         |        <type>${field._2}</type>
         |        ${if (field._3.nonEmpty) s"<referenceTo>field._3.get</referenceTo>" else ""}
         |    </fields>
         |""".stripMargin
    })

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$label</fullName>
       |    $fieldMetadata
       |</CustomObject>
       |""".stripMargin
  }

  test("Standard object visible") {
    FileSystemHelper.run(Map(
      "Dummy.cls" -> "public class Dummy { {DescribeSObjectResult r = SObjectType.Account;} }"
    )) { root: PathLike =>
      val org = new Org()
      val pkg = org.addPackage(None, Seq(root), Seq())
      pkg.deployAll()
      assert(!org.issues.hasMessages)
    }
  }

  test("Custom object visible") {
    FileSystemHelper.run(Map(
      "Foo__c.object" -> customObject("Foo", Seq(("Bar__c", "Text", None))),
      "Dummy.cls" -> "public class Dummy { {DescribeSObjectResult r = SObjectType.Foo__c;} }"
    )) { root: PathLike =>
      val org = new Org()
      val pkg = org.addPackage(None, Seq(root), Seq())
      pkg.deployAll()
      assert(!org.issues.hasMessages)
    }
  }

  test("Ghosted custom object visible") {
    FileSystemHelper.run(Map(
      "Dummy.cls" -> "public class Dummy { {DescribeSObjectResult r = SObjectType.ghosted__Foo__c;} }"
    )) { root: PathLike =>
      val org = new Org()
      val ghosted = org.addPackage(Some(Name("ghosted")), Seq(), Seq())
      val pkg = org.addPackage(None, Seq(root), Seq(ghosted))
      pkg.deployAll()
      assert(!org.issues.hasMessages)
    }
  }
}
