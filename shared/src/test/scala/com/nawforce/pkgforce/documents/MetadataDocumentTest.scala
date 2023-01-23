/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite

class MetadataDocumentTest extends AnyFunSuite {

  private val root: PathLike = Path("pkg")

  test("cls file") {
    MetadataDocument(root.join("Foo.cls")) match {
      case Some(ApexClassDocument(path, Name("Foo"))) if path == root.join("Foo.cls") => ()
      case x => assert(false, x)
    }
  }

  test("cls meta file") {
    MetadataDocument(root.join("Foo.cls-meta.xml")) match {
      case Some(ApexClassMetaDocument(path, Name("Foo")))
          if path == root.join("Foo.cls-meta.xml") =>
      case x => assert(false, x)
    }
  }

  test("invalid cls meta file is considered active") {
    FileSystemHelper.run(Map("Foo.cls-meta.xml" -> "")) { root: PathLike =>
      val md = MetadataDocument(root.join("Foo.cls-meta.xml")).collect {
        case md: ApexClassMetaDocument => md
      }
      assert(md.get.isActive)
    }
  }

  test("active cls meta file is considered active") {
    FileSystemHelper.run(
      Map(
        "Foo.cls-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <apiVersion>52.0</apiVersion>
            |  <status>Active</status>
            |</ApexClass>
            |""".stripMargin
      )
    ) { root: PathLike =>
      val md = MetadataDocument(root.join("Foo.cls-meta.xml")).collect {
        case md: ApexClassMetaDocument => md
      }
      assert(md.get.isActive)
    }
  }

  test("deleted cls meta file is not considered active") {
    FileSystemHelper.run(
      Map(
        "Foo.cls-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <apiVersion>52.0</apiVersion>
            |  <status>Deleted</status>
            |</ApexClass>
            |""".stripMargin
      )
    ) { root: PathLike =>
      val md = MetadataDocument(root.join("Foo.cls-meta.xml")).collect {
        case md: ApexClassMetaDocument => md
      }
      assert(!md.get.isActive)
    }
  }

  test("trigger file") {
    MetadataDocument(root.join("Foo.trigger")) match {
      case Some(ApexTriggerDocument(path, Name("Foo"))) if path == root.join("Foo.trigger") => ()
      case x => assert(false, x)
    }
  }

  test("trigger meta file") {
    MetadataDocument(root.join("Foo.trigger-meta.xml")) match {
      case Some(ApexTriggerMetaDocument(path, Name("Foo")))
          if path == root.join("Foo.trigger-meta.xml") =>
        ()
      case x => assert(false, x)
    }
  }

  test("invalid trigger meta file is considered active") {
    FileSystemHelper.run(Map("Foo.trigger-meta.xml" -> "")) { root: PathLike =>
      val md = MetadataDocument(root.join("Foo.trigger-meta.xml")).collect {
        case md: ApexTriggerMetaDocument => md
      }
      assert(md.get.isActive)
    }
  }

  test("active trigger meta file is considered active") {
    FileSystemHelper.run(
      Map(
        "Foo.trigger-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<ApexTrigger xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <apiVersion>52.0</apiVersion>
            |  <status>Active</status>
            |</ApexTrigger>
            |""".stripMargin
      )
    ) { root: PathLike =>
      val md = MetadataDocument(root.join("Foo.trigger-meta.xml")).collect {
        case md: ApexTriggerMetaDocument => md
      }
      assert(md.get.isActive)
    }
  }

  test("deleted trigger meta file is not considered active") {
    FileSystemHelper.run(
      Map(
        "Foo.trigger-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<ApexTrigger xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <apiVersion>52.0</apiVersion>
            |  <status>Deleted</status>
            |</ApexTrigger>
            |""".stripMargin
      )
    ) { root: PathLike =>
      val md = MetadataDocument(root.join("Foo.trigger-meta.xml")).collect {
        case md: ApexTriggerMetaDocument => md
      }
      assert(!md.get.isActive)
    }
  }

  test("component file") {
    MetadataDocument(root.join("Foo.component")) match {
      case Some(ComponentDocument(path, Name("Foo"))) if path == root.join("Foo.component") => ()
      case x => assert(false, x)
    }
  }

  test("standard object file") {
    val target = root.join("objects", "Foo.object")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectDocument(path, Name("Foo"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get.typeName(Some(Name("ns"))) == TypeName(Name("Foo"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("standard object file (sfdx)") {
    val target = root.join("objects", "Foo", "Foo.object-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectDocument(path, Name("Foo"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get.typeName(Some(Name("ns"))) == TypeName(Name("Foo"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("custom object file") {
    val target = root.join("objects", "Foo__c.object")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__c"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(Name("ns__Foo__c"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("custom object file (sfdx)") {
    val target = root.join("objects", "Foo__c", "Foo__c.object-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__c"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(Name("ns__Foo__c"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("custom metadata file") {
    val target = root.join("objects", "Foo__mdt.object")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(CustomMetadataDocument(path, Name("Foo__mdt"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__mdt"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(
            Name("ns__Foo__mdt"),
            Nil,
            Some(TypeName.Schema)
          )
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("custom metadata file (sfdx)") {
    val target = root.join("objects", "Foo__mdt", "Foo__mdt.object-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(CustomMetadataDocument(path, Name("Foo__mdt"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__mdt"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(
            Name("ns__Foo__mdt"),
            Nil,
            Some(TypeName.Schema)
          )
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("big object file") {
    val target = root.join("objects", "Foo__b.object")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(BigObjectDocument(path, Name("Foo__b"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__b"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(Name("ns__Foo__b"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("big object file (sfdx)") {
    val target = root.join("objects", "Foo__b", "Foo__b.object-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(BigObjectDocument(path, Name("Foo__b"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__b"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(Name("ns__Foo__b"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("platform event file") {
    val target = root.join("objects", "Foo__e.object")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(PlatformEventDocument(path, Name("Foo__e"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__e"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(Name("ns__Foo__e"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("platform event file (sfdx)") {
    val target = root.join("objects", "Foo__e", "Foo__e.object-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(PlatformEventDocument(path, Name("Foo__e"))) if path == target =>
        assert(doc.get.typeName(None) == TypeName(Name("Foo__e"), Nil, Some(TypeName.Schema)))
        assert(
          doc.get
            .typeName(Some(Name("ns"))) == TypeName(Name("ns__Foo__e"), Nil, Some(TypeName.Schema))
        )
        assert(doc.get.controllingTypeName(None) == doc.get.typeName(None))
        assert(doc.get.controllingTypeName(Some(Name("ns"))) == doc.get.typeName(Some(Name("ns"))))
      case x => assert(false, x)
    }
  }

  test("standard object field file (sfdx)") {
    val target = root.join("objects", "Bar", "fields", "Foo__c.field-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectFieldDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar.Fields.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.Bar.Fields.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.Bar"
        )
      case x => assert(false, x)
    }
  }

  test("standard object fieldset file (sfdx)") {
    val target = root.join("objects", "Bar", "fieldSets", "Foo__c.fieldSet-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectFieldSetDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar.FieldSets.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.Bar.FieldSets.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.Bar"
        )
      case x => assert(false, x)
    }
  }

  test("standard object sharing reason file (sfdx)") {
    val target = root.join("objects", "Bar", "sharingReasons", "Foo__c.sharingReason-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectSharingReasonDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar.RowCause.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.Bar.RowCause.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.Bar"
        )
      case x => assert(false, x)
    }
  }

  test("custom object field file (sfdx)") {
    val target = root.join("objects", "Bar__c", "fields", "Foo__c.field-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectFieldDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar__c.Fields.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.ns__Bar__c.Fields.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar__c")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.ns__Bar__c"
        )
      case x => assert(false, x)
    }
  }

  test("custom object fieldset file (sfdx)") {
    val target = root.join("objects", "Bar__c", "fieldSets", "Foo__c.fieldSet-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectFieldSetDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar__c.FieldSets.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.ns__Bar__c.FieldSets.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar__c")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.ns__Bar__c"
        )
      case x => assert(false, x)
    }
  }

  test("custom object sharing reason file (sfdx)") {
    val target = root.join("objects", "Bar__c", "sharingReasons", "Foo__c.sharingReason-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectSharingReasonDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar__c.RowCause.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.ns__Bar__c.RowCause.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar__c")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.ns__Bar__c"
        )
      case x => assert(false, x)
    }
  }

  test("custom metadata field file (sfdx)") {
    val target = root.join("objects", "Bar__mdt", "fields", "Foo__c.field-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectFieldDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar__mdt.Fields.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.ns__Bar__mdt.Fields.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar__mdt")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.ns__Bar__mdt"
        )
      case x => assert(false, x)
    }
  }

  test("custom metadata fieldset file (sfdx)") {
    val target = root.join("objects", "Bar__mdt", "fieldSets", "Foo__c.fieldSet-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectFieldSetDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar__mdt.FieldSets.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.ns__Bar__mdt.FieldSets.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar__mdt")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.ns__Bar__mdt"
        )
      case x => assert(false, x)
    }
  }

  test("custom metadata sharing reason file (sfdx)") {
    val target = root.join("objects", "Bar__mdt", "sharingReasons", "Foo__c.sharingReason-meta.xml")
    val doc    = MetadataDocument(target)
    doc match {
      case Some(SObjectSharingReasonDocument(path, Name("Foo__c"))) if path == target =>
        assert(doc.get.typeName(None).toString == "Schema.SObjectType.Bar__mdt.RowCause.Foo__c")
        assert(
          doc.get
            .typeName(Some(Name("ns")))
            .toString == "Schema.SObjectType.ns__Bar__mdt.RowCause.ns__Foo__c"
        )
        assert(doc.get.controllingTypeName(None).toString == "Schema.Bar__mdt")
        assert(
          doc.get
            .controllingTypeName(Some(Name("ns")))
            .toString == "Schema.ns__Bar__mdt"
        )
      case x => assert(false, x)
    }
  }

  test("labels file") {
    MetadataDocument(root.join("Foo.labels")) match {
      case Some(LabelsDocument(path, Name("Foo"))) if path == root.join("Foo.labels") => ()
      case x => assert(false, x)
    }
  }

  test("labels file (sfdx)") {
    MetadataDocument(root.join("Foo.labels-meta.xml")) match {
      case Some(LabelsDocument(path, Name("Foo"))) if path == root.join("Foo.labels-meta.xml") => ()
      case x => assert(false, x)
    }
  }

  test("page file") {
    MetadataDocument(root.join("Foo.page")) match {
      case Some(PageDocument(path, Name("Foo"))) if path == root.join("Foo.page") => ()
      case x => assert(false, x)
    }
  }

  test("extra dots") {
    MetadataDocument(root.join("gantt_v13.1.labels-meta.xml")) match {
      case Some(LabelsDocument(path, Name("gantt_v13.1")))
          if path == root.join("gantt_v13.1.labels-meta.xml") =>
        ()
      case x => assert(false, x)
    }
  }

}
