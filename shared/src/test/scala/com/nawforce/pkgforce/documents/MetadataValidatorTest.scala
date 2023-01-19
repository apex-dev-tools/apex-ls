/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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

import com.nawforce.pkgforce.diagnostics.IssuesManager
import com.nawforce.runtime.platform.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class MetadataValidatorTest extends AnyFunSuite with BeforeAndAfter {

  private var logger: IssuesManager = _

  before {
    logger = new IssuesManager()
  }

  test("single class ok") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      ApexNature,
      List(Path("/pkg/foo/Foo.cls"), Path("/pkg/foo/Foo.cls-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("duplicate classes error") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      ApexNature,
      List(Path("/pkg/foo/Foo.cls"), Path("/pkg/foo/Foo.cls-meta.xml"), Path("/pkg/bar/Foo.cls"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/bar/Foo.cls: Error: line 1: Duplicate for type 'Foo' found in '/pkg/bar/Foo.cls', ignoring this file, see also /pkg/foo/Foo.cls"
    )
  }

  test("missing class meta errors") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(ApexNature, List(Path("/pkg/foo/Foo.cls")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/foo/Foo.cls: Error: line 1: Type 'Foo' is defined, but meta file is missing for '/pkg/foo/Foo.cls'"
    )
  }

  test("wrong directory class meta errors") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      ApexNature,
      List(Path("/pkg/foo/Foo.cls"), Path("/pkg/bar/Foo.cls-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/foo/Foo.cls: Error: line 1: Type 'Foo' is defined, but its meta file is in a different directory see /pkg/bar/Foo.cls-meta.xml"
    )
  }

  test("multiple class meta errors") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      ApexNature,
      List(
        Path("/pkg/foo/Foo.cls"),
        Path("/pkg/foo/Foo.cls-meta.xml"),
        Path("/pkg/bar/Foo.cls-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/foo/Foo.cls: Error: line 1: Type 'Foo' is defined, but multiple meta files found at /pkg/foo/Foo.cls-meta.xml, /pkg/bar/Foo.cls-meta.xml"
    )
  }

  test("single trigger ok") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      TriggerNature,
      List(Path("/pkg/foo/Foo.trigger"), Path("/pkg/foo/Foo.trigger-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("duplicate triggers error") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      TriggerNature,
      List(Path("/pkg/foo/Foo.trigger"), Path("/pkg/bar/Foo.trigger"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/bar/Foo.trigger: Error: line 1: Duplicate for type '__sfdc_trigger/Foo' found in '/pkg/bar/Foo.trigger', ignoring this file, see also /pkg/foo/Foo.trigger"
    )
  }

  test("missing trigger meta errors") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(TriggerNature, List(Path("/pkg/foo/Foo.trigger")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/foo/Foo.trigger: Error: line 1: Type '__sfdc_trigger/Foo' is defined, but meta file is missing for '/pkg/foo/Foo.trigger'"
    )
  }

  test("wrong directory trigger meta errors") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      TriggerNature,
      List(Path("/pkg/foo/Foo.trigger"), Path("/pkg/bar/Foo.trigger-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/foo/Foo.trigger: Error: line 1: Type '__sfdc_trigger/Foo' is defined, but its meta file is in a different directory see /pkg/bar/Foo.trigger-meta.xml"
    )
  }

  test("multiple class trigger errors") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      TriggerNature,
      List(
        Path("/pkg/foo/Foo.trigger"),
        Path("/pkg/foo/Foo.trigger-meta.xml"),
        Path("/pkg/bar/Foo.trigger-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/pkg/foo/Foo.trigger: Error: line 1: Type '__sfdc_trigger/Foo' is defined, but multiple meta files found at /pkg/foo/Foo.trigger-meta.xml, /pkg/bar/Foo.trigger-meta.xml"
    )
  }

  test("sobject single meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(SObjectNature, List(Path("/objects/Foo__c/Foo__c.object-meta.xml")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("sobject dual meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__c/Foo__c.object-meta.xml"),
        Path("/dir2/objects/Foo__c/Foo__c.object-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__c/Foo__c.object-meta.xml: Error: line 1: Type 'Schema.Foo__c' is defined, but duplicate object-meta.xml files found at /dir2/objects/Foo__c/Foo__c.object-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__c/Foo__c.object-meta.xml: Error: line 1: Type 'Schema.Foo__c' is defined, but duplicate object-meta.xml files found at /dir1/objects/Foo__c/Foo__c.object-meta.xml"
    )
  }

  test("sobject only field") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(Path("/objects/Foo__c/fields/MyField__c.field-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("sobject duplicate field") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__c/fields/MyField__c.field-meta.xml"),
        Path("/dir2/objects/Foo__c/fields/MyField__c.field-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__c/fields/MyField__c.field-meta.xml: Error: line 1: Type 'Schema.SObjectType.Foo__c.Fields.MyField__c' is defined, but duplicate metadata found at /dir2/objects/Foo__c/fields/MyField__c.field-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__c/fields/MyField__c.field-meta.xml: Error: line 1: Type 'Schema.SObjectType.Foo__c.Fields.MyField__c' is defined, but duplicate metadata found at /dir1/objects/Foo__c/fields/MyField__c.field-meta.xml"
    )
  }

  test("sobject only fieldset") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(Path("/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("sobject duplicate fieldset") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml"),
        Path("/dir2/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml: Error: line 1: Type 'Schema.SObjectType.Foo__c.FieldSets.MyFieldSet__c' is defined, but duplicate metadata found at /dir2/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml: Error: line 1: Type 'Schema.SObjectType.Foo__c.FieldSets.MyFieldSet__c' is defined, but duplicate metadata found at /dir1/objects/Foo__c/fieldSets/MyFieldSet__c.fieldSet-meta.xml"
    )

  }

  test("sobject only sharing reason") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(Path("/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("sobject duplicate sharing reason") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml"),
        Path("/dir2/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml: Error: line 1: Type 'Schema.SObjectType.Foo__c.RowCause.MySharingReason__c' is defined, but duplicate metadata found at /dir2/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml: Error: line 1: Type 'Schema.SObjectType.Foo__c.RowCause.MySharingReason__c' is defined, but duplicate metadata found at /dir1/objects/Foo__c/sharingReasons/MySharingReason__c.sharingReason-meta.xml"
    )

  }

  test("platform event single meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(SObjectNature, List(Path("/objects/Foo__e/Foo__e.object-meta.xml")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("platform event dual meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__e/Foo__e.object-meta.xml"),
        Path("/dir2/objects/Foo__e/Foo__e.object-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__e/Foo__e.object-meta.xml: Error: line 1: Type 'Schema.Foo__e' is defined, but duplicate object-meta.xml files found at /dir2/objects/Foo__e/Foo__e.object-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__e/Foo__e.object-meta.xml: Error: line 1: Type 'Schema.Foo__e' is defined, but duplicate object-meta.xml files found at /dir1/objects/Foo__e/Foo__e.object-meta.xml"
    )
  }

  test("platform event no meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(Path("/objects/Foo__e/fields/MyField__c.field-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/objects/Foo__e/fields/MyField__c.field-meta.xml: Error: line 1: Components of type 'Schema.Foo__e' are defined, but the required object-meta.xml file is missing"
    )
  }

  test("custom metadata event single meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(SObjectNature, List(Path("/objects/Foo__mdt/Foo__mdt.object-meta.xml")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("custom metadata event dual meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__mdt/Foo__mdt.object-meta.xml"),
        Path("/dir2/objects/Foo__mdt/Foo__mdt.object-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__mdt/Foo__mdt.object-meta.xml: Error: line 1: Type 'Schema.Foo__mdt' is defined, but duplicate object-meta.xml files found at /dir2/objects/Foo__mdt/Foo__mdt.object-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__mdt/Foo__mdt.object-meta.xml: Error: line 1: Type 'Schema.Foo__mdt' is defined, but duplicate object-meta.xml files found at /dir1/objects/Foo__mdt/Foo__mdt.object-meta.xml"
    )
  }

  test("custom metadata no meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(Path("/objects/Foo__mdt/fields/MyField__c.field-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/objects/Foo__mdt/fields/MyField__c.field-meta.xml: Error: line 1: Components of type 'Schema.Foo__mdt' are defined, but the required object-meta.xml file is missing"
    )
  }

  test("big object single meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(SObjectNature, List(Path("/objects/Foo__b/Foo__b.object-meta.xml")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.isEmpty)
  }

  test("big object dual meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(
        Path("/dir1/objects/Foo__b/Foo__b.object-meta.xml"),
        Path("/dir2/objects/Foo__b/Foo__b.object-meta.xml")
      )
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/objects/Foo__b/Foo__b.object-meta.xml: Error: line 1: Type 'Schema.Foo__b' is defined, but duplicate object-meta.xml files found at /dir2/objects/Foo__b/Foo__b.object-meta.xml"
    )
    assert(
      issues(1).toString ==
        "/dir2/objects/Foo__b/Foo__b.object-meta.xml: Error: line 1: Type 'Schema.Foo__b' is defined, but duplicate object-meta.xml files found at /dir1/objects/Foo__b/Foo__b.object-meta.xml"
    )
  }

  test("big object no meta") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      SObjectNature,
      List(Path("/objects/Foo__b/fields/MyField__c.field-meta.xml"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 1)
    assert(
      issues.head.toString ==
        "/objects/Foo__b/fields/MyField__c.field-meta.xml: Error: line 1: Components of type 'Schema.Foo__b' are defined, but the required object-meta.xml file is missing"
    )
  }

  test("dual components") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(
      ComponentNature,
      List(Path("/dir1/Foo.component"), Path("/dir2/Foo.component"))
    )

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/Foo.component: Error: line 1: Duplicate for type 'Component.Foo', see also /dir2/Foo.component"
    )
    assert(
      issues(1).toString ==
        "/dir2/Foo.component: Error: line 1: Duplicate for type 'Component.Foo', see also /dir1/Foo.component"
    )
  }

  test("dual pages") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(PageNature, List(Path("/dir1/Foo.page"), Path("/dir2/Foo.page")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/Foo.page: Error: line 1: Duplicate for type 'Page.Foo', see also /dir2/Foo.page"
    )
    assert(
      issues(1).toString ==
        "/dir2/Foo.page: Error: line 1: Duplicate for type 'Page.Foo', see also /dir1/Foo.page"
    )
  }

  test("dual flows") {
    val validator = new MetadataValidator(logger, None)
    validator.validate(FlowNature, List(Path("/dir1/Foo.flow"), Path("/dir2/Foo.flow")))

    val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
    assert(issues.length == 2)
    assert(
      issues.head.toString ==
        "/dir1/Foo.flow: Error: line 1: Duplicate for type 'Flow.Interview.Foo', see also /dir2/Foo.flow"
    )
    assert(
      issues(1).toString ==
        "/dir2/Foo.flow: Error: line 1: Duplicate for type 'Flow.Interview.Foo', see also /dir1/Foo.flow"
    )
  }

}
