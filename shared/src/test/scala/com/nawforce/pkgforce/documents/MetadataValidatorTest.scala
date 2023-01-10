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

}
