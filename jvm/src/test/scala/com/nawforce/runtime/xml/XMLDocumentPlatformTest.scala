/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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
package com.nawforce.runtime.xml

import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.parsers.SourceData
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite

class XMLDocumentPlatformTest extends AnyFunSuite {
  private val namespace = "http://soap.sforce.com/2006/04/metadata"

  test("JVM fails closed when entity expansion changes the semantic tree") {
    val source =
      s"<!DOCTYPE root [<!ENTITY child '<child/>'>]><root xmlns='$namespace'>&child;</root>"
    val document = XMLDocument(Path("test.xml"), SourceData(source)).value.get
    assert(document.rootElement.getChildren("child").nonEmpty)
    assert(document.rootElement.location == Location(document.rootElement.line))
    assert(
      document.rootElement.getChildren("child").head.location == Location(
        document.rootElement.getChildren("child").head.line
      )
    )
  }
}
