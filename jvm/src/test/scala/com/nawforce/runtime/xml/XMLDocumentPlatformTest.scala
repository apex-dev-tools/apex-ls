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

  test("JVM associates original source ranges across a DOCTYPE") {
    val source =
      s" \r\n<!DOCTYPE m:root [<!ENTITY sample 'a > b'>]>\r\n<m:root xmlns:m='$namespace'><m:child>&sample;</m:child></m:root>"
    val document = XMLDocument(Path("test.xml"), SourceData(source)).value.get
    assert(document.rootElement.line == 3)
    assert(document.rootElement.location == Location(3, 0, 3, 94))
    assert(document.rootElement.getChildren("child").head.location == Location(3, 58, 3, 85))
    val elementLine = source.split("\r\n")(2)
    assert(elementLine.substring(0, 94) == elementLine)
    assert(elementLine.substring(58, 85) == "<m:child>&sample;</m:child>")
  }

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

  test("JVM malformed XML diagnostics remain point based") {
    val result   = XMLDocument(Path("test.xml"), SourceData("<root><child></root>"))
    val location = result.issues.head.diagnostic.location
    assert(result.value.isEmpty)
    assert(location.startLine == location.endLine)
    assert(location.startPosition == location.endPosition)
  }
}
