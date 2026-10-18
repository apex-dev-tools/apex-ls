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

import com.nawforce.runtime.parsers.SourceData
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class XMLDocumentPlatformTest extends AnyFunSuite {
  private val namespace = "http://soap.sforce.com/2006/04/metadata"

  private def assertDoctypeRejected(source: String): Unit = {
    val result = XMLDocument(Path("test.xml"), SourceData(source))
    assert(result.value.isEmpty)
    assert(result.issues.head.diagnostic.message.contains("DOCTYPE is disallowed"))
  }

  private def withTempFile(content: String)(op: java.nio.file.Path => Unit): Unit = {
    val file = Files.createTempFile("apex-ls-xxe", ".txt")
    try {
      Files.write(file, content.getBytes(StandardCharsets.UTF_8))
      op(file)
    } finally {
      Files.deleteIfExists(file)
    }
  }

  test("JVM rejects internal entity declarations") {
    assertDoctypeRejected(
      s"<!DOCTYPE root [<!ENTITY child '<child/>'>]><root xmlns='$namespace'>&child;</root>"
    )
  }

  test("JVM rejects external general entities without resolving them") {
    withTempFile("SECRET_MARKER") { secret =>
      assertDoctypeRejected(
        s"<!DOCTYPE root [<!ENTITY x SYSTEM '${secret.toUri}'>]><root xmlns='$namespace'>&x;</root>"
      )
    }
  }

  test("JVM rejects external parameter entities without resolving them") {
    withTempFile("<!ENTITY leak 'SECRET_MARKER'>") { dtd =>
      assertDoctypeRejected(
        s"<!DOCTYPE root [<!ENTITY % p SYSTEM '${dtd.toUri}'> %p;]><root xmlns='$namespace'>&leak;</root>"
      )
    }
  }

  test("JVM rejects an external DTD without loading it") {
    val missing = Paths.get("does-not-exist", "payload.dtd").toAbsolutePath.toUri
    assertDoctypeRejected(s"<!DOCTYPE root SYSTEM '$missing'><root xmlns='$namespace'/>")
  }
}
