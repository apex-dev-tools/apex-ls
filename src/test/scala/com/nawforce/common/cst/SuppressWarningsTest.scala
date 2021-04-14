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
package com.nawforce.common.cst

import com.nawforce.common.api.{Name, ServerOps}
import com.nawforce.common.documents.ApexClassDocument
import com.nawforce.common.org.OrgImpl
import com.nawforce.common.path.PathFactory
import com.nawforce.common.types.apex.FullDeclaration
import com.nawforce.common.types.core.TypeDeclaration
import com.nawforce.runtime.parsers.SourceData
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class SuppressWarningsTest extends AnyFunSuite with BeforeAndAfter {
  private val defaultPath = PathFactory("Dummy.cls").toString
  private val defaultDoc = ApexClassDocument(PathFactory("Dummy.cls"), Name("Dummy"))
  private var defaultOrg: OrgImpl = _

  def typeDeclaration(clsText: String): TypeDeclaration = {
    OrgImpl.current.withValue(defaultOrg) {
      val td = FullDeclaration.create(defaultOrg.unmanaged, defaultDoc, SourceData(clsText)).head
      defaultOrg.unmanaged.upsertMetadata(td)
      td.validate()
      td
    }
  }

  before {
    ServerOps.setAutoFlush(false)
    defaultOrg = new OrgImpl
  }

  after {
    ServerOps.setAutoFlush(true)
  }

  test("Suppress disabled") {
    typeDeclaration(
      "public class Dummy {class Inner {Integer b; List<Inner> a; {b = a[null].b;}}}")
    assert(
      defaultOrg.issues
        .getMessages(defaultPath) == "Error: line 1 at 66-70: Array indexes must be Integers, found 'null'\n")
  }

  test("Outer Suppress") {
    typeDeclaration(
      "@SuppressWarnings public class Dummy {class Inner {Integer b; List<Inner> a; {Integer b = a[null].b;}}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Inner Suppress") {
    typeDeclaration(
      "public class Dummy {@SuppressWarnings class Inner {Integer b; List<Inner> a; {Integer b = a[null].b;}}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  // TODO: This should work
  /*
  test("Method Suppress") {
    typeDeclaration("public class Dummy {class Inner {Integer b; List<Inner> a; @SuppressWarnings void foo(){ Integer b = a[null].b;}}}")
    defaultOrg.issues.dumpMessages(false)
    assert(!defaultOrg.issues.hasMessages)
  }*/
}