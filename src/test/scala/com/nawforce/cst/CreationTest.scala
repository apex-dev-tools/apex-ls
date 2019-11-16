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
package com.nawforce.cst

import java.io.ByteArrayInputStream
import java.nio.file.{Path, Paths}

import com.nawforce.api.Org
import com.nawforce.names.Name
import com.nawforce.types.{ApexTypeDeclaration, TypeDeclaration}
import org.scalatest.{BeforeAndAfter, FunSuite}

class CreationTest extends FunSuite with BeforeAndAfter {

  private val defaultName: Name = Name("Dummy")
  private val defaultPath: Path = Paths.get(defaultName.toString)
  private var defaultOrg: Org = new Org

  def typeDeclaration(clsText: String): TypeDeclaration = {
    Org.current.withValue(defaultOrg) {
      val td = ApexTypeDeclaration.create(defaultOrg.unmanaged, defaultPath, new ByteArrayInputStream(clsText.getBytes()))
      if (td.isEmpty)
        defaultOrg.issues.dumpMessages(false)
      td.foreach(t => {
        defaultOrg.unmanaged.upsertType(t)
        t.validate()
      })
      td.head
    }
  }

  before {
    defaultOrg = new Org
  }

  test("Basic new") {
    typeDeclaration("public class Dummy {{Object a = new Address();}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Inner class") {
    typeDeclaration("public class Dummy {{Object a = new Messaging.InboundEmail.BinaryAttachment();}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Basic with list constructor") {
    typeDeclaration("public class Dummy {{Object a = new Address{1};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 43-46: Expression list construction is only supported for Set or List types, not 'System.Address'\n")
  }

  test("Basic with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new Address{1 => 2};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 43-51: Expression pair list construction is only supported for Map types, not 'System.Address'\n")
  }

  test("Basic SObject") {
    typeDeclaration("public class Dummy {{Object a = new Account();}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Basic SObject with params") {
    typeDeclaration("public class Dummy {{Object a = new Account(Name='Hello', Jigsaw='Bad');}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Basic SObject with list constructor") {
    typeDeclaration("public class Dummy {{Object a = new Account{1};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 43-46: Expression list construction is only supported for Set or List types, not 'Schema.Account'\n")
  }

  test("Basic SObject with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new Account{1 => 2};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 43-51: Expression pair list construction is only supported for Map types, not 'Schema.Account'\n")
  }

  test("Empty Map") {
    typeDeclaration("public class Dummy {{Object a = new Map<String, Address>();}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Map with argument") {
    typeDeclaration("public class Dummy {{Object a = new Map<String, Address>{'1' => new Address()};}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Map with list constructor") {
    typeDeclaration("public class Dummy {{Object a = new Map<String, Address>{1, 2};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 56-62: Expression list construction is only supported for Set or List types, not 'System.Map<System.String, System.Address>'\n")
  }

  test("Empty List") {
    typeDeclaration("public class Dummy {{Object a = new List<Address>();}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("List with argument") {
    typeDeclaration("public class Dummy {{Object a = new List<Address>{new Address()};}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("List with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new List<Address>{1 => 2};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 49-57: Expression pair list construction is only supported for Map types, not 'System.List<System.Address>'\n")
  }

  test("Empty Set") {
    typeDeclaration("public class Dummy {{Object a = new Set<Address>();}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Set with argument") {
    typeDeclaration("public class Dummy {{Object a = new Set<Address>{new Address()};}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Set with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new Set<Address>{1 => 2};}}")
    assert(defaultOrg.issues.getMessages(defaultPath) ==
      "Error: line 1 at 48-56: Expression pair list construction is only supported for Map types, not 'System.Set<System.Address>'\n")
  }

  test("Array with Index") {
    typeDeclaration("public class Dummy {{List<Object> a = new Address[0];}}")
    assert(!defaultOrg.issues.hasMessages)
  }

  test("Array with Init") {
    typeDeclaration("public class Dummy {{ List<Object> a = new Address[]{new Address()}; }}")
    assert(!defaultOrg.issues.hasMessages)
  }
}
