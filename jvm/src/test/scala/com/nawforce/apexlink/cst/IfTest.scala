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

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class IfTest extends AnyFunSuite with TestHelper {

  test("Bad conditional") {
    typeDeclaration("public class Dummy {{ if (foo) {} }}")
    assert(
      dummyIssues == "Missing: line 1 at 26-29: No variable or type found for 'foo' on 'Dummy'\n"
    )
  }

  test("Non boolean conditional") {
    typeDeclaration("public class Dummy {{ if ('') {} }}")
    assert(
      dummyIssues == "Error: line 1 at 26-28: If expression should return a Boolean value, not a 'System.String'\n"
    )
  }

  test("Null boolean conditional") {
    typeDeclaration("public class Dummy {{ if (null) {} }}")
    assert(
      dummyIssues == "Error: line 1 at 26-30: If expression should return a Boolean value, not a 'null'\n"
    )
  }

  test("Single statement") {
    happyTypeDeclaration("public class Dummy {{ if (true) System.debug(''); }}")
  }

  test("Else statement") {
    happyTypeDeclaration(
      "public class Dummy {{ if (true) System.debug(''); else System.debug(''); }}"
    )
  }

  test("Scoping bug") {
    happyTypeDeclaration("public class Dummy {{ if (false) String a = ''; else a =''; }}")
  }

  test("Scoping bug with block") {
    typeDeclaration("public class Dummy {{ if (false) {String a = '';} else a =''; }}")
    assert(
      dummyIssues == "Missing: line 1 at 55-56: No variable or type found for 'a' on 'Dummy'\n"
    )
  }

  test("Scoping bug with else block") {
    happyTypeDeclaration("public class Dummy {{ if (false) String a = ''; else { a ='';} }}")
  }

  test("Single block") {
    happyTypeDeclaration("public class Dummy {{ if (true) {System.debug('');} }}")
  }

  test("Else block") {
    happyTypeDeclaration(
      "public class Dummy {{ if (true) System.debug(''); else {System.debug('');} }}"
    )
  }

  test("Dual block") {
    happyTypeDeclaration(
      "public class Dummy {{ if (true) {System.debug('');} else {System.debug('');} }}"
    )
  }

}
