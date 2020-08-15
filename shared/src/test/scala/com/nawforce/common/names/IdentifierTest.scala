/*
 [The "BSD licence"]
 Copyright (c) 2017 Kevin Jones
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
package com.nawforce.common.names

import com.nawforce.common.api.Name
import org.scalatest.funsuite.AnyFunSuite

class IdentifierTest extends AnyFunSuite {
  private val illegalUnderscore = "can not start or end with '_'"
  private val illegalDigit="can not start with a digit"
  private val illegalChar = "can only use characters A-Z, a-z, 0-9 or _"
  private val illegalDoubleUnderscore="can not use '__'"

  test("Illegal identifiers") {
    assert(Name("a").isLegalIdentifier.isEmpty)
    assert(Name("ab").isLegalIdentifier.isEmpty)
    assert(Name("_").isLegalIdentifier.contains(illegalUnderscore))
    assert(Name("_a").isLegalIdentifier.contains(illegalUnderscore))
    assert(Name("a_").isLegalIdentifier.contains(illegalUnderscore))
    assert(Name("0").isLegalIdentifier.contains(illegalDigit))
    assert(Name("0a").isLegalIdentifier.contains(illegalDigit))
    assert(Name("a__b").isLegalIdentifier.contains(illegalDoubleUnderscore))
    assert(Name("$ab").isLegalIdentifier.contains(illegalChar))
    assert(Name("a$b").isLegalIdentifier.contains(illegalChar))
    assert(Name("ab$").isLegalIdentifier.contains(illegalChar))
  }

  test("Reserved identifiers") {
    assert(Name("if").isReservedIdentifier)
    assert(Name("date").isReservedIdentifier)
    assert(!Name("foo").isReservedIdentifier)
  }
}
