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
package com.nawforce.finding

import com.nawforce.finding.TypeRequest.TypeRequest
import com.nawforce.names.TypeName
import com.nawforce.types.{PackageDeclaration, TypeDeclaration}

/* Lazy TypeName resolver for relative types. The package & enclosing (outer) typename are used to allow
 * the relative TypeName to be converted to an absolute form. Assumes outerTypeName can always be resolved
 * against the package!
 */
final case class RelativeTypeName(pkg: PackageDeclaration, outerTypeName: TypeName, relativeTypeName: TypeName) {

  // Returns absolute type or may fallback to relative if not found, use typeRequest for error detection
  lazy val typeName: TypeName = {
    // We need the absolute type if we can get it
    typeRequest.map(_.map(_.typeName)) match {
      case Some(Right(tn)) => tn
      case _ => relativeTypeName
    }
  }

  // TypeRequest for the relative type, None if not required
  lazy val typeRequest: Option[TypeRequest] = {
    if (relativeTypeName != TypeName.Void && !pkg.isGhostedType(relativeTypeName))
      Some(TypeRequest(relativeTypeName, outerTypeDeclaration))
    else
      None
  }

  private lazy val outerTypeDeclaration: TypeDeclaration = TypeRequest(outerTypeName, pkg).right.get
}
