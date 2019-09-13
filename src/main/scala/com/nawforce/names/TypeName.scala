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
package com.nawforce.names

case class TypeName(name: Name, params: Seq[TypeName]=Nil, outer: Option[TypeName]=None) {

  lazy val outerName: Name = outer.map(_.outerName).getOrElse(name)

  def withParams(newParams: Seq[TypeName]): TypeName = {
    if (newParams != params)
      TypeName(name, newParams, outer)
    else
      this
  }

  def withOuter(newOuter: Option[TypeName]): TypeName = {
    if (newOuter != outer)
      TypeName(name, params, newOuter)
    else
      this
  }

  def withArraySubscripts(count: Int): TypeName = {
    assert(count >= 0)
    if (count == 0)
      this
    else
      this.asListOf.withArraySubscripts(count-1)
  }

  def withNameReplace(regex: String, replacement: String) : TypeName = {
    TypeName(Name(name.value.replaceAll(regex, replacement)), params, outer)
  }

  def asDotName: DotName = {
    outer match {
      case None => DotName(Seq(name))
      case Some(x) => x.asDotName.append(name)
    }
  }

  def asListOf: TypeName = new TypeName(Name.List, Seq(this), Some(TypeName.System))

  def asClassOf: TypeName = new TypeName(Name.Class$, Seq(this), Some(TypeName.System))

  override def toString: String = {
    val alias = TypeName.aliasMap.get(this)
    alias.getOrElse({
      (if (outer.isEmpty) "" else outer.get.toString + ".") +
        name.toString +
        (if (params.isEmpty) "" else s"<${params.map(_.toString).mkString(", ")}>")
    })
  }
}

object TypeName {
  lazy val Any = TypeName(Name.Any$, Nil, Some(TypeName.Internal))

  lazy val Void = TypeName(Name("void"))
  lazy val Object = TypeName(Name("Object"))
  lazy val System = TypeName(Name("System"))
  lazy val Schema = TypeName(Name("Schema"))
  lazy val SObject = TypeName(Name.SObject, Nil, Some(TypeName.System))
  lazy val Label = TypeName(Name.Label, Nil, Some(TypeName.System))
  lazy val Internal = TypeName(Name.Internal)

  lazy val Long = TypeName(Name("Long"), Nil, Some(TypeName.System))
  lazy val Integer = TypeName(Name("Integer"), Nil, Some(TypeName.System))
  lazy val Double = TypeName(Name("Double"), Nil, Some(TypeName.System))
  lazy val Decimal = TypeName(Name("Decimal"), Nil, Some(TypeName.System))
  lazy val String = TypeName(Name("String"), Nil, Some(TypeName.System))
  lazy val Boolean = TypeName(Name("Boolean"), Nil, Some(TypeName.System))
  lazy val PageReference = TypeName(Name.PageReference, Nil, Some(TypeName.System))
  lazy val SObjectType = TypeName(Name.SObjectType, Nil, Some(TypeName.Schema))
  lazy val SObjectField = TypeName(Name.SObjectField, Nil, Some(TypeName.Schema))
  lazy val DescribeSObjectResult = TypeName(Name.DescribeSObjectResult, Nil, Some(TypeName.Schema))

  private lazy val aliasMap = Map[TypeName, String](
    TypeName(Name.Null$, Nil, Some(TypeName.Internal)) -> "null",
    Any -> "any",
    TypeName(Name.Object$, Nil, Some(TypeName.Internal)) -> "Object",
    TypeName(Name.RecordSet$, Nil, Some(TypeName.Internal)) -> "[SOQL Results]"
  )

  def apply(names: Seq[Name]): TypeName = {
    names match {
      case hd +: Nil => new TypeName(hd)
      case hd +: tl => new TypeName(hd, Nil, Some(TypeName(tl)))
    }
  }
}