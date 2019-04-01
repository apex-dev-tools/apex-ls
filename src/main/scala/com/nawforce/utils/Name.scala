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
package com.nawforce.utils

import scalaz.Memo

/**
  * A case insensitive string typically used for holding symbol names
  */
case class Name(value: String) {
  private val normalised = value.toLowerCase

  def canEqual(that: Any): Boolean = that.isInstanceOf[Name]

  override def equals(that: Any): Boolean = {
    that match {
      case otherName: Name =>
        otherName.canEqual(this) && otherName.normalised == normalised
      case _ => false
    }
  }

  override def hashCode(): Int = normalised.hashCode

  override def toString: String = value
}

object Name {
  def apply(name: String): Name = cache(name)

  lazy val System: Name = cache("System")
  lazy val Void: Name = cache("void")

  private val cache: String => Name = Memo.immutableHashMapMemo { name: String => new Name(name) }
}

/**
  * A qualified name with 'dot' separators
  */
case class DotName(names: Seq[Name]) {

  def lastName: Name = names.last

  def append(name: Name): DotName = DotName(names :+ name)

  override def toString: String = names.mkString(".")
}

object DotName {
  def apply(name: String): DotName = {
    DotName(name.split('.').toSeq.map(p => Name(p)))
  }
}


