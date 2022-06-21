/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base._

import scala.util.hashing.MurmurHash3

/** Class constructor, note custom equality does not include location information. */
trait IConstructorDeclaration extends IBodyDeclaration with AnnotationsAndModifiers {
  def qname: QualifiedName
  def formalParameterList: FormalParameterList
  override def id: IdWithLocation
  override def bodyLocation: Option[Location]
  override def blockLocation: Option[Location]
  override def annotations: Array[Annotation]
  override def modifiers: Array[Modifier]

  def signature: String = s"$annotationsAndModifiers $qname($formalParameterList)".tidyWhitespace

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IConstructorDeclaration]
    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) &&
    other.id == id &&
    other.formalParameterList == formalParameterList
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        id,
        formalParameterList
      )
    )
  }

  override def toString: String = {
    s"${id.location} $signature".tidyWhitespace
  }
}
