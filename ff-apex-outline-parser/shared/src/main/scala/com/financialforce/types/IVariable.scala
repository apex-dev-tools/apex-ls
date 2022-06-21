/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base._

import scala.util.hashing.MurmurHash3

/** Common handling for Variable like elements (fields, properties and formal parameters) that have similar members.
  * Note: custom equality does not include location information.
  */
trait IVariable extends AnnotationsAndModifiers {
  var typeRef: TypeRef
  def id: IdWithLocation
  override def annotations: Array[Annotation]
  override def modifiers: Array[Modifier]

  def signature: String =
    s"$annotationsAndModifiers $typeRef $id".tidyWhitespace

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IVariable]

    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) &&
    other.typeRef.sameRef(typeRef) &&
    other.id == id
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(MurmurHash3.arrayHash(annotations), MurmurHash3.arrayHash(modifiers), typeRef, id)
    )
  }

  override def toString: String = signature
}
