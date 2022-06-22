/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base._

import scala.collection.immutable.ArraySeq
import scala.util.hashing.MurmurHash3

/** Class method, note custom equality does not include location information. */
trait IMethodDeclaration extends IBodyDeclaration with AnnotationsAndModifiers {
  var typeRef: Option[TypeRef]
  def formalParameters: ArraySeq[IFormalParameter]
  override def id: IdWithLocation
  override def bodyLocation: Option[Location]
  override def blockLocation: Option[Location]
  override def annotations: Array[Annotation]
  override def modifiers: Array[Modifier]

  def signature: String = {
    val typeName = typeRef.map(_.toString).getOrElse("void")
    s"$annotationsAndModifiers $typeName $id(${formalParameters.mkString(", ")})".tidyWhitespace
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IMethodDeclaration]
    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) && (
      (other.typeRef.isEmpty && typeRef.isEmpty) ||
      (other.typeRef.nonEmpty && typeRef.nonEmpty &&
      other.typeRef.get.sameRef(typeRef.get))
    ) &&
    other.id == id &&
    other.formalParameters == formalParameters
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        typeRef.map(_.toString.toLowerCase().hashCode).getOrElse(0),
        id,
        formalParameters.hashCode()
      )
    )
  }

  override def toString: String = {
    s"${id.location} $signature"
  }
}
