package com.financialforce.types

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object StringUtils {

  def asString[T](o: Option[T]): String = {
    o match {
      case None    => ""
      case Some(o) => s"${o.toString}"
    }
  }

  def asString[T](a: mutable.ArrayBuffer[T]): String = {
    asString(a, " ")
  }

  def asString[T](a: mutable.ArrayBuffer[T], separator: String): String = {
    a.map(_.toString).mkString(separator)
  }

  def asString[T](a: ArraySeq[T]): String = {
    asString(a, " ")
  }

  def asString[T](a: ArraySeq[T], separator: String): String = {
    a.map(_.toString).mkString(separator)
  }

  def asString[T](a: Array[T]): String = {
    asString(a, " ")
  }

  def asString[T](a: Array[T], separator: String): String = {
    a.map(_.toString).mkString(separator)
  }

  def asAnnotationAndModifierString(a: Array[Annotation], m: Array[Modifier]): String = {
    val mod        = if (m.nonEmpty) s"${m.mkString(" ")}" else ""
    val annotation = if (a.nonEmpty) s"${a.mkString(" ")} " else ""
    s"$annotation$mod"
  }

  def asSignatureString(a: Signature): String = {
    s"${asAnnotationAndModifierString(a.annotations, a.modifiers)} ${a.typeRef.getFullName} ${a.id}"
  }

  def asMethodSignatureString(a: IMethodDeclaration): String = {
    val withVoid = a.typeRef.map(_.getFullName).getOrElse("void")
    s"${asAnnotationAndModifierString(a.annotations, a.modifiers)} $withVoid ${a.id}(${a.formalParameterList})"
  }

  def asConstructorSignatureString(a: IConstructorDeclaration): String = {
    s"${asAnnotationAndModifierString(a.annotations, a.modifiers)} ${a.qname}(${a.formalParameterList})"
  }

}
