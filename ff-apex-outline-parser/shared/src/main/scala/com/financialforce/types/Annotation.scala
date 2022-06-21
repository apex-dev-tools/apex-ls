package com.financialforce.types

case class Annotation(name: String, parameters: Option[String]) {
  override def toString: String = {
    if (parameters.isDefined) s"@$name(${parameters.get})" else s"@$name"
  }
}

object Annotation {
  final val emptyArray = Array[Annotation]()

  private val cache = new ArrayInternCache[Annotation]()

  def intern(annotations: Array[Annotation]): Array[Annotation] = {
    cache.intern(annotations)
  }
}
