package com.financialforce.types

case class QualifiedName(parts: Array[IdWithLocation]) {
  def location: Location = {
    val start = parts.head.location
    val end   = parts.last.location
    Location.from(start, end)
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[QualifiedName]
    parts.sameElements(other.parts)
  }

  override def toString: String = {
    parts.map(_.toString).mkString(".")
  }
}
