package com.financialforce.types

case class Modifier(text: String) {
  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Modifier]
    text.equalsIgnoreCase(other.text)
  }

  override def toString: String = text
}

object Modifier {
  final val emptyArray = Array[Modifier]()

  private val cache = new ArrayInternCache[Modifier]()

  def intern(modifiers: Array[Modifier]): Array[Modifier] = {
    cache.intern(modifiers)
  }
}
