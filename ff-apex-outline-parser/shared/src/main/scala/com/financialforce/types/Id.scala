package com.financialforce.types

trait Id {
  def name: String

  def lowerCaseName: String = name.toLowerCase
}

trait IdWithLocation extends Id {
  def location: Location
}

abstract class IdLocationHolder(_location: Location) extends IdWithLocation {
  // These are inlined to save memory
  private val startLine: Int       = _location.startLine
  private val startLineOffset: Int = _location.startLineOffset
  private val startByteOffset: Int = _location.startByteOffset
  private val endLine: Int         = _location.endLine
  private val endLineOffset: Int   = _location.endLineOffset
  private val endByteOffset: Int   = _location.endByteOffset

  override def location: Location =
    Location(startLine, startLineOffset, startByteOffset, endLine, endLineOffset, endByteOffset)
}

/* An Id and its associated location, beware equality is defined only over the id. */
class LocatableId(override val name: String, _location: Location)
    extends IdLocationHolder(_location) {

  override def toString: String = name

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[LocatableId]
    lowerCaseName.equals(other.lowerCaseName)
  }

  override val hashCode: Int = lowerCaseName.hashCode
}
