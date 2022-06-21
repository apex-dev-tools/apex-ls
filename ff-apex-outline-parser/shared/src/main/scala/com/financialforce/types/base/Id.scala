/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.types.base

/** Identifier, always treat as case insensitive & ideally intern the name. */
trait Id {
  def name: String

  def lowerCaseName: String = name.toLowerCase
}

/** Identifier with a location in a file from where it was extracted. Location does not hold path information
  * so that needs to be available from some other context, such as the ITypeDeclaration that this is found in.
  */
trait IdWithLocation extends Id {
  def location: Location
}

/** Helper for implementing IdWithLocation support into a class in a memory friendly way. */
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

/** An Id and its associated location, beware equality is defined only over the id. */
class LocatableId(override val name: String, _location: Location)
    extends IdLocationHolder(_location) {

  override def toString: String = name

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[LocatableId]
    lowerCaseName.equals(other.lowerCaseName)
  }

  override val hashCode: Int = lowerCaseName.hashCode
}
