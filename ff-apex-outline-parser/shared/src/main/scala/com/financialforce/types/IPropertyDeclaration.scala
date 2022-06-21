/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base._

/** Class property, note custom IVariable equality does not include location information or property blocks. */
trait IPropertyDeclaration extends IBodyDeclaration with IVariable {
  def propertyBlocks: Array[PropertyBlock]
  override def typeRef: TypeRef
  override def id: IdWithLocation
  override def bodyLocation: Option[Location]
  override def blockLocation: Option[Location]
  override def annotations: Array[Annotation]
  override def modifiers: Array[Modifier]

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IPropertyDeclaration]
    super.equals(other)
  }
}
