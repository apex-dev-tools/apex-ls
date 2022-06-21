/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base._

/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */

/** Method formal parameter, tightly aligned to IVariable. */
trait IFormalParameter extends IVariable with IdWithLocation {
  override def location: Location
  override def annotations: Array[Annotation]
  override def modifiers: Array[Modifier]
  override var typeRef: TypeRef
  override def name: String
  override def id = new LocatableId(name, location)

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IFormalParameter]
    super.equals(other)
  }
}
