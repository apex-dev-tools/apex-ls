/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base.{IdWithLocation, Location}

/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */

/** Static or non-static initializer block */
trait IInitializer extends IBodyDeclaration {
  def isStatic: Boolean
  override def id: IdWithLocation
  override def bodyLocation: Option[Location]
  override def blockLocation: Option[Location]
}
