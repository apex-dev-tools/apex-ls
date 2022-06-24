/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types

import com.financialforce.types.base.{IdWithLocation, Location}

/** Common handling for elements that can appear in a class body. bodyLocation define the location of the element
  * while blockLocation defines the location of any nested block which will be a subpart of the bodyLocation.
  */
trait IBodyDeclaration {
  def id: IdWithLocation
  def bodyLocation: Option[Location]
  def blockLocation: Option[Location]
}
