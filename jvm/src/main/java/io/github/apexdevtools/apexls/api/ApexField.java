/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.api;

public interface ApexField {
    ApexType getOwner();
    String getFieldName();
    String getModifiers();
    String getMemberType();
    ApexTypeId getType();
}
