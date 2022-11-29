/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.api;

public interface ApexMethodParameter {
    String getModifier();
    String getName();
    ApexTypeId getArgumentTypeId();
}
