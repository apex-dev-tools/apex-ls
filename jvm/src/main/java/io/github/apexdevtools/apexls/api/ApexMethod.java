/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.api;

import io.github.apexdevtools.apexls.types.ApexLocationAdapter;

import java.util.List;

public interface ApexMethod {
    String getMethodName();
    boolean isConstructor();
    String getModifiers();
    ApexTypeId getReturnType();
    List<ApexMethodParameter> getParameters();
    ApexLocationAdapter getLocation();
    ApexType getDefiningType();
}
