/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.api;

public interface ApexMethodParameter {
    String getModifier();
    String getName();
    ApexTypeId getArgumentTypeId();
}
