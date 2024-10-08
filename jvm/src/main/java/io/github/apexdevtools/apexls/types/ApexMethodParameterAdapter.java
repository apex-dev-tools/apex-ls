/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.types;

import com.financialforce.types.IFormalParameter;
import io.github.apexdevtools.apexls.api.ApexMethodParameter;
import io.github.apexdevtools.apexls.api.ApexTypeId;

public class ApexMethodParameterAdapter  implements ApexMethodParameter {
    final private IFormalParameter fp;

    public ApexMethodParameterAdapter(IFormalParameter fp) {
        this.fp = fp;
    }

    @Override
    public String getName() {
        return fp.name();
    }

    @Override
    public String getModifier() {
        return fp.annotationsAndModifiers();
    }

    @Override
    public ApexTypeId getArgumentTypeId() {
        return NameApexTypeId.apply(fp.typeRef());
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (getClass() != other.getClass()) return false;
        ApexMethodParameterAdapter otherParam = (ApexMethodParameterAdapter) other;
        return fp == otherParam.fp;
    }

    @Override
    public int hashCode() {
        return fp.hashCode();
    }

}
