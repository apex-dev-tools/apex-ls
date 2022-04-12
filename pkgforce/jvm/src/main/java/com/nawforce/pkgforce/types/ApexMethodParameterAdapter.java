/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.oparser.FormalParameter;
import com.nawforce.pkgforce.api.ApexMethodParameter;
import com.nawforce.pkgforce.api.ApexTypeId;

public class ApexMethodParameterAdapter  implements ApexMethodParameter {
    final private FormalParameter fp;

    public ApexMethodParameterAdapter(FormalParameter fp) {
        this.fp = fp;
    }

    @Override
    public String getName() {
        return fp.id().get().toString();
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
