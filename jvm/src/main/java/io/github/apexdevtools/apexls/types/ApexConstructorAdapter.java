/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.types;

import com.financialforce.types.IConstructorDeclaration;
import com.financialforce.types.IFormalParameter;
import io.github.apexdevtools.apexls.api.ApexMethod;
import io.github.apexdevtools.apexls.api.ApexMethodParameter;
import io.github.apexdevtools.apexls.api.ApexType;
import io.github.apexdevtools.apexls.api.ApexTypeId;
import scala.collection.immutable.ArraySeq;

import java.util.Arrays;
import java.util.List;

public class ApexConstructorAdapter implements ApexMethod {
    final private IConstructorDeclaration cd;
    final private ApexTypeAdapter definingType;

    public ApexConstructorAdapter(ApexTypeAdapter definingType, IConstructorDeclaration cd) {
        this.definingType = definingType;
        this.cd = cd;
    }

    @Override
    public String getMethodName() {
        return cd.id().toString();
    }

    @Override
    public boolean isConstructor() {
        return true;
    }

    @Override
    public String getModifiers() {
        return cd.annotationsAndModifiers();
    }

    @Override
    public ApexTypeId getReturnType() {
        return definingType;
    }

    @Override
    public List<ApexMethodParameter> getParameters() {
        ArraySeq<IFormalParameter> parameters = cd.formalParameters();
        ApexMethodParameter[] result = new ApexMethodParameter[parameters.length()];
        for (int i = 0; i < parameters.length(); i++)
            result[i] = new ApexMethodParameterAdapter(parameters.apply(i));
        return Arrays.asList(result);
    }

    @Override
    public ApexLocationAdapter getLocation() {
        return new ApexLocationAdapter(cd.bodyLocation().get());
    }

    @Override
    public ApexType getDefiningType() {
        return definingType;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (getClass() != other.getClass()) return false;
        ApexConstructorAdapter otherMethod = (ApexConstructorAdapter) other;
        return cd == otherMethod.cd && definingType == otherMethod.definingType;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + cd.hashCode();
        hash = 31 * hash + definingType.hashCode();
        return hash;
    }

}
