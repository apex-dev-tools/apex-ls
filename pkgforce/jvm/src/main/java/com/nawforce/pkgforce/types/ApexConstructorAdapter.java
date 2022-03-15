/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.oparser.ConstructorDeclaration;
import com.financialforce.oparser.FormalParameter;
import com.nawforce.pkgforce.api.ApexMethod;
import com.nawforce.pkgforce.api.ApexMethodParameter;
import com.nawforce.pkgforce.api.ApexType;
import com.nawforce.pkgforce.api.ApexTypeId;
import scala.collection.mutable.ArrayBuffer;

import java.util.Arrays;
import java.util.List;

public class ApexConstructorAdapter implements ApexMethod {
    final private ConstructorDeclaration cd;
    final private ApexTypeAdapter definingType;

    public ApexConstructorAdapter(ApexTypeAdapter definingType, ConstructorDeclaration cd) {
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
        // TODO: Annotations
        return cd.modifiers().mkString(" ");
    }

    @Override
    public ApexTypeId getReturnType() {
        // TODO: Type resolving
        return null;
    }

    @Override
    public List<ApexMethodParameter> getParameters() {
        ArrayBuffer<FormalParameter> parameters = cd.formalParameterList().formalParameters();
        ApexMethodParameter[] result = new ApexMethodParameter[parameters.length()];
        for (int i = 0; i < parameters.length(); i++)
            result[i] = new ApexMethodParameterAdapter(parameters.apply(i));
        return Arrays.asList(result);
    }

    @Override
    public ApexLocationAdapter getLocation() {
        return new ApexLocationAdapter(cd.location().get());
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
