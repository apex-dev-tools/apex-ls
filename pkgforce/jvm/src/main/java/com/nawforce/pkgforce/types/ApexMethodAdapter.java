/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.oparser.FormalParameter;
import com.financialforce.oparser.MethodDeclaration;
import com.nawforce.pkgforce.api.ApexMethod;
import com.nawforce.pkgforce.api.ApexMethodParameter;
import com.nawforce.pkgforce.api.ApexType;
import com.nawforce.pkgforce.api.ApexTypeId;
import scala.collection.mutable.ArrayBuffer;

import java.util.Arrays;
import java.util.List;

public class ApexMethodAdapter implements ApexMethod {
    final private MethodDeclaration md;
    final private ApexTypeAdapter definingType;

    public ApexMethodAdapter(ApexTypeAdapter definingType, MethodDeclaration md) {
        this.definingType = definingType;
        this.md = md;
    }

    @Override
    public String getMethodName() {
        return md.id().toString();
    }

    @Override
    public boolean isConstructor() {
        return false;
    }

    @Override
    public String getModifiers() {
        return md.annotationsAndModifiers();
    }

    @Override
    public ApexTypeId getReturnType() {
        return NameApexTypeId.apply(md.typeRef());
    }

    @Override
    public List<ApexMethodParameter> getParameters() {
        ArrayBuffer<FormalParameter> parameters = md.formalParameterList().formalParameters();
        ApexMethodParameter[] result = new ApexMethodParameter[parameters.length()];
        for (int i = 0; i < parameters.length(); i++)
            result[i] = new ApexMethodParameterAdapter(parameters.apply(i));
        return Arrays.asList(result);
    }

    @Override
    public ApexLocationAdapter getLocation() {
        return new ApexLocationAdapter(md.location().get());
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
        ApexMethodAdapter otherMethod = (ApexMethodAdapter) other;
        return md == otherMethod.md && definingType == otherMethod.definingType;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + md.hashCode();
        hash = 31 * hash + definingType.hashCode();
        return hash;
    }

}
