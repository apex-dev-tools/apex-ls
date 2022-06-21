/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.oparser.PropertyDeclaration;
import com.nawforce.pkgforce.api.ApexField;
import com.nawforce.pkgforce.api.ApexType;
import com.nawforce.pkgforce.api.ApexTypeId;
import scala.collection.immutable.ArraySeq;

public class ApexPropertyAdapter implements ApexField {
    final private ApexTypeAdapter owner;
    final private PropertyDeclaration pd;

    public ApexPropertyAdapter(ApexTypeAdapter owner, PropertyDeclaration pd) {
        this.owner = owner;
        this.pd = pd;
    }

    @Override
    public ApexType getOwner() {
        return owner;
    }

    @Override
    public String getFieldName() {
        return pd.id().toString();
    }

    @Override
    public String getModifiers() {
        return ArraySeq.unsafeWrapArray(pd.modifiers()).mkString(" ");
    }

    @Override
    public String getMemberType() {
        return "PROPERTY";
    }

    @Override
    public ApexTypeId getType() {
        return NameApexTypeId.apply(pd.typeRef());
    }
}
