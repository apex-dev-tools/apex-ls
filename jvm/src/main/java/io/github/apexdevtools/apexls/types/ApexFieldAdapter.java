/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.types;

import com.financialforce.types.IFieldDeclaration;
import io.github.apexdevtools.apexls.api.ApexField;
import io.github.apexdevtools.apexls.api.ApexType;
import io.github.apexdevtools.apexls.api.ApexTypeId;
import scala.collection.immutable.ArraySeq;

public class ApexFieldAdapter implements ApexField {
    final private ApexTypeAdapter owner;
    final private IFieldDeclaration fd;

    public ApexFieldAdapter(ApexTypeAdapter owner, IFieldDeclaration fd) {
        this.owner = owner;
        this.fd = fd;
    }

    @Override
    public ApexType getOwner() {
        return owner;
    }

    @Override
    public String getFieldName() {
        return fd.id().toString();
    }

    @Override
    public String getModifiers() {
        return ArraySeq.unsafeWrapArray(fd.modifiers()).mkString(" ");
    }

    @Override
    public String getMemberType() {
        return "FIELD";
    }

    @Override
    public ApexTypeId getType() {
        return NameApexTypeId.apply(fd.typeRef());
    }
}
