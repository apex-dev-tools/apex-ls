/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.oparser.FieldDeclaration;
import com.nawforce.pkgforce.api.ApexField;
import com.nawforce.pkgforce.api.ApexType;
import com.nawforce.pkgforce.api.ApexTypeId;
import scala.collection.immutable.ArraySeq;

public class ApexFieldAdapter implements ApexField {
    final private ApexTypeAdapter owner;
    final private FieldDeclaration fd;

    public ApexFieldAdapter(ApexTypeAdapter owner, FieldDeclaration fd) {
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
