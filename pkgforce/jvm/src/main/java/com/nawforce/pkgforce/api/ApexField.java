/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

public interface ApexField {
    ApexType getOwner();
    String getFieldName();
    String getModifiers();
    String getMemberType();
    ApexTypeId getType();
}
