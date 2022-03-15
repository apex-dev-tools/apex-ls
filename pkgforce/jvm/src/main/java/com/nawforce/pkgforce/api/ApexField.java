/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

import com.nawforce.pkgforce.types.ApexLocationAdapter;

public interface ApexField {
    ApexType getOwner();
    String getFieldName();
    String getModifiers();
    String getMemberType();
    ApexTypeId getType();
}
