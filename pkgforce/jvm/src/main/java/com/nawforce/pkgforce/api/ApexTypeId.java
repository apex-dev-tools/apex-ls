/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

/* An identifier of an Apex type */
public interface ApexTypeId {
    String getApexName();
    String getApexNamespace();

    ApexTypeId getEnclosingType();
}
