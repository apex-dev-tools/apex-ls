/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

/*
 * An identifier of an Apex type. This supports less functionality than the original apex.db implementation to allow
 * for lightweight implementations. See ApexType support for missing functionality.
 */
public interface ApexTypeId {
    /* The type name, this includes namespace and allows generics, it can not be null or empty. */
    String getApexName();

    /* The type namespace, may not be null be maybe empty for unmanaged types */
    String getApexNamespace();

    /* A type id for the enclosing class of inner types, returns null for outer types. */
    ApexTypeId getEnclosingType();
}
