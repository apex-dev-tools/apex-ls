/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.api;

/*
 * An identifier of an Apex type. This supports less functionality than the original apex.db implementation to allow
 * for lightweight implementations. See ApexType support for missing functionality.
 *
 * We expose if the type has been resolved successfully. Unresolved types should generally be treated as errors.
 */
public interface ApexTypeId {
    /* The type name, this includes namespace and allows generics, it can not be null or empty. */
    String getApexName();

    /* The type namespace, may not be null but maybe empty for unmanaged or unresolved types. */
    String getApexNamespace();

    /* A type id for the enclosing class of inner types, returns null for outer types. */
    ApexTypeId getEnclosingType();

    /* Was this type resolved, if not true the type could not be found. */
    boolean isResolved();
}
