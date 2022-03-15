/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.nawforce.pkgforce.api.ApexTypeId;
import com.nawforce.pkgforce.names.TypeName;

/*
 * An ApexTypeId just constructed from a name. The apexName is required to include the namespace. The namespace may
 * not be null but may be "" to indicate unmanaged. Safe to use with generics, but it does not provide access to type
 * parameters.
 */
public class NameApexTypeId implements ApexTypeId {
    final private String apexName;
    final private String namespace;

    public NameApexTypeId(String apexName, String namespace) {
        assert namespace.length() == 0 || apexName.startsWith(namespace + '.');
        this.apexName = apexName;
        this.namespace = namespace;
    }

    @Override
    public String getApexName() {
        return apexName;
    }

    @Override
    public String getApexNamespace() {
        return namespace;
    }

    @Override
    public ApexTypeId getEnclosingType() {
        String fullName = namespace.length() == 0 ? apexName : apexName.substring(namespace.length() + 1);
        TypeName typeName = TypeName.fromStringOrNull(fullName);
        if (typeName == null || typeName.outer().isEmpty())
            return null;

        return new NameApexTypeId(typeName.outer().get().toString(), namespace);
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (getClass() != other.getClass()) return false;
        return apexName.equals(((NameApexTypeId) other).apexName);
    }

    @Override
    public int hashCode() {
        return apexName.hashCode();
    }

}
