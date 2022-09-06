/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.types.base.Location;

public class ApexLocationAdapter {
    final private Location location;

    public ApexLocationAdapter(Location location) {
        this.location = location;
    }

    public int getStartIndex() {
        return location.startByteOffset();
    }

    public int getEndIndex() {
        return location.endByteOffset();
    }

    public int getLine() {
        return location.startLine();
    }

    public int getColumn() {
        return location.startLineOffset();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (!(other instanceof ApexLocationAdapter)) return false;
        ApexLocationAdapter otherLocation = (ApexLocationAdapter) other;
        return getStartIndex() == otherLocation.getStartIndex() &&
                getEndIndex() == otherLocation.getEndIndex() &&
                getLine() == otherLocation.getLine() &&
                getColumn() == otherLocation.getColumn();
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + getStartIndex();
        hash = 31 * hash + getEndIndex();
        hash = 31 * hash + getLine();
        hash = 31 * hash + getColumn();
        return hash;
    }
}
