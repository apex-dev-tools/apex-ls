/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import apex.jorje.data.Location;

public class ApexLocationAdapter implements Location {
    final private com.financialforce.oparser.Location location;

    public ApexLocationAdapter(com.financialforce.oparser.Location location) {
        this.location = location;
    }

    @Override
    public int getStartIndex() {
        return location.startByteOffset();
    }

    @Override
    public int getEndIndex() {
        return location.endByteOffset();
    }

    @Override
    public int getLine() {
        return location.startLine();
    }

    @Override
    public int getColumn() {
        return location.startLineOffset();
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (!(other instanceof Location)) return false;
        Location otherLocation = (Location) other;
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
