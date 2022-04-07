/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

import java.util.List;

public class ApexResourceFile {

    private final String filename;
    private final List<ApexType> types;
    private final boolean hasErrors;

    public ApexResourceFile(String filename, List<ApexType> types, boolean hasErrors) {
        this.filename = filename;
        this.types = types;
        this.hasErrors = hasErrors;
    }

    public List<ApexType> getTypes() {
        return types;
    }

    public String getFilename() {
        return filename;
    }

    public boolean hasError() {
        return hasErrors;
    }
}
