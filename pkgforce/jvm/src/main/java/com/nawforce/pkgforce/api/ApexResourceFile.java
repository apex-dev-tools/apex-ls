package com.nawforce.pkgforce.api;

import com.financialforce.oparser.TypeDeclaration;

import java.util.List;

public class ApexResourceFile {

    private final String filename;
    private final List<TypeDeclaration> types;
    private final boolean hasErrors;

    ApexResourceFile(String filename, List<TypeDeclaration> types, boolean hasErrors) {
        this.filename = filename;
        this.types = types;
        this.hasErrors = hasErrors;
    }

    public List<TypeDeclaration> getTypes() {
        return types;
    }

    public String getFilename() {
        return filename;
    }

    public boolean hasError() {
        return hasErrors;
    }
}
