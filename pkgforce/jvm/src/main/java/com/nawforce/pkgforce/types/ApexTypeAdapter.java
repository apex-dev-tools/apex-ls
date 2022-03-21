/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.types;

import com.financialforce.oparser.TypeDeclaration;
import com.financialforce.oparser.TypeRef;
import com.nawforce.pkgforce.api.*;
import com.nawforce.runtime.workspace.IPM;
import com.nawforce.runtime.workspace.ModuleScoped$;
import scala.collection.mutable.ArrayBuffer;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

// TODO: Annotations & enum constants as fields?
public class ApexTypeAdapter implements ApexType {
    final private TypeDeclaration td;

    public ApexTypeAdapter(TypeDeclaration td) {
        this.td = td;
    }

    @Override
    public ApexResourceFile getFile() {
        Boolean hasErrors = index()
                .map(index -> index.issues()
                        .issuesForFiles(new String[]{td.path()}, false, 1).length > 0)
                .orElse(false);
        return new ApexResourceFile(td.path(), getTypes(td), hasErrors);
    }

    private List<ApexType> getTypes(TypeDeclaration td) {
        if (td.enclosing().nonEmpty())
            return getTypes(td.enclosing().get());

        ApexType[] types = new ApexType[1 + td.innerTypes().length()];
        types[0] = new ApexTypeAdapter(td);
        for (int i = 0; i < td.innerTypes().length(); i++)
            types[1 + i] = new ApexTypeAdapter(td.innerTypes().apply(i));
        return Arrays.asList(types);
    }

    @Override
    public String getApexName() {
        String name = td.id().get().toString();
        if (td.enclosing().nonEmpty())
            name = td.enclosing().get().id().get().toString() + "." + name;
        String ns = getApexNamespace();
        if (ns.length() != 0)
            name = ns + "." + name;
        return name;
    }

    @Override
    public String getApexNamespace() {
        return pkg().map(IPM.Package::namespaceAsString).orElse("");
    }

    @Override
    public ApexType getEnclosingType() {
        if (!module().isPresent() || td.enclosing().isEmpty())
            return null;

        return new ApexTypeAdapter(td.enclosing().get());
    }

    @Override
    public boolean isSObject() {
        // TODO: This is determined by how the type was loaded, we will want something better
        return false;
    }

    @Override
    public ApexTypeId getParent() {
        if (td.extendsTypeRef().isEmpty())
            return null;

        // TODO: Namespace handling
        return new NameApexTypeId(td.extendsTypeRef().toString(), "");
    }

    @Override
    public List<ApexTypeId> getInterfaces() {
        if (td.implementsTypeList().isEmpty())
            return null;

        ArrayBuffer<TypeRef> refs = td.implementsTypeList().get().typeRefs();
        NameApexTypeId[] interfaces = new NameApexTypeId[refs.length()];
        int entry = 0;
        for (int i = 0; i < td.constructors().length(); i++)
            // TODO: Fix namespace
            interfaces[entry++] = new NameApexTypeId(refs.apply(i).toString(), "");
        return Arrays.asList(interfaces);
    }

    @Override
    public String getModifiers() {
        return td.modifiers().mkString(" ");
    }

    @Override
    public List<ApexMethod> getMethods() {
        ApexMethod[] methods = new ApexMethod[td.constructors().length() + td.methods().length()];
        int entry = 0;
        for (int i = 0; i < td.constructors().length(); i++)
            methods[entry++] = new ApexConstructorAdapter(this, td.constructors().apply(i));
        for (int i = 0; i < td.methods().length(); i++)
            methods[entry++] = new ApexMethodAdapter(this, td.methods().apply(i));
        return Arrays.asList(methods);
    }

    @Override
    public List<ApexField> getFields() {
        ApexField[] fields = new ApexField[td.properties().length() + td.fields().length()];
        int entry = 0;
        for (int i = 0; i < td.properties().length(); i++)
            fields[entry++] = new ApexPropertyAdapter(this, td.properties().apply(i));
        for (int i = 0; i < td.fields().length(); i++)
            fields[entry++] = new ApexFieldAdapter(this, td.fields().apply(i));
        return Arrays.asList(fields);
    }

    private Optional<IPM.Index> index() {
        return pkg().map(IPM.Package::org);
    }

    private Optional<IPM.Package> pkg() {
        return module().map(IPM.Module::pkg);
    }

    private Optional<IPM.Module> module() {
        return Optional.ofNullable(ModuleScoped$.MODULE$.module(td).getOrElse(null));
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (getClass() != other.getClass()) return false;
        return td == ((ApexTypeAdapter) other).td;
    }
}