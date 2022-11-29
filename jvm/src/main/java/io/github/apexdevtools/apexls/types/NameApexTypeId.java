/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.types;

import com.financialforce.types.base.TypeRef;
import io.github.apexdevtools.apexls.api.ApexTypeId;
import com.nawforce.pkgforce.names.TypeName;
import com.nawforce.runtime.workspace.IModuleTypeDeclaration;
import scala.Option;

import java.util.Objects;

/*
 * An ApexTypeId just constructed from a name. The apexName is required to include the namespace. The namespace may
 * not be null but may be "" to indicate unmanaged. Safe to use with generics, but it does not provide access to type
 * parameters.
 */
public class NameApexTypeId implements ApexTypeId {
    final private String apexName;
    final private String namespace;
    final private boolean isResolved;

    public NameApexTypeId(String apexName, String namespace, boolean isResolved) {
        assert namespace.length() == 0 || apexName.startsWith(namespace + '.');
        this.apexName = apexName;
        this.namespace = namespace;
        this.isResolved = isResolved;
    }

    public static NameApexTypeId apply(TypeRef typeRef) {
        if (typeRef == null)
            return null;
        if (typeRef instanceof IModuleTypeDeclaration) {
            IModuleTypeDeclaration td = (IModuleTypeDeclaration)typeRef;
            return new NameApexTypeId(td.fullName(), td.namespaceAsString(), true);
        } else {
            return new NameApexTypeId(typeRef.fullName(), "", false);
        }
    }

    public static NameApexTypeId apply(Option<TypeRef> typeRef) {
        if (typeRef.isEmpty()) return null;
        return apply(typeRef.get());
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

        return new NameApexTypeId(typeName.outer().get().toString(), namespace, isResolved);
    }

    @Override
    public boolean isResolved() {
        return isResolved;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (getClass() != other.getClass()) return false;
        NameApexTypeId otherTypeId = (NameApexTypeId) other;
        return isResolved == otherTypeId.isResolved &&
                apexName.equals(otherTypeId.apexName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(isResolved, apexName);
    }

}
