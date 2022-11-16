/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.api;

import java.util.List;

/* A representation of an Apex type, can also act as an ApexTypeId for that type. */
public interface ApexType extends ApexTypeId {

    ApexResourceFile getFile();
    boolean isSObject();
    ApexTypeId getParent();
    List<ApexTypeId> getInterfaces();
    String getModifiers();
    List<ApexMethod> getMethods();
    List<ApexField> getFields();
}
