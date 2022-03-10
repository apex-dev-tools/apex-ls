/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

/* A representation of an Apex type, can also act as an ApexTypeId for that type. */
public interface ApexType extends ApexTypeId {

    ApexResourceFile getFile();

    /* TODO
    List<ApexMethod> getMethods();
    List<ApexField> getFields();
    IString getModifiers();
    IString getUnitType();
    boolean isSObject();
     */
}
