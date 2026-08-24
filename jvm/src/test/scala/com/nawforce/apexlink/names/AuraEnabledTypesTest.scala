/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.apexlink.names

import com.nawforce.pkgforce.names.{Name, TypeName}
import org.scalatest.funsuite.AnyFunSuite

class AuraEnabledTypesTest extends AnyFunSuite {

  private def typeName(namespace: String, name: String, params: TypeName*): TypeName =
    TypeName(Name(name), params, Some(TypeName(Name(namespace))))

  test("Namespaced type is disallowed") {
    assert(AuraEnabledTypes.isDisallowed(typeName("Schema", "SObjectType")))
    assert(AuraEnabledTypes.isDisallowed(typeName("Database", "QueryLocator")))
    assert(AuraEnabledTypes.isDisallowed(typeName("System", "Exception")))
    assert(AuraEnabledTypes.isDisallowed(typeName("Package", "Version")))
    assert(AuraEnabledTypes.isDisallowed(typeName("eventbus", "ChangeEventHeader")))
  }

  test("Unlisted type is allowed") {
    assert(!AuraEnabledTypes.isDisallowed(typeName("System", "String")))
    assert(!AuraEnabledTypes.isDisallowed(typeName("Database", "DMLOptions")))
    assert(!AuraEnabledTypes.isDisallowed(typeName("Schema", "DisplayType")))
  }

  test("Matching is case insensitive, as Apex identifiers are") {
    assert(AuraEnabledTypes.isDisallowed(typeName("SCHEMA", "SOBJECTTYPE")))
    assert(AuraEnabledTypes.isDisallowed(typeName("schema", "sobjecttype")))
    assert(AuraEnabledTypes.isDisallowed(typeName("EVENTBUS", "ChangeEventHeader")))
    assert(AuraEnabledTypes.isDisallowed(typeName("PACKAGE", "version")))
  }

  test("Only a two level name matches") {
    // A name in the right namespace but nested one level deeper is not the listed type
    assert(
      !AuraEnabledTypes.isDisallowed(
        TypeName(
          Name("SObjectType"),
          Nil,
          Some(TypeName(Name("Schema"), Nil, Some(TypeName(Name("Outer")))))
        )
      )
    )
    // An unqualified name is not enough, a user type could share the name
    assert(!AuraEnabledTypes.isDisallowed(TypeName(Name("SObjectType"))))
    assert(!AuraEnabledTypes.isDisallowed(TypeName(Name("Version"))))
  }

  test("Type arguments take no part in matching the type itself") {
    // System.Set is listed and is only ever written with an argument
    assert(AuraEnabledTypes.isDisallowed(typeName("System", "Set", TypeName(Name("String")))))
    // System.List is not listed, whatever it holds
    assert(!AuraEnabledTypes.isDisallowed(typeName("System", "List", TypeName(Name("String")))))
  }

  test("Disallowed type nested anywhere in the type arguments is found") {
    val sObjectType   = typeName("Schema", "SObjectType")
    val listOfSObject = typeName("System", "List", typeName("System", "SObject"))
    assert(AuraEnabledTypes.isDisallowed(typeName("System", "Map", sObjectType, listOfSObject)))
    assert(
      AuraEnabledTypes.isDisallowed(
        typeName("System", "List", typeName("System", "Map", TypeName(Name("String")), sObjectType))
      )
    )
  }

  test("A namespace qualifier carrying arguments does not match") {
    assert(
      !AuraEnabledTypes.isDisallowed(
        TypeName(
          Name("SObjectType"),
          Nil,
          Some(TypeName(Name("Schema"), Seq(TypeName(Name("Account"))), None))
        )
      )
    )
  }
}
