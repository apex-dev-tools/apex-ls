/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class CreationTest extends AnyFunSuite with TestHelper {

  test("Basic new") {
    typeDeclaration("public class Dummy {{Object a = new Address();}}")
    assert(dummyIssues.isEmpty)
  }

  test("Inner class") {
    typeDeclaration(
      "public class Dummy {{Object a = new Messaging.InboundEmail.BinaryAttachment();}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Basic with list constructor") {
    typeDeclaration("public class Dummy {{Object a = new Address{1};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 43-46: Expression list construction is only supported for Set or List types, not 'System.Address'\n"
    )
  }

  test("Basic with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new Address{1 => 2};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 43-51: Expression pair list construction is only supported for Map types, not 'System.Address'\n"
    )
  }

  test("Basic SObject") {
    typeDeclaration("public class Dummy {{Object a = new Account();}}")
    assert(dummyIssues.isEmpty)
  }

  test("Basic SObject with params") {
    typeDeclaration("public class Dummy {{Object a = new Account(Name='Hello', Jigsaw='Bad');}}")
    assert(dummyIssues.isEmpty)
  }

  test("Basic SObject with list constructor") {
    typeDeclaration("public class Dummy {{Object a = new Account{1};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 43-46: Expression list construction is only supported for Set or List types, not 'Schema.Account'\n"
    )
  }

  test("Basic SObject with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new Account{1 => 2};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 43-51: Expression pair list construction is only supported for Map types, not 'Schema.Account'\n"
    )
  }

  test("Empty Map") {
    typeDeclaration("public class Dummy {{Object a = new Map<String, Address>();}}")
    assert(dummyIssues.isEmpty)
  }

  test("Map with argument") {
    typeDeclaration(
      "public class Dummy {{Object a = new Map<String, Address>{'1' => new Address()};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Map with list constructor") {
    typeDeclaration("public class Dummy {{Object a = new Map<String, Address>{1, 2};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 56-62: Expression list construction is only supported for Set or List types, not 'System.Map<System.String, System.Address>'\n"
    )
  }

  test("Map with any key type") {
    // SOQL currently produces an any result, this should break if we fix that as String != RecordSet<Account>
    typeDeclaration(
      "public class Dummy {{Object a = new Map<SObject, String>{[Select Id From Account] => ''};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Map with any value type") {
    // SOQL currently produces an any result, this should break if we fix that as String != RecordSet<Account>
    typeDeclaration(
      "public class Dummy {{Object a = new Map<String, SObject>{'' => [Select Id From Account]};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Map with incorrect key type") {
    typeDeclaration("public class Dummy {{Object a = new Map<Id, String>{new Account() => null};}}")
    assert(
      dummyIssues == "Error: line 1 at 51-74: Incompatible key type 'Schema.Account' for 'System.Id'\n"
    )
  }

  test("Map with incorrect value type") {
    typeDeclaration("public class Dummy {{Object a = new Map<Id, String>{null => new Account()};}}")
    assert(
      dummyIssues == "Error: line 1 at 51-74: Incompatible value type 'Schema.Account' for 'System.String'\n"
    )
  }

  test("Empty List") {
    typeDeclaration("public class Dummy {{Object a = new List<Address>();}}")
    assert(dummyIssues.isEmpty)
  }

  test("List with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new List<Address>{1 => 2};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 49-57: Expression pair list construction is only supported for Map types, not 'System.List<System.Address>'\n"
    )
  }

  test("List with assignable arguments") {
    typeDeclaration(
      "public class Dummy {{Object a = new List<Address>{new Address(), new Address()};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("List with non-assignable arguments") {
    typeDeclaration("public class Dummy {{Object a = new List<Integer>{'Hello'};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 49-58: Expression of type 'System.String' can not be assigned to System.Integer'\n"
    )
  }

  test("List with assignable arguments to Object") {
    typeDeclaration(
      "public class Dummy {{Object a = new List<Object>{new Address(), new Dummy()};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("List with assignable arguments to SObject") {
    typeDeclaration("public class Dummy {{Object a = new List<SObject>{new Account()};}}")
    assert(dummyIssues.isEmpty)
  }

  test("List with non-assignable arguments to SObject") {
    typeDeclaration("public class Dummy {{Object a = new List<SObject>{'Hello'};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 49-58: Expression of type 'System.String' can not be assigned to System.SObject'\n"
    )
  }

  test("Empty Set") {
    typeDeclaration("public class Dummy {{Object a = new Set<Address>();}}")
    assert(dummyIssues.isEmpty)
  }

  test("Set with map constructor") {
    typeDeclaration("public class Dummy {{Object a = new Set<Address>{1 => 2};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 48-56: Expression pair list construction is only supported for Map types, not 'System.Set<System.Address>'\n"
    )
  }

  test("Set with assignable arguments") {
    typeDeclaration(
      "public class Dummy {{Object a = new Set<Address>{new Address(), new Address()};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Set with non-assignable arguments") {
    typeDeclaration("public class Dummy {{Object a = new Set<Integer>{'Hello'};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 48-57: Expression of type 'System.String' can not be assigned to System.Integer'\n"
    )
  }

  test("Set with assignable arguments to Object") {
    typeDeclaration(
      "public class Dummy {{Object a = new Set<Object>{new Address(), new Dummy()};}}"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Set with assignable arguments to SObject") {
    typeDeclaration("public class Dummy {{Object a = new Set<SObject>{new Account()};}}")
    assert(dummyIssues.isEmpty)
  }

  test("Set with non-assignable arguments to SObject") {
    typeDeclaration("public class Dummy {{Object a = new Set<SObject>{'Hello'};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 48-57: Expression of type 'System.String' can not be assigned to System.SObject'\n"
    )
  }

  test("Array with literal Integer index") {
    typeDeclaration("public class Dummy {{List<Object> a = new Address[0];}}")
    assert(dummyIssues.isEmpty)
  }

  test("Array with Integer value index") {
    typeDeclaration("public class Dummy {{Integer b; List<Object> a = new Address[b];}}")
    assert(dummyIssues.isEmpty)
  }

  test("Array with non-Integer index") {
    typeDeclaration("public class Dummy {{List<Object> a = new Address[true];}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 50-54: Index for array construction must be an Integer, not 'System.Boolean'\n"
    )
  }

  test("Array with assignable arguments") {
    typeDeclaration("public class Dummy {{ List<Object> a = new Address[]{new Address()}; }}")
    assert(dummyIssues.isEmpty)
  }

  test("Array with non-assignable arguments") {
    typeDeclaration("public class Dummy {{Object a = new Integer[]{'Hello'};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 46-53: Expression of type 'System.String' can not be assigned to System.Integer'\n"
    )
  }

  test("Array with assignable arguments to Object") {
    typeDeclaration("public class Dummy {{Object a = new Object[]{new Address(), new Dummy()};}}")
    assert(dummyIssues.isEmpty)
  }

  test("Array with assignable arguments to SObject") {
    typeDeclaration("public class Dummy {{Object a = new SObject[]{new Account()};}}")
    assert(dummyIssues.isEmpty)
  }

  test("Array with non-assignable arguments to SObject") {
    typeDeclaration("public class Dummy {{Object a = new SObject[]{'Hello'};}}")
    assert(
      dummyIssues ==
        "Error: line 1 at 46-53: Expression of type 'System.String' can not be assigned to System.SObject'\n"
    )
  }

}
