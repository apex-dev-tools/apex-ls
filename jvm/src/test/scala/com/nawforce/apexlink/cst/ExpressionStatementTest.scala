/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class ExpressionStatementTest extends AnyFunSuite with TestHelper {

  test("New expression") {
    happyTypeDeclaration("public class Dummy {{ new Dummy(); }}")
  }

  test("Method call expression") {
    happyTypeDeclaration("public class Dummy { void func() { func(); } }")
  }

  test("Method call dot expression") {
    happyTypeDeclaration("public class Dummy { static void func() { Dummy.func(); } }")
  }

  test("Postfix assignment expression") {
    happyTypeDeclaration("public class Dummy {{ Integer i; i++; }}")
  }

  test("Prefix assignment expression") {
    happyTypeDeclaration("public class Dummy {{ Integer i; ++i; }}")
  }

  test("Binary assignment expression") {
    happyTypeDeclaration("public class Dummy {{ Integer i; i&=i; }}")
  }

  test("Sub expression") {
    happyTypeDeclaration("public class Dummy {{ (new Account()); }}")
  }

  test("Field dot expression") {
    typeDeclaration("public class Dummy {{ Account.Name; }}")
    assert(
      dummyIssues == "Error: line 1 at 22-34: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Array expression") {
    typeDeclaration("public class Dummy {{ List<Object> a; a[1]; }}")
    assert(
      dummyIssues == "Error: line 1 at 38-42: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Cast expression") {
    typeDeclaration("public class Dummy {{ (Account)(new Account()); }}")
    assert(
      dummyIssues == "Error: line 1 at 22-46: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Prefix expression") {
    typeDeclaration("public class Dummy {{ Integer i; -i; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-35: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Negation expression") {
    typeDeclaration("public class Dummy {{ Boolean b; !b; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-35: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Arithmetic expression") {
    typeDeclaration("public class Dummy {{ Integer i; i+i; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-36: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Bit expression") {
    typeDeclaration("public class Dummy {{ Integer i; i&i; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-36: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Comparison expression") {
    typeDeclaration("public class Dummy {{ Integer i; i>=i; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-37: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("InstanceOf expression") {
    typeDeclaration("public class Dummy {{ Integer i; i instanceOf Dummy; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-51: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Equality expression") {
    typeDeclaration("public class Dummy {{ Integer i; i <> i; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-39: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

  test("Logical expression") {
    typeDeclaration("public class Dummy {{ Boolean b; b && b; }}")
    assert(
      dummyIssues == "Error: line 1 at 33-39: Only assignment, new & method call expressions can be used as statements\n"
    )
  }

}
