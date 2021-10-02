/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.types

import com.nawforce.apexlink.org.PackageImpl
import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class ExtendedTest extends AnyFunSuite with TestHelper {

  test("No type arguments on standard class") {
    FileSystemHelper.run(
      Map("Dummy.cls" -> "public class Dummy<T> { }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Dummy.cls") ==
        "Error: line 1 at 19-20: Class type arguments can only by used by 'Extended' Apex classes\n")
    }
  }

  test("Mismatched file and class names") {
    FileSystemHelper.run(
      Map("Foo.xcls" -> "public class Dummy<T> { }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.xcls") ==
        "Error: line 1 at 13-18: Type name 'Dummy' does not match file name 'Foo.xcls'\n")
    }
  }

  test("Mismatched file and class names (case insensitive)") {
    FileSystemHelper.run(
      Map("dummy.xcls" -> "public class DuMmy<T> { }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Type arguments allowed on extended class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<T> { }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Multiple arguments allowed on extended class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<T, S, R> { }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Duplicate arguments on extended class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<T, S, t> { }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Dummy.xcls") ==
        "Error: line 1 at 25-26: Duplicate type argument for 'T'\n")
    }
  }

  test("Extended class reference in Apex class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy { }",
        "Foo.cls" -> "public class Foo { {Dummy a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Extended class reference in Apex class (xcls layer)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"packageDirectories\":[{\"path\":\"pkg\"}],\"plugins\":{\"xcls\":{\"path\":\"xcls\"}}}",
        "xcls/Dummy.xcls" -> "public class Dummy { }",
        "pkg/Foo.cls" -> "public class Foo { {Dummy a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class reference") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy_String a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class reference dual arguments") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X, Y> { }",
        "Foo.cls" -> "public class Foo { {Dummy_String_Integer a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class reference with non-namespaced generic arg") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy_List_String a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class reference with namespaced generic arg") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy_SystemList_String a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Inner Generic Extended class reference in Apex class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy { public class Inner<X> {} }",
        "Foo.cls" -> "public class Foo { {Dummy_Inner_Bar a;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Dummy.xcls") ==
        "Error: line 1 at 40-41: Class type arguments can only by used by outer classes\n")
      assert(org.issues.getMessages("/Foo.cls") ==
        "Missing: line 1 at 36-37: No type declaration found for 'Dummy_Inner_Bar'\n")
    }
  }

  test("Generic Extended class bad reference in Apex class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy_Bar a;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Missing: line 1 at 30-31: No type declaration found for 'Dummy_Bar'\n")
    }
  }

  test("Generic Extended class nested reference in Apex class") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy_Dummy_String a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class without type argument") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class too many type arguments") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { }",
        "Foo.cls" -> "public class Foo { {Dummy_String_String a;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Missing: line 1 at 40-41: No type declaration found for 'Dummy_String_String'\n")
    }
  }

  test("Generic Extended class missing type argument") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X, Y> { }",
        "Foo.cls" -> "public class Foo { {Dummy_String a;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Missing: line 1 at 33-34: No type declaration found for 'Dummy_String'\n")
    }
  }

  test("Generic Extended class namespace referenced") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"namespace\": \"ns\", \"packageDirectories\":[{\"path\":\"pkg\"}],\"plugins\":{\"xcls\":{\"path\":\"xcls\"}}}",
        "xcls/Dummy.xcls" -> "public class Dummy<X> { }",
        "pkg/Foo.cls" -> "public class Foo { {Dummy_nsFoo a;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic Extended class namespace referenced missing arg") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"namespace\": \"ns\", \"packageDirectories\":[{\"path\":\"pkg\"}],\"plugins\":{\"xcls\":{\"path\":\"xcls\"}}}",
        "xcls/Dummy.xcls" -> "public class Dummy<X, Y> { }",
        "pkg/Foo.cls" -> "public class Foo { {Dummy_nsFoo a;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/pkg/Foo.cls") ==
        "Missing: line 1 at 32-33: No type declaration found for 'Dummy_nsFoo'\n")
    }
  }

  test("Generic instance field referenced") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> {X field; }",
        "Foo.cls" -> "public class Foo { {Dummy_String a; Integer b; b = a.field;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Error: line 1 at 47-58: Incompatible types in assignment, from 'System.String' to 'System.Integer'\n")
    }
  }

  test("Generic static field referenced") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> {static X field; }",
        "Foo.cls" -> "public class Foo { { Integer b; b = Dummy_String.field;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Missing: line 1 at 36-54: No type declaration found for 'X'\n")
    }
  }

  test("Generic instance property referenced") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> {X property {set; get;} }",
        "Foo.cls" -> "public class Foo { {Dummy_String a; Integer b; b = a.property;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Error: line 1 at 47-61: Incompatible types in assignment, from 'System.String' to 'System.Integer'\n")
    }
  }

  test("Generic static property referenced") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> {static X property {set; get;} }",
        "Foo.cls" -> "public class Foo { { Integer b; b = Dummy_String.property;} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Missing: line 1 at 36-57: No type declaration found for 'X'\n")
    }
  }

  test("Generic instance method referenced") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> {public X myMethod(X arg) {}}",
        "Foo.cls" -> "public class Foo { {Dummy_String a; Integer b; b = a.myMethod('');} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Error: line 1 at 47-65: Incompatible types in assignment, from 'System.String' to 'System.Integer'\n")
    }
  }

  test("Generic static method referenced") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> {public static X myMethod() {}}",
        "Foo.cls" -> "public class Foo { {Integer b; b = Dummy_String.myMethod();} }")) { root: PathLike =>
      val org = createOrg(root)
      assert(org.issues.getMessages("/Foo.cls") ==
        "Error: line 1 at 31-58: Incompatible types in assignment, from 'Dummy.X' to 'System.Integer'\n")
    }
  }

  test("Generic new reference") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { {Object a = new Dummy_X();} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic method call on type argument") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { X value; {value.foo();} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Generic field reference on type argument") {
    FileSystemHelper.run(
      Map("Dummy.xcls" -> "public class Dummy<X> { X value; {Object o = value.foo;} }")) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  private def refresh(pkg: PackageImpl, path: PathLike, source: String): Unit = {
    path.write(source)
    pkg.refresh(path)
  }

  test("Refresh fixes bad method") {
    withManualFlush {
      FileSystemHelper.run(
        Map("Dummy.xcls" -> "public class Dummy<X> {public X myMethod(X arg) {}}",
          "Foo.cls" -> "public class Foo { {Dummy_String a; Integer b; b = a.otherMethod('');} }")) { root: PathLike =>
        val org = createOrg(root)
        assert(org.issues.getMessages("/Foo.cls") ==
          "Error: line 1 at 51-68: No matching method found for 'otherMethod' on 'Dummy<System.String>' taking arguments 'System.String'\n")

        val pkg = org.unmanaged
        refresh(pkg, root.join("Foo.cls"), "public class Foo { {Dummy_String a; String b; b = a.myMethod('');} }")
        assert(org.flush())
        assert(!org.issues.hasErrorsOrWarnings)
      }
    }
  }

  test("Refresh fixes bad method (reversed)") {
    withManualFlush {
      FileSystemHelper.run(
        Map("Dummy.xcls" -> "public class Dummy<X> {public X myMethod(X arg) {}}",
          "Foo.cls" -> "public class Foo { {Dummy_String a; String b; b = a.otherMethod('');} }")) { root: PathLike =>
        val org = createOrg(root)
        assert(org.issues.getMessages("/Foo.cls") ==
          "Error: line 1 at 50-67: No matching method found for 'otherMethod' on 'Dummy<System.String>' taking arguments 'System.String'\n")

        val pkg = org.unmanaged
        refresh(pkg, root.join("Dummy.xcls"), "public class Dummy<X> {public X otherMethod(X arg) {}}")
        assert(org.flush())
        assert(!org.issues.hasErrorsOrWarnings)
      }
    }
  }
}