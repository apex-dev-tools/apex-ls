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
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class MethodTest extends AnyFunSuite with TestHelper {

  test("Method call instance->instance") {
    typeDeclaration("public class Dummy {void f1(){} void f2() {f1();} }")
    assert(dummyIssues.isEmpty)
  }

  test("Method call static->static") {
    typeDeclaration("public class Dummy {static void f1(){} static void f2() {f1();} }")
    assert(dummyIssues.isEmpty)
  }

  test("Method call static->instance") {
    typeDeclaration("public class Dummy {void f1(){} static void f2() {f1();} }")
    assert(
      dummyIssues == "Missing: line 1 at 50-54: No matching method found for 'f1' on 'Dummy' taking no arguments, are you trying to call the instance method 'private void f1()' from a static context?\n"
    )
  }

  test("Method call instance->static") {
    typeDeclaration("public class Dummy {static void f1(){} void f2() {f1();} }")
    assert(dummyIssues.isEmpty)
  }

  test("Method call wrong arguments") {
    typeDeclaration("public class Dummy {static void f1(String a){} void f2() {f1();} }")
    assert(
      dummyIssues == "Missing: line 1 at 58-62: No matching method found for 'f1' on 'Dummy' taking no arguments, did you mean to call 'private static void f1(System.String a)'?\n"
    )
  }

  test("Method call with ambiguous target") {
    typeDeclaration(
      "public class Dummy { {Dummy d; d.func(null);} void func(List<Integer> a) {} void func(Integer b) {} }"
    )
    assert(
      dummyIssues == "Missing: line 1 at 31-43: Ambiguous method call for 'func' on 'Dummy' taking arguments 'null', wrong argument types for calling 'private void func(System.Integer b)'\n"
    )
  }

  test("Method call ambiguous target strict match") {
    typeDeclaration("public class Dummy { {Decimal.valueOf(1); } }")
    assert(dummyIssues.isEmpty)
  }

  test("Method call with non-ambiguous target") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "public virtual class A {}",
        "B.cls" -> "public virtual class B extends A {}",
        "Dummy.cls" -> "public class Dummy extends B { {Dummy d; d.func(d);} void func(A a) {} void func(B b) {} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(getMessages(root.join("Dummy.cls")) == "")
    }
  }

  test("Platform generic interface params duplicate") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy { void run(Database.Batchable<SObject> arg) {} void run(Database.Batchable<String> arg) {} }"
      )
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 71-74: Method 'run' can not use same platform generic interface as existing method at line 1 at 26-29\n"
    )
  }

  test("Method call using platform generic interface with wrong type argument type") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy { void run(Database.Batchable<SObject> arg) {} {Database.Batchable<Object> a; run(a);} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("SObject generic params duplicate") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy { void run(List<SObject> arg) {} void run(List<Account> arg) {} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("SObject generic params fulfills interface") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements API { public interface API {void run(List<SObject> arg);} public void run(List<Account> arg) {} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("String params fulfills Id interface") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements API { public interface API {void run(Id arg);} public void run(String arg) {} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Id params fulfills String interface") {
    typeDeclarations(
      Map(
        "Dummy.cls" ->
          "public class Dummy implements API { public interface API {void run(String arg);} public void run(Id arg) {} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Method call for possible synthetic platform method") {
    typeDeclaration("public class Dummy { {Database.QueryLocatorIterator it; it.next(); } }")
    assert(dummyIssues.isEmpty)
  }

  test("Method call basic database query") {
    typeDeclaration("public class Dummy { {List<SObject> a = Database.query(''); } }")
    assert(dummyIssues.isEmpty)
  }

  test("Method call for possible ambiguous database query") {
    typeDeclaration(
      "public class Dummy {void f1(List<SObject> a){} void f1(SObject a) {} {f1(Database.query('')); } }"
    )
    assert(dummyIssues.isEmpty)
  }

  test("Method call custom equals (bad param type)") {
    typeDeclarations(
      Map(
        "MyInterface.cls" -> "public interface MyInterface {}",
        "Dummy.cls" -> "public class Dummy implements MyInterface { public Boolean equals(MyInterface other) {return true;} {Boolean b = equals(this);} }"
      )
    )
    assert(dummyIssues.isEmpty)
  }

  test("Static method private override different return") {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Base.cls" -> "public virtual class Base { static Base getInstance() {return null;} }",
          "Extend.cls" -> "public class Extend extends Base { static Extend getInstance() {return null;} { getInstance();} }"
        )
      ) { root: PathLike =>
        createHappyOrg(root)
      }
    }
  }

  test("Static method public override different return") {
    FileSystemHelper.run(
      Map(
        "Base.cls" -> "public virtual class Base { static public Base getInstance() {return null;} }",
        "Dummy.cls" -> "public class Dummy extends Base { static public Dummy getInstance() {return null;} { getInstance();} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Static aura enabled method with allowed params and return types") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { @AuraEnabled public static String fn(List<String> ids){return null;} }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Static aura enabled method with disallowed param") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { @AuraEnabled public static void fn(Set<String> ids){} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Dummy.cls")
        ) == "Error: line 1 at 13-18: AuraEnabled methods do not support parameter type of System.Set<System.String>\n"
      )
    }
  }

  test("Static aura enabled method with disallowed inner param") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { @AuraEnabled public static void fn(List<Set<String>> ids){} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Dummy.cls")
        ) == "Error: line 1 at 13-18: AuraEnabled methods do not support parameter type of System.List<System.Set<System.String>>\n"
      )
    }
  }

  test("Static aura enabled method with disallowed return type") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { @AuraEnabled public static Set<Id> fn(String ids){ return null;} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Dummy.cls")
        ) == "Error: line 1 at 13-18: AuraEnabled methods do not support return type of System.Set<System.Id>\n"
      )
    }
  }

  test("Static aura enabled method with return typ with disallowed inner type") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { @AuraEnabled public static Map<String,Set<Id>> fn(String ids){ return null;} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Dummy.cls")
        ) == "Error: line 1 at 13-18: AuraEnabled methods do not support return type of System.Map<System.String, System.Set<System.Id>>\n"
      )
    }
  }

  test("Static method with protected modifier") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public class Dummy { static protected Dummy getInstance() {return null;} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Dummy.cls")
        ) == "Error: line 1 at 44-55: protected method 'getInstance' cannot be static\n"
      )
    }
  }

  test("Instance method private none-override different return") {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Base.cls" -> "public virtual class Base { Base getInstance() {return null;} }",
          "Dummy.cls" -> "public class Dummy extends Base { Dummy getInstance() {return null;} { this.getInstance();} }"
        )
      ) { root: PathLike =>
        createHappyOrg(root)
      }
    }
  }

  test("Instance method private none-override same return") {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Base.cls" -> "public virtual class Base { void getInstance() {} }",
          "Dummy.cls" -> "public class Dummy extends Base { void getInstance() {return;} { this.getInstance();} }"
        )
      ) { root: PathLike =>
        createHappyOrg(root)
      }
    }
  }

  test("Instance method protected override different return") {
    FileSystemHelper.run(
      Map(
        "Base.cls" -> "public virtual class Base { protected virtual Base getInstance() {return null;} }",
        "Extend.cls" -> "public class Extend extends Base { protected override Extend getInstance() {return null;} { this.getInstance();} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(root.join("Extend.cls")) ==
          "Error: line 1 at 61-72: Method 'getInstance' has wrong return type to override, should be 'Base'\n"
      )
    }
  }

  test("Instance method public override different return") {
    FileSystemHelper.run(
      Map(
        "Base.cls" -> "public virtual class Base { public virtual Base getInstance() {return null;} }",
        "Extend.cls" -> "public class Extend extends Base { public override Extend getInstance() {return null;} { this.getInstance();} }"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(root.join("Extend.cls")) ==
          "Error: line 1 at 58-69: Method 'getInstance' has wrong return type to override, should be 'Base'\n"
      )
    }
  }

  test("private abstract method implementation") {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Base.cls"   -> "public abstract class Base { abstract void fn(); }",
          "Extend.cls" -> "public class Extend extends Base { void fn() {}}"
        )
      ) { root: PathLike =>
        createHappyOrg(root)
      }
    }
  }

  test("public method implementing a private abstract method") {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Base.cls"   -> "public abstract class Base { abstract void fn(); }",
          "Extend.cls" -> "public class Extend extends Base { public void fn() {}}"
        )
      ) { root: PathLike =>
        createHappyOrg(root)
      }
    }
  }

  test("public abstract method implementation with no override keyword") {
    FileSystemHelper.run(
      Map(
        "Base.cls"   -> "public abstract class Base { public abstract void fn(); }",
        "Extend.cls" -> "public class Extend extends Base { public void fn() {}}"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Extend.cls")
        ) == "Error: line 1 at 47-49: Method 'fn' must use the 'override' keyword when implementing an abstract method\n"
      )
    }
  }

  test("protected abstract method implementation with no override keyword") {
    FileSystemHelper.run(
      Map(
        "Base.cls"   -> "public abstract class Base { protected abstract void fn(); }",
        "Extend.cls" -> "public class Extend extends Base { protected void fn() {}}"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Extend.cls")
        ) == "Error: line 1 at 50-52: Method 'fn' must use the 'override' keyword when implementing an abstract method\n"
      )
    }
  }

  test("private inner abstract method implementation with no override keyword") {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Dummy.cls" -> "public abstract class Dummy { abstract void fn(); class inner extends Dummy { void fn() {} }}"
        )
      ) { root: PathLike =>
        createHappyOrg(root)
      }
    }
  }

  test(
    "private inner abstract method implementation with no override keyword on public implementation"
  ) {
    withAllowPrivateOverride {
      FileSystemHelper.run(
        Map(
          "Dummy.cls" -> "public abstract class Dummy { abstract void fn(); class inner extends Dummy { public void fn(){} }}"
        )
      ) { root: PathLike =>
        createOrg(root)
        assert(
          getMessages(
            root.join("Dummy.cls")
          ) == "Error: line 1 at 90-92: Method 'fn' must use the 'override' keyword when implementing an abstract method\n"
        )
      }
    }
  }

  test("public inner abstract method implementation with no override keyword") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "public abstract class Dummy { public abstract void fn(); class inner extends Dummy { public void fn() {} }}"
      )
    ) { root: PathLike =>
      createOrg(root)
      assert(
        getMessages(
          root.join("Dummy.cls")
        ) == "Error: line 1 at 97-99: Method 'fn' must use the 'override' keyword when implementing an abstract method\n"
      )
    }
  }

  test("Method call with ghosted type") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |"packageDirectories": [{"path": "force-app"}],
          |"plugins": {"dependencies": [{"namespace": "ext"}]}
          |}""".stripMargin,
        "force-app/Dummy.cls" -> "public class Dummy { {ext.Something a; String.escapeSingleQuotes(a); } }"
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }

  test("Ambiguous Method call with ghosted type") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          |"packageDirectories": [{"path": "force-app"}],
          |"plugins": {"dependencies": [{"namespace": "ext"}]}
          |}""".stripMargin,
        "force-app/Dummy.cls" ->
          """public class Dummy { {
            |ext.Something a;
            | // Legal as should return an Any as we don't know which 'publish' was intended
            |String b = EventBus.publish(a); } }
            |""".stripMargin
      )
    ) { root: PathLike =>
      createHappyOrg(root)
    }
  }
}
