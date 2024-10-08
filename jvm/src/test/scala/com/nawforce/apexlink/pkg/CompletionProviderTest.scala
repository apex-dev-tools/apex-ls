/*
 Copyright (c) 2021 Kevin Jones & FinancialForce, All rights reserved.
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
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.rpc.CompletionItemLink
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite

class CompletionProviderTest extends AnyFunSuite with TestHelper {

  test("Internal Completion") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val content =
        """public class Dummy { public void someMethod() {m}
            |public String methodA(){}
            |public String methodB(String a, String b){}
            |public static String methodStatic(){}
            |private String methodPrivate(){}
            |public String myField;
            |public static String myStaticField;
            |private String myPrivateField;
            |public class MyInner{};
            |private interface MyPrivateInner{};
            |}""".stripMargin.replaceAll("\r\n", "\n")
      val offset      = content.split('\n').head.length - 1
      val completions = org.getCompletionItemsInternal(path, line = 1, offset, content)
      assert(completions.exists(_.kind == "Keyword"))
      assert(
        completions
          .filterNot(_.kind == "Keyword")
          .toSet ==
          Set(
            CompletionItemLink(
              "methodB(a, b)",
              "Method",
              "public System.String methodB(System.String a, System.String b)"
            ),
            CompletionItemLink("methodA()", "Method", "public System.String methodA()"),
            CompletionItemLink(
              "methodPrivate()",
              "Method",
              "private System.String methodPrivate()"
            ),
            CompletionItemLink(
              "methodStatic()",
              "Method",
              "public static System.String methodStatic()"
            ),
            CompletionItemLink("myField", "Field", "public String myField"),
            CompletionItemLink("myStaticField", "Field", "public static String myStaticField"),
            CompletionItemLink("myPrivateField", "Field", "private String myPrivateField"),
            CompletionItemLink("MyInner", "Class", "public"),
            CompletionItemLink("MyPrivateInner", "Interface", "private")
          )
      )
    }
  }

  test("Internal Completion (multiple chars)") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val content =
        """public class Dummy { public void someMethod() {m}
            |public String methodA(){}
            |public String methodB(String a, String b){}
            |public static String methodStatic(){}
            |private String methodPrivate(){}
            |public String myField;
            |public static String myStaticField;
            |private String myPrivateField;
            |public class MyInner{};
            |private interface MyPrivateInner{};
            |}""".stripMargin.replaceAll("\r\n", "\n")
      val offset      = content.split('\n').head.length - 1
      val completions = org.getCompletionItemsInternal(path, line = 1, offset, content)
      assert(completions.exists(_.kind == "Keyword"))
      assert(
        completions
          .filterNot(_.kind == "Keyword")
          .toSet ==
          Set(
            CompletionItemLink(
              "methodB(a, b)",
              "Method",
              "public System.String methodB(System.String a, System.String b)"
            ),
            CompletionItemLink("methodA()", "Method", "public System.String methodA()"),
            CompletionItemLink(
              "methodPrivate()",
              "Method",
              "private System.String methodPrivate()"
            ),
            CompletionItemLink(
              "methodStatic()",
              "Method",
              "public static System.String methodStatic()"
            ),
            CompletionItemLink("myField", "Field", "public String myField"),
            CompletionItemLink("myStaticField", "Field", "public static String myStaticField"),
            CompletionItemLink("myPrivateField", "Field", "private String myPrivateField"),
            CompletionItemLink("MyInner", "Class", "public"),
            CompletionItemLink("MyPrivateInner", "Interface", "private")
          )
      )
    }
  }

  test("Instance Completions") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
          |public String methodA(){}
          |public String methodB(String a, String b){}
          |public static String methodStatic(){}
          |private String methodPrivate(){}
          |public String myField;
          |public static String myStaticField;
          |private String myPrivateField;
          |public class MyInner{};
          |private interface MyPrivateInner{};
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val path    = root.join("Completion.cls")
      val content = "public class Completion { public Completion() {String a = new Dummy().m"
      assert(
        org
          .getCompletionItemsInternal(path, line = 1, offset = content.length, content)
          .toSet ==
          Set(
            CompletionItemLink(
              "methodB(a, b)",
              "Method",
              "public System.String methodB(System.String a, System.String b)"
            ),
            CompletionItemLink("methodA()", "Method", "public System.String methodA()"),
            CompletionItemLink("myField", "Field", "public String myField")
          )
      )
    }
  }

  test("Static Completions") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
          |public String methodA(){}
          |public String methodB(String a, String b){}
          |public static String methodStatic(){}
          |private String methodPrivate(){}
          |public String myField;
          |public static String myStaticField;
          |private String myPrivateField;
          |public class MyInner{};
          |private interface MyPrivateInner{};
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val path    = root.join("Completion.cls")
      val content = "public class Completion { public Completion() {String a = Dummy.m"
      assert(
        org
          .getCompletionItemsInternal(path, line = 1, offset = content.length, content)
          .toSet ==
          Set(
            CompletionItemLink(
              "methodStatic()",
              "Method",
              "public static System.String methodStatic()"
            ),
            CompletionItemLink("myStaticField", "Field", "public static String myStaticField"),
            CompletionItemLink("MyInner", "Class", "public")
          )
      )
    }
  }

  test("Instance Completions (in statement)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
          |public String methodA(){}
          |public String methodB(String a, String b){}
          |public static String methodStatic(){}
          |private String methodPrivate(){}
          |public String myField;
          |public static String myStaticField;
          |private String myPrivateField;
          |public class MyInner{};
          |private interface MyPrivateInner{};
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val path    = root.join("Completion.cls")
      val content = "public class Completion { public Completion() {Dummy a; if ( a.m"
      assert(
        org
          .getCompletionItemsInternal(path, line = 1, offset = content.length, content)
          .toSet ==
          Set(
            CompletionItemLink(
              "methodB(a, b)",
              "Method",
              "public System.String methodB(System.String a, System.String b)"
            ),
            CompletionItemLink("methodA()", "Method", "public System.String methodA()"),
            CompletionItemLink("myField", "Field", "public String myField")
          )
      )
    }
  }

  test("Instance Completions (no context)") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
          |public String methodA(){}
          |public String methodB(String a, String b){}
          |public static String methodStatic(){}
          |private String methodPrivate(){}
          |public String myField;
          |public static String myStaticField;
          |private String myPrivateField;
          |public class MyInner{};
          |private interface MyPrivateInner{};
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val path    = root.join("Completion.cls")
      val content = "public class Completion { public Completion() {String a = new Dummy()."
      assert(
        org
          .getCompletionItemsInternal(path, line = 1, offset = content.length, content)
          .toSet ==
          Set(
            CompletionItemLink("methodA()", "Method", "public System.String methodA()"),
            CompletionItemLink(
              "methodB(a, b)",
              "Method",
              "public System.String methodB(System.String a, System.String b)"
            ),
            CompletionItemLink("clone()", "Method", "public Dummy clone()"),
            CompletionItemLink("hashCode()", "Method", "public virtual System.Integer hashCode()"),
            CompletionItemLink("toString()", "Method", "public virtual System.String toString()"),
            CompletionItemLink(
              "equals(other)",
              "Method",
              "public virtual System.Boolean equals(Object other)"
            ),
            CompletionItemLink("myField", "Field", "public String myField")
          )
      )
    }
  }

  test("Empty Class Completions") {
    FileSystemHelper.run(Map("Dummy.cls" -> "")) { root: PathLike =>
      val org = createOrg(root)
      assert(
        org
          .getCompletionItemsInternal(root.join("Dummy.cls"), line = 1, offset = 0, "")
          .map(_.label)
          sameElements Array(
            "abstract",
            "static",
            "class",
            "testmethod",
            "transient",
            "enum",
            "final",
            "virtual",
            "global",
            "webservice",
            "inherited",
            "with",
            "without",
            "interface",
            "override",
            "private",
            "protected",
            "public"
          )
      )
    }
  }

  test("Class Declaration Completions") {
    FileSystemHelper.run(Map("Dummy.cls" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "class Dummy e"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label) sameElements Array("implements", "extends")
      )
    }
  }

  test("Class Declaration Completions (extends)") {
    FileSystemHelper.run(Map("Dummy.cls" -> "", "Foo.cls" -> "public class Foo { }")) {
      root: PathLike =>
        val org     = createOrg(root)
        val testSrc = "class Dummy extends F"
        assert(
          org
            .getCompletionItemsInternal(
              root.join("Dummy.cls"),
              line = 1,
              offset = testSrc.length,
              testSrc
            )
            .map(_.label) sameElements Array("Foo")
        )
    }
  }

  test("public constructor completion") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "",
        "Foo.cls" -> "public class Foo { public Foo(String s){} public Foo(){} private Foo(Integer i){}}"
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = s"class Dummy {{new Fo"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label) sameElements Array("Foo()", "Foo(s)")
      )
    }
  }

  test("private constructor completion") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "",
        "Foo.cls" -> "public class Foo { public Foo(String s){} public Foo(){} @TestVisible private Foo(Integer i){}}"
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "@isTest class Dummy {{new Fo"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label)
          .sorted sameElements Array("Foo()", "Foo(s)", "Foo(i)").sorted
      )
    }
  }

  test("protected super constructor completion") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "",
        "Foo.cls" -> "public class Foo { public Foo(String s){} public Foo(){} protected Foo(Integer i){}}"
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "class Dummy extends Foo { public Dummy(){sup"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .filter(_.label.contains("super"))
          .map(_.label)
          sameElements Array("super", "super()", "super(i)", "super(s)")
      )
    }
  }

  test("private super constructor completion") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> "",
        "Foo.cls" -> "public class Foo { public Foo(String s){} public Foo(){} private Foo(Integer i){}}"
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "class Dummy extends Foo { public Dummy(){sup"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .filter(_.label.contains("super"))
          .map(_.label)
          sameElements Array("super", "super()", "super(s)")
      )
    }
  }

  test("this constructor completion") {
    FileSystemHelper.run(Map("Dummy.cls" -> "")) { root: PathLike =>
      val org = createOrg(root)
      val testSrc =
        "public class Dummy { public Dummy(String s){} private Dummy(Integer i){} protected Dummy(Boolean b){} public Dummy(){th"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .filter(_.label.contains("this"))
          .map(_.label)
          sameElements Array("this", "this()", "this(b)", "this(i)", "this(s)")
      )
    }
  }

  test("Primary Completions (variable type)") {
    FileSystemHelper.run(Map("Dummy.cls" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "class Dummy { {Boolean abc; abc."
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .toSet == Set(
          CompletionItemLink(
            "addError(msg, escape)",
            "Method",
            "public virtual void addError(System.String msg, System.Boolean escape)"
          ),
          CompletionItemLink(
            "addError(msg, escape)",
            "Method",
            "public virtual void addError(System.Exception msg, System.Boolean escape)"
          ),
          CompletionItemLink(
            "addError(msg)",
            "Method",
            "public virtual void addError(System.Exception msg)"
          ),
          CompletionItemLink(
            "addError(msg)",
            "Method",
            "public virtual void addError(System.String msg)"
          )
        )
      )
    }
  }

  test("Primary Completions (method type)") {
    FileSystemHelper.run(Map("Dummy.cls" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "class Dummy { public Boolean func() {func()."
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .toSet == Set(
          CompletionItemLink(
            "addError(msg, escape)",
            "Method",
            "public virtual void addError(System.String msg, System.Boolean escape)"
          ),
          CompletionItemLink(
            "addError(msg, escape)",
            "Method",
            "public virtual void addError(System.Exception msg, System.Boolean escape)"
          ),
          CompletionItemLink(
            "addError(msg)",
            "Method",
            "public virtual void addError(System.Exception msg)"
          ),
          CompletionItemLink(
            "addError(msg)",
            "Method",
            "public virtual void addError(System.String msg)"
          )
        )
      )
    }
  }

  test("Empty Trigger Declaration Completion") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = ""
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.trigger"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label) sameElements Array("trigger")
      )
    }
  }

  test("Trigger Declaration Completion") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "trigger Dummy "
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.trigger"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label) sameElements Array("on")
      )
    }
  }

  test("Trigger Declaration Completion (Trigger Cases)") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "trigger Dummy on Account( "
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.trigger"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label) sameElements Array("after", "before")
      )
    }
  }

  test("Trigger Declaration Completion (Trigger Cases Keywords)") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "trigger Dummy on Account(before "
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.trigger"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .map(_.label) sameElements Array("insert", "delete", "undelete", "update")
      )
    }
  }

  test("Triggers Primary Completions (variable type)") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "trigger Dummy on Account(before insert) { Boolean testVar = false; te"
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.trigger"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .toSet contains CompletionItemLink("testVar", "Variable", "System.Boolean")
      )
    }
  }

  test("Trigger Context Variable Completions") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "")) { root: PathLike =>
      val org     = createOrg(root)
      val testSrc = "trigger Dummy on Account(before insert){Trigger."
      assert(
        org
          .getCompletionItemsInternal(
            root.join("Dummy.trigger"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
          .toSet == Set(
          CompletionItemLink(
            "newMap",
            "Field",
            "public static System.Map<System.Id, System.SObject> newMap"
          ),
          CompletionItemLink("isAfter", "Field", "public static System.Boolean isAfter"),
          CompletionItemLink("New", "Field", "public static System.List<System.SObject> New"),
          CompletionItemLink("old", "Field", "public static System.List<System.SObject> old"),
          CompletionItemLink(
            "operationType",
            "Field",
            "public static System.TriggerOperation operationType"
          ),
          CompletionItemLink("isBefore", "Field", "public static System.Boolean isBefore"),
          CompletionItemLink("isExecuting", "Field", "public static System.Boolean isExecuting"),
          CompletionItemLink("isUndelete", "Field", "public static System.Boolean isUndelete"),
          CompletionItemLink("size", "Field", "public static System.Integer size"),
          CompletionItemLink("isDelete", "Field", "public static System.Boolean isDelete"),
          CompletionItemLink("isInsert", "Field", "public static System.Boolean isInsert"),
          CompletionItemLink("isUpdate", "Field", "public static System.Boolean isUpdate"),
          CompletionItemLink(
            "oldMap",
            "Field",
            "public static System.Map<System.Id, System.SObject> oldMap"
          )
        )
      )
    }
  }

  test("Triggers Static Completions") {
    FileSystemHelper.run(
      Map(
        "Dummy.cls" ->
          """public class Dummy {
          |public String methodA(){}
          |public String methodB(String a, String b){}
          |public static String methodStatic(){}
          |private String methodPrivate(){}
          |public String myField;
          |public static String myStaticField;
          |private String myPrivateField;
          |public class MyInner{};
          |private interface MyPrivateInner{};
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val org     = createOrg(root)
      val path    = root.join("Completion.trigger")
      val content = "trigger Completion on Account(before insert) { Dummy.m"
      assert(
        org
          .getCompletionItemsInternal(path, line = 1, offset = content.length, content)
          .toSet ==
          Set(
            CompletionItemLink(
              "methodStatic()",
              "Method",
              "public static System.String methodStatic()"
            ),
            CompletionItemLink("myStaticField", "Field", "public static String myStaticField"),
            CompletionItemLink("MyInner", "Class", "public")
          )
      )
    }
  }

  forAll(
    Set(
      ("String", false),
      ("Dummy", true),
      ("Dummy2", false),
      ("Inner", true),
      ("Inner2", false),
      ("Other", true),
      ("Other2", false),
      ("Other.Inner", true),
      ("Other2.Inner", false),
      ("Other.Inner2", false),
      ("Foo__c", true),
      ("Bar__c", false),
      ("List<String>", true),
      ("List<Dummy>", true),
      ("List<Dummy2>", false),
      ("List<Foo__c>", true),
      ("List<Bar__c>", false),
      ("Map<Id, String>", true),
      ("Map<Id, Dummy2>", false)
    )
  ) { typeNameAndResult =>
    test(s"Var Creator completion ${typeNameAndResult._1}") {
      FileSystemHelper.run(
        Map(
          "Other.cls"             -> "public class Other { class Inner {} }",
          "objects/Foo__c.object" -> customObject("Foo", Seq())
        )
      ) { root: PathLike =>
        val org     = createHappyOrg(root)
        val testSrc = s"class Dummy { class Inner {} { ${typeNameAndResult._1} a = n"
        val completions = org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
        if (typeNameAndResult._2)
          assert(completions.map(_.label) sameElements Array(s"new ${typeNameAndResult._1}();"))
        else
          assert(completions.map(_.label) sameElements Array("super", "this", "new"))
      }
    }

    test(s"Field Creator completion ${typeNameAndResult._1}") {
      FileSystemHelper.run(
        Map(
          "Other.cls"             -> "public class Other { class Inner {} }",
          "objects/Foo__c.object" -> customObject("Foo", Seq())
        )
      ) { root: PathLike =>
        val org     = createHappyOrg(root)
        val testSrc = s"class Dummy { class Inner {} public ${typeNameAndResult._1}  a = n"
        val completions = org
          .getCompletionItemsInternal(
            root.join("Dummy.cls"),
            line = 1,
            offset = testSrc.length,
            testSrc
          )
        if (typeNameAndResult._2)
          assert(completions.map(_.label) sameElements Array(s"new ${typeNameAndResult._1}();"))
        else
          assert(completions.map(_.label) sameElements Array("super", "this", "new"))
      }
    }
  }

}
