/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper.{CURSOR, locToString}
import com.nawforce.apexlink.rpc.HoverItem
import com.nawforce.apexlink.TestHelper
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class HoverProviderTest extends AnyFunSuite with TestHelper {
  test("Hover for method") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Hover.cls")
      val content =
        """public class Dummy { public void someMethod() {methodB(1, 2);}
          |public String methodB(Integer a, Integer b){}
          |}""".stripMargin.replaceAll("\r\n", "\n")
      val offset    = content.split('\n').head.length - 10
      val hoverItem = org.unmanaged.getHover(path, line = 1, offset, Some(content))
      assert(
        hoverItem.content.get ==
          "```apex\npublic System.String methodB(System.Integer a, System.Integer b)\n```"
      )
      assert(hoverItem.kind.contains("markdown"))
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 47)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 54)
    }
  }

  test("Hover for inner class") {
    val contentAndCursorPos =
      withCursor(
        s"public virtual class Foo {public void after(){new Du${CURSOR}mmy();}  " +
          s"private class Dummy {} }"
      )
    FileSystemHelper.run(Map("Foo.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(hoverItem.content.get == "```apex\nprivate class Dummy\n```")
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 57)
    }
  }

  test("Hover for external class") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){Du${CURSOR}mmy.dummyMethod();} }")
    val dummy    = "public class Dummy implements DummyTwo {public static void dummyMethod(){} }"
    val dummyTwo = "public interface DummyTwo {}"
    FileSystemHelper.run(
      Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy, "DummyTwo.cls" -> dummyTwo)
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(hoverItem.content.get == "```apex\npublic class Dummy implements DummyTwo\n```")
      assert(hoverItem.kind.contains("markdown"))
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for class implementing multiple interfaces") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){Du${CURSOR}mmy.dummyMethod();} }")
    val dummy =
      "public class Dummy implements DummyTwo, DummyThree, DummyFour {public static void dummyMethod(){} }"
    val dummyTwo   = "public interface DummyTwo {}"
    val dummyThree = "public interface DummyThree {}"
    val dummyFour  = "public interface DummyFour {}"
    FileSystemHelper.run(
      Map(
        "Foo.cls"        -> contentAndCursorPos._1,
        "Dummy.cls"      -> dummy,
        "DummyTwo.cls"   -> dummyTwo,
        "DummyThree.cls" -> dummyThree,
        "DummyFour.cls"  -> dummyFour
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(
        hoverItem.content.get ==
          "```apex\npublic class Dummy implements DummyTwo, DummyThree, DummyFour\n```"
      )
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for class implementing interfaces and extending classes") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){Du${CURSOR}mmy.dummyMethod();} }")
    val dummy =
      "public class Dummy extends DummyTwo implements DummyThree, DummyFour {public static void dummyMethod(){} }"
    val dummyTwo   = "public abstract class DummyTwo {}"
    val dummyThree = "public interface DummyThree {}"
    val dummyFour  = "public interface DummyFour {}"
    FileSystemHelper.run(
      Map(
        "Foo.cls"        -> contentAndCursorPos._1,
        "Dummy.cls"      -> dummy,
        "DummyTwo.cls"   -> dummyTwo,
        "DummyThree.cls" -> dummyThree,
        "DummyFour.cls"  -> dummyFour
      )
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(
        hoverItem.content.get ==
          "```apex\npublic class Dummy extends DummyTwo implements DummyThree, DummyFour\n```"
      )
      assert(hoverItem.location.get.startLine == 1)
      assert(hoverItem.location.get.startPosition == 46)
      assert(hoverItem.location.get.endLine == 1)
      assert(hoverItem.location.get.endPosition == 51)
    }
  }

  test("Hover for constructor") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public void after(){new Du${CURSOR}mmy(1);} }")
    val dummy = "public class Dummy{public Dummy(Integer a){} }"
    FileSystemHelper.run(Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy)) {
      root: PathLike =>
        val org = createHappyOrg(root)
        val hoverItem =
          org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
        assert(hoverItem.content.get == "```apex\npublic Dummy(System.Integer a)\n```")
        assert(hoverItem.kind.contains("markdown"))
        assert(hoverItem.location.get.startLine == 1)
        assert(hoverItem.location.get.startPosition == 46)
        assert(hoverItem.location.get.endLine == 1)
        assert(hoverItem.location.get.endPosition == 58)
    }
  }

  test("Hover for unsupported keyword") {
    val contentAndCursorPos =
      withCursor(s"public virtual class Foo {public vo${CURSOR}id after(){Dummy.dummyMethod();} }")
    val dummy    = "public class Dummy implements DummyTwo {public static void dummyMethod(){} }"
    val dummyTwo = "public interface DummyTwo {}"
    FileSystemHelper.run(
      Map("Foo.cls" -> contentAndCursorPos._1, "Dummy.cls" -> dummy, "DummyTwo.cls" -> dummyTwo)
    ) { root: PathLike =>
      val org = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join("Foo.cls"), line = 1, contentAndCursorPos._2, None)
      assert(hoverItem.content.isEmpty)
      assert(hoverItem.location.isEmpty)
      assert(hoverItem.kind.isEmpty)
    }
  }

  private def hoverAt(files: Map[String, String], file: String, cursor: CursorPos)(
    assertion: HoverItem => Unit
  ): Unit = {
    FileSystemHelper.run(files) { root: PathLike =>
      val org = createHappyOrg(root)
      assertion(org.unmanaged.getHover(root.join(file), cursor.line, cursor.offset, None))
    }
  }

  private def assertHover(
    hoverItem: HoverItem,
    header: String,
    source: String,
    identifier: String
  ): Unit = {
    assert(hoverItem.content.contains(s"```apex\n$header\n```"))
    assert(hoverItem.kind.contains("markdown"))
    assert(locToString(source, hoverItem.location.get) == identifier)
  }

  private def assertNoHover(hoverItem: HoverItem): Unit = {
    assert(hoverItem.content.isEmpty)
    assert(hoverItem.location.isEmpty)
    assert(hoverItem.kind.isEmpty)
  }

  test("Hover for field reference") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  private String name;
         |  public void method() { na${CURSOR}me = 'a'; }
         |}""".stripMargin)
    hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "private String name", content, "name")
    }
  }

  test("Hover for static field reference on external class") {
    val (content, cursor) = withCursorMultiLine(
      s"public class Foo { public void method() { Integer i = Dummy.co${CURSOR}unt; } }"
    )
    val dummy = "public class Dummy { public static final Integer count = 1; }"
    hoverAt(Map("Foo.cls" -> content, "Dummy.cls" -> dummy), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public static final Integer count", content, "Dummy.count")
    }
  }

  test("Hover for property reference") {
    val (content, cursor) = withCursorMultiLine(
      s"public class Foo { public void method() { new Dummy().La${CURSOR}bel = 'a'; } }"
    )
    val dummy = "public class Dummy { public String Label { get; set; } }"
    hoverAt(Map("Foo.cls" -> content, "Dummy.cls" -> dummy), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public String Label", content, "new Dummy().Label")
    }
  }

  test("Hover for enum constant reference") {
    val (content, cursor) = withCursorMultiLine(
      s"public class Foo { public void method() { Colour c = Colour.R${CURSOR}ED; } }"
    )
    val colour = "public enum Colour { RED, GREEN }"
    hoverAt(Map("Foo.cls" -> content, "Colour.cls" -> colour), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public static final Colour RED", content, "Colour.RED")
    }
  }

  test("Hover for unknown field reference") {
    val (content, cursor) = withCursorMultiLine(
      s"public class Foo { public void method() { Integer i = Dummy.mis${CURSOR}sing; } }"
    )
    val dummy = "public class Dummy { public static Integer count; }"
    FileSystemHelper.run(Map("Foo.cls" -> content, "Dummy.cls" -> dummy)) { root: PathLike =>
      val org = createOrg(root)
      assertNoHover(org.unmanaged.getHover(root.join("Foo.cls"), cursor.line, cursor.offset, None))
    }
  }

  test("Hover for method reference from trigger") {
    val (content, cursor) =
      withCursorMultiLine(s"trigger Dummy on Account (before insert) { Foo.met${CURSOR}hod(1); }")
    val foo = "public class Foo { public static void method(Integer a) {} }"
    hoverAt(Map("Dummy.trigger" -> content, "Foo.cls" -> foo), "Dummy.trigger", cursor) {
      hoverItem =>
        assertHover(hoverItem, "public static void method(System.Integer a)", content, "method")
    }
  }

  test("Hover for trigger declaration") {
    val (content, cursor) =
      withCursorMultiLine(s"trigger Du${CURSOR}mmy on Account (before insert, after update) { }")
    hoverAt(Map("Dummy.trigger" -> content), "Dummy.trigger", cursor) { hoverItem =>
      assertHover(
        hoverItem,
        "trigger Dummy on Account (before insert, after update)",
        content,
        "Dummy"
      )
    }
  }

  test("Hover for trigger keyword and object name") {
    Seq(
      s"trig${CURSOR}ger Dummy on Account (before insert) { }",
      s"trigger Dummy o${CURSOR}n Account (before insert) { }",
      s"trigger Dummy on Acc${CURSOR}ount (before insert) { }",
      s"trigger Dummy on Account (bef${CURSOR}ore insert) { }"
    ).foreach(source => {
      val (content, cursor) = withCursorMultiLine(source)
      hoverAt(Map("Dummy.trigger" -> content), "Dummy.trigger", cursor)(assertNoHover)
    })
  }

  test("Hover for class declaration") {
    val (content, cursor) =
      withCursorMultiLine(s"public virtual class F${CURSOR}oo extends Bar implements Baz { }")
    val files = Map(
      "Foo.cls" -> content,
      "Bar.cls" -> "public virtual class Bar {}",
      "Baz.cls" -> "public interface Baz {}"
    )
    hoverAt(files, "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public virtual class Foo extends Bar implements Baz", content, "Foo")
    }
  }

  test("Hover for inner class declaration") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  private class In${CURSOR}ner {}
         |}""".stripMargin)
    hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "private class Inner", content, "Inner")
    }
  }

  test("Hover for method declaration") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  public String meth${CURSOR}odB(Integer a, Integer b) { return null; }
         |}""".stripMargin)
    hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor) { hoverItem =>
      assertHover(
        hoverItem,
        "public System.String methodB(System.Integer a, System.Integer b)",
        content,
        "methodB"
      )
    }
  }

  test("Hover for constructor declaration") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  public F${CURSOR}oo(Integer a) {}
         |}""".stripMargin)
    hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public Foo(System.Integer a)", content, "Foo")
    }
  }

  test("Hover for field declaration") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  private static final String na${CURSOR}me = 'a';
         |}""".stripMargin)
    hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "private static final String name", content, "name")
    }
  }

  test("Hover for property declaration") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  public String La${CURSOR}bel { get; set; }
         |}""".stripMargin)
    hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public String Label", content, "Label")
    }
  }

  test("Hover for enum constant declaration") {
    val (content, cursor) = withCursorMultiLine(s"public enum Colour { RED, GR${CURSOR}EEN }")
    hoverAt(Map("Colour.cls" -> content), "Colour.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public static final Colour GREEN", content, "GREEN")
    }
  }

  test("Hover for enum declaration") {
    val (content, cursor) = withCursorMultiLine(s"public enum Col${CURSOR}our { RED, GREEN }")
    hoverAt(Map("Colour.cls" -> content), "Colour.cls", cursor) { hoverItem =>
      assertHover(hoverItem, "public enum Colour", content, "Colour")
    }
  }

  test("Hover for declaration site matches reference") {
    val (content, cursor) = withCursorMultiLine(s"""public class Foo {
         |  public String Label { get; set; }
         |  public void method() { La${CURSOR}bel = 'a'; }
         |}""".stripMargin)
    FileSystemHelper.run(Map("Foo.cls" -> content)) { root: PathLike =>
      val org       = createHappyOrg(root)
      val path      = root.join("Foo.cls")
      val reference = org.unmanaged.getHover(path, cursor.line, cursor.offset, None)
      val declaration =
        org.unmanaged.getHover(path, line = 2, content.split('\n')(1).indexOf("Label") + 2, None)
      assert(reference.content.nonEmpty)
      assert(reference.content == declaration.content)
      assert(reference.kind == declaration.kind)
    }
  }

  test("Hover for declaration modifiers and types") {
    Seq(
      s"public class Foo { pri${CURSOR}vate String name; }",
      s"public class Foo { private Str${CURSOR}ing name; }",
      s"public class Foo { private String name; pub${CURSOR}lic void method() {} }",
      s"public class Foo { private String name; public vo${CURSOR}id method() {} }",
      s"public class Foo { private String name; public void method(Integer a${CURSOR}) {} }",
      s"public class Foo { private String name; public void method() {} ${CURSOR} }"
    ).foreach(source => {
      val (content, cursor) = withCursorMultiLine(source)
      hoverAt(Map("Foo.cls" -> content), "Foo.cls", cursor)(assertNoHover)
    })
  }
}
