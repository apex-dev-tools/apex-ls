/*
 * Copyright (c) 2026 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.types.apex.{FullDeclaration, TriggerDeclaration}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class DocCommentTest extends AnyFunSuite with TestHelper {

  /* Workspace loads use the outline parser, a refresh re-parses the class with ANTLR. */
  private def antlrClass(content: String)(op: FullDeclaration => Unit): Unit = {
    withManualFlush {
      FileSystemHelper.run(Map("Dummy.cls" -> content)) { root: PathLike =>
        val org  = createOrg(root)
        val path = root.join("Dummy.cls")
        org.unmanaged.refresh(path, highPriority = false)
        org.flush()
        op(unmanagedClass("Dummy").get.asInstanceOf[FullDeclaration])
      }
    }
  }

  private def docText(declaration: DocumentedDeclaration): Option[String] =
    declaration.docComment.map(_.asString)

  private def member(td: FullDeclaration, name: String): ClassBodyDeclaration =
    td.bodyDeclarations.find(_.name == Name(name)).get

  test("Doc comment attaches to class, members and inner types") {
    antlrClass("""/** Class doc */
        |public class Dummy {
        |  /** Field doc */
        |  private String field;
        |  /** Property doc */
        |  public Integer prop { get; set; }
        |  /** Constructor doc */
        |  public Dummy() {}
        |  /** Method doc */
        |  public void method() {}
        |  /** Inner doc */
        |  public class Inner {}
        |  /** Interface doc */
        |  public interface Iface { /** Iface method doc */ void run(); }
        |  /** Enum doc */
        |  public enum Colour { /** Red doc */ RED, GREEN }
        |}""".stripMargin) { td =>
      assert(docText(td).contains("/** Class doc */"))
      assert(docText(member(td, "field")).contains("/** Field doc */"))
      assert(docText(member(td, "prop")).contains("/** Property doc */"))
      assert(docText(member(td, "Dummy")).contains("/** Constructor doc */"))
      assert(docText(member(td, "method")).contains("/** Method doc */"))
      val inner = member(td, "Inner").asInstanceOf[FullDeclaration]
      assert(docText(inner).contains("/** Inner doc */"))
      val iface = member(td, "Iface").asInstanceOf[FullDeclaration]
      assert(docText(iface).contains("/** Interface doc */"))
      assert(docText(member(iface, "run")).contains("/** Iface method doc */"))
      val colour = member(td, "Colour").asInstanceOf[FullDeclaration]
      assert(docText(colour).contains("/** Enum doc */"))
      assert(docText(member(colour, "RED")).contains("/** Red doc */"))
      assert(member(colour, "GREEN").docComment.isEmpty)
    }
  }

  test("Doc comment spans annotations and modifiers") {
    antlrClass("""/**
        | * Class doc
        | */
        |@IsTest
        |public class Dummy {
        |  /** Field doc */
        |  @TestVisible
        |  private static String field;
        |  /** Method doc */
        |  @AuraEnabled
        |  public static void method() {}
        |  /** Property doc */
        |  @AuraEnabled public Integer prop { get; set; }
        |}""".stripMargin) { td =>
      assert(docText(td).contains("/**\n * Class doc\n */"))
      assert(docText(member(td, "field")).contains("/** Field doc */"))
      assert(docText(member(td, "method")).contains("/** Method doc */"))
      assert(docText(member(td, "prop")).contains("/** Property doc */"))
    }
  }

  test("Undocumented declarations hold no doc comment") {
    antlrClass("""public class Dummy {
        |  private String field;
        |  public void method() {}
        |}""".stripMargin) { td =>
      assert(td.docComment.isEmpty)
      assert(member(td, "field").docComment.isEmpty)
      assert(member(td, "method").docComment.isEmpty)
    }
  }

  test("Ordinary comment between doc comment and declaration prevents attachment") {
    antlrClass("""public class Dummy {
        |  /** Method doc */
        |  // Not a doc comment
        |  public void method() {}
        |  /** Other doc */
        |  /* Not a doc comment */
        |  public void other() {}
        |  /** Third doc */
        |  // Not a doc comment
        |  @AuraEnabled
        |  public static void third() {}
        |}""".stripMargin) { td =>
      assert(member(td, "method").docComment.isEmpty)
      assert(member(td, "other").docComment.isEmpty)
      assert(member(td, "third").docComment.isEmpty)
    }
  }

  test("Separating declaration prevents attachment") {
    antlrClass("""public class Dummy {
        |  /** Field doc */
        |  private String field;
        |  public void method() {}
        |}""".stripMargin) { td =>
      assert(docText(member(td, "field")).contains("/** Field doc */"))
      assert(member(td, "method").docComment.isEmpty)
    }
  }

  test("Nearest of consecutive doc comments attaches") {
    antlrClass("""public class Dummy {
        |  /** Orphan doc */
        |  /** Method doc */
        |  public void method() {}
        |}""".stripMargin) { td =>
      assert(docText(member(td, "method")).contains("/** Method doc */"))
    }
  }

  test("Doc comment shares source bytes without copying") {
    antlrClass("""public class Dummy {
        |  /** Método doc */
        |  public void method() {}
        |}""".stripMargin) { td =>
      val doc = member(td, "method").docComment.get
      assert(doc.source eq td.source.code.source)
      assert(doc.asString == "/** Método doc */")
    }
  }

  test("Doc comment attaches to trigger") {
    FileSystemHelper.run(Map("Dummy.trigger" -> """/** Trigger doc */
          |trigger Dummy on Account (before insert) {}""".stripMargin)) { root: PathLike =>
      createOrg(root)
      val td = unmanagedType(TypeName(Name("__sfdc_trigger/Dummy"))).get
        .asInstanceOf[TriggerDeclaration]
      assert(docText(td).contains("/** Trigger doc */"))
    }
  }

  test("Text strips delimiters and leading asterisks") {
    assert(DocComment.text("/** Single line */").contains("Single line"))
    assert(
      DocComment
        .text("/**\n   * First line\n   *\n   *   indented second\n   */")
        .contains("First line\n\n  indented second")
    )
    assert(DocComment.text("/**\r\n * Windows\r\n */").contains("Windows"))
    assert(DocComment.text("/** @description Tagged */").contains("@description Tagged"))
  }

  test("Text is absent for banners and empty comments") {
    assert(DocComment.text("/** */").isEmpty)
    assert(DocComment.text("/*****/").isEmpty)
    assert(DocComment.text("/**********\n **********\n **********/").isEmpty)
    assert(DocComment.text("/**\n *\n *\n */").isEmpty)
  }
}
