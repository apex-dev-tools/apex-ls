/*
 [The "BSD licence"]
 Copyright (c) 2017 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue, IssuesAnd}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.pkgforce.xml.{
  XMLDocumentLike,
  XMLElementLike,
  XMLException,
  XMLFactory,
  XMLName
}
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.xml.XMLDocument
import org.scalatest.funsuite.AnyFunSuite

class XMLDocumentTest extends AnyFunSuite {

  private val namespace = XMLDocumentLike.sfNamespace

  // Backward compatability helper
  def parse(path: PathLike): Either[Issue, XMLDocument] = {
    XMLFactory.parse(path) match {
      case IssuesAnd(errors, doc) if errors.nonEmpty => Left(errors.head)
      case IssuesAnd(_, doc)                         => Right(doc.get)
    }
  }

  def parseOptional(path: PathLike): IssuesAnd[Option[XMLDocument]] = {
    XMLFactory.parse(path)
  }

  test("Simple doc is parsed") {
    FileSystemHelper.run(Map[String, String]("test.xml" -> "<test/>")) { root: PathLike =>
      parse(root.join("test.xml")) match {
        case Left(err) => assert(false, err)
        case Right(_)  => ()
      }
    }
  }

  test("Bad doc has error") {
    FileSystemHelper.run(Map[String, String]("test.xml" -> "\n  <test>")) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(Issue(f, Diagnostic(ERROR_CATEGORY, Location(1, _, 1, _), _), _)) if f == file =>
          ()
        case Left(err) => assert(false, err)
        case Right(_)  => assert(false)
      }
    }
  }

  test("Empty doc has error") {
    FileSystemHelper.run(Map[String, String]("test.xml" -> "")) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(Issue(f, Diagnostic(ERROR_CATEGORY, Location(1, 0, 1, 0), _), _)) if f == file =>
          ()
        case Left(err) => assert(false, err)
        case Right(_)  => assert(false)
      }
    }
  }

  test("whitespace only doc has no error") {
    FileSystemHelper.run(Map[String, String]("test.xml" -> " \n\t \r\n  ")) { root: PathLike =>
      parseOptional(root.join("test.xml")) match {
        case IssuesAnd(errors, doc) =>
          assert(errors.isEmpty)
          assert(doc.isEmpty)
      }
    }
  }

  test("root node") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Hello</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.path == file)
          val node = doc.rootElement
          assert(node.line == 1)
          assert(node.name == XMLName(XMLDocument.sfNamespace, "test"))
          assert(node.text == "Hello")
      }
    }
  }

  test("element locations cover exact nested and repeated lexical elements") {
    val source =
      s"  \n<root xmlns=\"$namespace\" quoted='1 > 0'>\n  <item>one</item>\n  <item><item/></item>\n</root>\n"
    withDocument(source) { doc =>
      val root  = doc.rootElement
      val items = root.getChildren("item")
      assert(root.line == 2)
      assert(
        slice(source, root.location) == source
          .substring(source.indexOf("<root"), source.lastIndexOf("</root>") + 7)
      )
      assert(items.length == 2)
      assert(slice(source, items.head.location) == "<item>one</item>")
      assert(slice(source, items(1).location) == "<item><item/></item>")
      assert(slice(source, items(1).getChildren("item").head.location) == "<item/>")
    }
  }

  test("element locations handle XML lexical constructs") {
    val source =
      s"<?xml version='1.0'?>\n<!-- before --><root xmlns='$namespace' value='a > b'>&amp;<![CDATA[<not-an-element>]]><?inside ok?><child value=\"&quot;\"/></root>"
    withDocument(source) { doc =>
      assert(slice(source, doc.rootElement.location) == source.substring(source.indexOf("<root")))
      assert(
        slice(
          source,
          doc.rootElement.getChildren("child").head.location
        ) == "<child value=\"&quot;\"/>"
      )
    }
  }

  test("element locations handle DOCTYPE internal subsets") {
    val source =
      s" \r\n<!DOCTYPE m:root [<!-- ] > --><!ENTITY sample 'a > b'>]>\r\n<m:root xmlns:m='$namespace'><m:child>&amp;</m:child></m:root>"
    withDocument(source) { doc =>
      val root  = doc.rootElement
      val child = root.getChildren("child").head
      assert(root.line == 3)
      assert(slice(source, root.location) == source.substring(source.indexOf("<m:root")))
      assert(slice(source, child.location) == "<m:child>&amp;</m:child>")
    }
  }

  test("element locations handle default and qualified namespaces") {
    val source =
      s"<m:root xmlns:m='$namespace'><m:child/><m:child><m:leaf/></m:child></m:root>"
    withDocument(source) { doc =>
      val children = doc.rootElement.getChildren("child")
      assert(slice(source, doc.rootElement.location) == source)
      assert(
        children.map(child => slice(source, child.location)) == Seq(
          "<m:child/>",
          "<m:child><m:leaf/></m:child>"
        )
      )
      assert(slice(source, children(1).getChildren("leaf").head.location) == "<m:leaf/>")
    }
  }

  test("element locations use code-point columns") {
    val source = s"<root xmlns='$namespace'>é🤦e\u0301<child/></root>"
    withDocument(source) { doc =>
      val child          = doc.rootElement.getChildren("child").head
      val expectedColumn = source.codePointCount(0, source.indexOf("<child"))
      assert(child.location.startPosition == expectedColumn)
      assert(slice(source, child.location) == "<child/>")
    }
  }

  Seq("\n", "\r\n", "\r").foreach { newline =>
    test(s"element locations handle ${newlineName(newline)} line endings") {
      val source = s" $newline<root xmlns='$namespace'>$newline<child/>$newline</root>"
      withDocument(source) { doc =>
        val root  = doc.rootElement
        val child = root.getChildren("child").head
        assert(root.location == Location(2, 0, 4, 7))
        assert(child.location == Location(3, 0, 3, 8))
        assert(slice(source, root.location) == source.substring(source.indexOf("<root")))
        assert(slice(source, child.location) == "<child/>")
      }
    }
  }

  test("XMLElementLike location defaults to its line") {
    val externalElement = new XMLElementLike {
      override val line: Int                                      = 7
      override val name: XMLName                                  = XMLName(namespace, "external")
      override val text: String                                   = ""
      override def getChildren(name: String): Seq[XMLElementLike] = Seq.empty
    }
    assert(externalElement.location == Location(7))
  }

  test("leading whitespace before declaration is parsed") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" ->
          """
            |<?xml version="1.0" encoding="UTF-8"?>
            |<test xmlns="http://soap.sforce.com/2006/04/metadata">Hello</test>
            |""".stripMargin
      )
    ) { root: PathLike =>
      parse(root.join("test.xml")) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.rootElement.text == "Hello")
      }
    }
  }

  test("leading whitespace before root element is parsed") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" ->
          """
            |<test xmlns="http://soap.sforce.com/2006/04/metadata">Hello</test>
            |""".stripMargin
      )
    ) { root: PathLike =>
      parse(root.join("test.xml")) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.rootElement.text == "Hello")
      }
    }
  }

  test("unicode text after replacement token is parsed") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">{0}…</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.rootElement.text == "{0}…")
      }
    }
  }

  test("single child node") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar<a>Foo</a>Baz</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          val node = doc.rootElement.getOptionalSingleChild("a")
          assert(node.nonEmpty)
          assert(node.get.line == 1)
          assert(node.get.name == XMLName(XMLDocument.sfNamespace, "a"))
          assert(node.get.text == "Foo")
          node.get.checkIsOrThrow("a")
      }
    }
  }

  test("dual child node not matched") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar<a>Foo</a><a>Baz</a></test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          val node = doc.rootElement.getOptionalSingleChild("a")
          assert(node.isEmpty)
      }
    }
  }

  test("no child node not matched") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          val node = doc.rootElement.getOptionalSingleChild("a")
          assert(node.isEmpty)
      }
    }
  }

  test("optional single child as string") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar<a>Foo</a>Baz</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.rootElement.getOptionalSingleChildAsString("a").contains("Foo"))
      }
    }
  }

  test("optional single child as boolean") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar<a>true</a>Baz</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.rootElement.getOptionalSingleChildAsBoolean("a").contains(true))
      }
    }
  }

  test("mandatory single child as string") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar<a>Foo</a>Baz</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(doc.rootElement.getSingleChildAsString("a") == "Foo")
      }
    }
  }

  test("mandatory single child as boolean") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar<a>false</a>Baz</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          assert(!doc.rootElement.getSingleChildAsBoolean("a"))
      }
    }
  }

  test("mandatory single child as string throws") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          try {
            doc.rootElement.getSingleChildAsString("a")
            assert(false)
          } catch {
            case ex: XMLException =>
              assert(ex.msg == "Expecting element 'test' to have a single 'a' child element")
              assert(ex.where == doc.rootElement.location)
            case _: Throwable => assert(false)
          }
      }
    }
  }

  test("mandatory single child as boolean throws") {
    FileSystemHelper.run(
      Map[String, String](
        "test.xml" -> "<test xmlns=\"http://soap.sforce.com/2006/04/metadata\">Bar</test>"
      )
    ) { root: PathLike =>
      val file = root.join("test.xml")
      parse(file) match {
        case Left(err) => assert(false, err)
        case Right(doc) =>
          try {
            doc.rootElement.getSingleChildAsBoolean("a")
            assert(false)
          } catch {
            case ex: XMLException =>
              assert(ex.msg == "Expecting element 'test' to have a single 'a' child element")
              assert(ex.where == doc.rootElement.location)
            case _: Throwable => assert(false)
          }
      }
    }
  }

  test("element validation errors select the smallest offending element") {
    val source =
      s"<wrong xmlns='$namespace'><enabled>sometimes</enabled></wrong>"
    withDocument(source) { doc =>
      val root    = doc.rootElement
      val enabled = root.getChildren("enabled").head

      val wrongName = intercept[XMLException](root.checkIsOrThrow("expected"))
      assert(wrongName.where == root.location)
      assert(Location.extract(source, wrongName.where) == source)

      val wrongBoolean = intercept[XMLException](root.getOptionalSingleChildAsBoolean("enabled"))
      assert(wrongBoolean.where == enabled.location)
      assert(Location.extract(source, wrongBoolean.where) == "<enabled>sometimes</enabled>")
    }
  }

  private def withDocument(source: String)(verify: XMLDocument => Unit): Unit = {
    FileSystemHelper.run(Map("test.xml" -> source)) { root: PathLike =>
      parse(root.join("test.xml")) match {
        case Left(error)     => fail(error.toString)
        case Right(document) => verify(document)
      }
    }
  }

  private def slice(source: String, location: Location): String = Location.extract(source, location)

  private def newlineName(newline: String): String = newline match {
    case "\n"   => "LF"
    case "\r\n" => "CRLF"
    case "\r"   => "CR"
  }
}
