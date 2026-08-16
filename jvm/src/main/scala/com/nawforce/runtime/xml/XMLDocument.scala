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
package com.nawforce.runtime.xml

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.pkgforce.xml.{
  XMLDocumentLike,
  XMLElementLike,
  XMLName,
  XMLSourceElement,
  XMLSourceRange
}
import com.nawforce.runtime.parsers.SourceData
import org.xml.sax.Locator

import java.io.ByteArrayInputStream
import javax.xml.parsers.SAXParserFactory
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

final class XMLElement private[xml] (element: Elem, sourceElement: Option[XMLSourceElement])
    extends XMLElementLike {
  def this(element: Elem) = this(element, None)

  override lazy val line: Int =
    sourceElement.fold(element.attribute("line").get.toString().toInt)(_.location.startLine)

  override lazy val location: Location = sourceElement.fold(Location(line))(_.location)

  override lazy val name: XMLName = XMLName(element.namespace, element.label)

  override lazy val text: String = element.text

  override def getChildren(name: String): Seq[XMLElementLike] = {
    val sourceChildren = sourceElement.map(_.children)
    element.child.collect { case child: Elem => child }.zipWithIndex.collect {
      case (child, index) if child.namespace == XMLDocument.sfNamespace && child.label == name =>
        new XMLElement(child, sourceChildren.map(_(index)))
    }
  }
}

final class XMLDocument private[xml] (
  path: PathLike,
  elem: Elem,
  sourceElement: Option[XMLSourceElement]
) extends XMLDocumentLike(path) {
  def this(path: PathLike, elem: Elem) = this(path, elem, None)

  override lazy val rootElement: XMLElementLike = new XMLElement(elem, sourceElement)
}

object XMLDocument {
  val sfNamespace = "http://soap.sforce.com/2006/04/metadata"

  def apply(path: PathLike, sourceData: SourceData): IssuesAnd[Option[XMLDocument]] = {
    val bytes = sourceData.asUTF8
    if (bytes.nonEmpty && bytes.forall(isXmlWhitespace))
      return IssuesAnd(None)

    try {
      val elem = XMLLineLoader.load(new ByteArrayInputStream(trimLeadingXmlWhitespace(bytes)))
      val sourceElement = XMLSourceRange
        .index(sourceData.asString)
        .filter(matchesTree(elem, _))
      IssuesAnd(Some(new XMLDocument(path, elem, sourceElement)))
    } catch {
      case e: SAXParseException =>
        IssuesAnd(
          ArraySeq(
            Issue(
              path,
              Diagnostic(
                ERROR_CATEGORY,
                Location(e.getLineNumber, e.getColumnNumber - 1),
                e.getLocalizedMessage
              )
            )
          ),
          None
        )
    }
  }

  private def trimLeadingXmlWhitespace(bytes: Array[Byte]): Array[Byte] = {
    val start = bytes.indexWhere(!isXmlWhitespace(_))
    if (start > 0)
      bytes.drop(start)
    else
      bytes
  }

  private def isXmlWhitespace(byte: Byte): Boolean = {
    byte == ' ' || byte == '\t' || byte == '\r' || byte == '\n'
  }

  private def matchesTree(element: Elem, sourceElement: XMLSourceElement): Boolean = {
    val qualifiedName = Option(element.prefix).fold(element.label)(_ + ":" + element.label)
    val children      = element.child.collect { case child: Elem => child }
    sourceElement.qualifiedName == qualifiedName &&
    children.length == sourceElement.children.length &&
    children.zip(sourceElement.children).forall { case (child, sourceChild) =>
      matchesTree(child, sourceChild)
    }
  }

}

trait WithLocation extends NoBindingFactoryAdapter {
  private var locator: org.xml.sax.Locator = _
  private val startLines                   = mutable.Stack[Int]()

  final override def setDocumentLocator(locator: Locator): Unit = {
    this.locator = locator
    super.setDocumentLocator(locator)
  }

  final override def createNode(
    pre: String,
    label: String,
    attrs: MetaData,
    scope: NamespaceBinding,
    children: List[Node]
  ): Elem = {
    val newAttrs = attrs.append(Attribute("line", Text(startLines.pop().toString), Null))
    super.createNode(pre, label, newAttrs, scope, children)
  }

  final override def startElement(
    uri: scala.Predef.String,
    _localName: scala.Predef.String,
    name: scala.Predef.String,
    attributes: org.xml.sax.Attributes
  ): scala.Unit = {
    startLines.push(locator.getLineNumber)
    super.startElement(uri, _localName, name, attributes)
  }
}

object XMLLineLoader extends factory.XMLLoader[Elem] {
  override def adapter = new parsing.NoBindingFactoryAdapter with WithLocation

  private lazy val cachedParser = {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.newSAXParser()
  }

  override def parser: SAXParser = {
    cachedParser.reset()
    cachedParser
  }
}
