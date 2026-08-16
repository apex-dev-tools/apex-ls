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
package com.nawforce.pkgforce.xml

import com.nawforce.pkgforce.path.Location
import org.scalatest.funsuite.AnyFunSuite

class XMLSourceRangeTest extends AnyFunSuite {
  test("indexes declarations, DOCTYPE internal subsets, and ignored markup") {
    val source =
      " \n<?xml version='1.0'?><!DOCTYPE root [<!-- ] > --><!ENTITY sample 'a > b'>]><!--c--><root attr=\"&sample;\"><![CDATA[</root>]]><?pi x?><child>&sample;</child></root>"
    val root        = XMLSourceRange.index(source).get
    val contentLine = source.split("\n")(1)
    assert(root.qualifiedName == "root")
    assert(root.location == Location(2, contentLine.indexOf("<root"), 2, contentLine.length))
    assert(root.children.map(_.qualifiedName) == Seq("child"))
    assert(
      root.children.head.location == Location(
        2,
        contentLine.indexOf("<child>"),
        2,
        contentLine.indexOf("</child>") + "</child>".length
      )
    )
  }

  test("fails closed for malformed and incomplete source") {
    Seq(
      "<root>",
      "<root><child></root>",
      "<root/><extra/>",
      "<root attr='unterminated>",
      "<root><!-- incomplete</root>",
      "<root><![CDATA[incomplete</root>",
      "<!DOCTYPE root [><root/>",
      "text<root/>"
    ).foreach(source => assert(XMLSourceRange.index(source).isEmpty, source))
  }
}
