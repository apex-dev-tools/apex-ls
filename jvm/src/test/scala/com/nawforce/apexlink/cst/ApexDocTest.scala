/*
 * Copyright (c) 2026 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.cst

import org.scalatest.funsuite.AnyFunSuite

class ApexDocTest extends AnyFunSuite {

  private def md(raw: String): String = ApexDoc.markdown(raw).get

  test("Text strips delimiters and leading asterisks") {
    assert(ApexDoc.text("/** Single line */").contains("Single line"))
    assert(
      ApexDoc
        .text("/**\n   * First line\n   *\n   *   indented second\n   */")
        .contains("First line\n\n  indented second")
    )
    assert(ApexDoc.text("/**\r\n * Windows\r\n */").contains("Windows"))
    assert(ApexDoc.text("/** @description Tagged */").contains("@description Tagged"))
  }

  test("Text is absent for banners and empty comments") {
    assert(ApexDoc.text("/** */").isEmpty)
    assert(ApexDoc.text("/*****/").isEmpty)
    assert(ApexDoc.text("/**********\n **********\n **********/").isEmpty)
    assert(ApexDoc.text("/**\n *\n *\n */").isEmpty)
    assert(ApexDoc.markdown("/**********\n * ======== \n **********/").isEmpty)
  }

  test("Text strips decorative banners and alternate terminators") {
    assert(
      ApexDoc
        .text("/*************\n * Banner doc *\n * ----------- *\n *************/")
        .contains("Banner doc")
    )
    assert(ApexDoc.text("/** Short **/").contains("Short"))
    assert(ApexDoc.text("/**\n No asterisk\n   indented\n */").contains("No asterisk\nindented"))
  }

  test("Spec-style comment renders description and tag sections") {
    assert(
      md("""/**
           | * Adds two numbers.
           | *
           | * More detail.
           | * @param a the first
           | * @param b the second
           | * @return the sum
           | * @throws MathException when it overflows
           | * @see Other#method
           | * @author Someone
           | */""".stripMargin) ==
        """Adds two numbers.
          |
          |More detail.
          |
          |**Parameters**
          |- `a` — the first
          |- `b` — the second
          |
          |**Returns** — the sum
          |
          |**Throws**
          |- `MathException` — when it overflows
          |
          |**See**
          |- Other#method
          |
          |**@author** Someone""".stripMargin
    )
  }

  test("Tag-only comment uses @description as the main description") {
    assert(
      md("""/**
           | * @author Salesforce.org
           | * @date 2015
           | * @group Opportunity
           | * @description Handles opportunities
           | *   across multiple lines.
           | */""".stripMargin) ==
        """Handles opportunities
          |across multiple lines.
          |
          |**@author** Salesforce.org
          |
          |**@date** 2015
          |
          |**@group** Opportunity""".stripMargin
    )
  }

  test("Untagged description and @description are combined") {
    assert(md("/**\n * Summary.\n * @description Detail.\n */") == "Summary.\n\nDetail.")
  }

  test("Aliased tags are rendered as their spec equivalents") {
    assert(
      md("""/**
           | * @params a - first
           | * @PARAM b: second
           | * @returns the sum
           | * @exception FooException – on failure
           | */""".stripMargin) ==
        """**Parameters**
          |- `a` — first
          |- `b` — second
          |
          |**Returns** — the sum
          |
          |**Throws**
          |- `FooException` — on failure""".stripMargin
    )
  }

  test("Tags with continuation lines are joined") {
    assert(
      md("/**\n * @param a the first\n *        value\n * @return\n *   the sum\n */") ==
        "**Parameters**\n- `a` — the first value\n\n**Returns** — the sum"
    )
  }

  test("Unknown and misspelled tags remain visible") {
    assert(
      md("/**\n * @desctiption Typo\n * @nodoc\n * @group-content ../x.htm\n */") ==
        "**@desctiption** Typo\n\n**@nodoc**\n\n**@group-content** ../x.htm"
    )
    assert(md("/** @param */") == "**@param**")
  }

  test("Inline tags render as code or text") {
    assert(
      md("/** Use {@code List<String>} or {@link Foo#bar} and {@literal <b>} {@hidden x} */") ==
        "Use `List<String>` or Foo\\#bar and &lt;b> x"
    )
    assert(md("/** {@code a`b} {@inheritDoc} {@code} */") == "`` a`b `` {@inheritDoc}")
    assert(md("/** {@code Map<String, {x}>} */") == "`Map<String, {x}>`")
    assert(md("/** Unclosed {@code x */") == "Unclosed {@code x")
  }

  test("Markdown-significant content cannot escape its block") {
    assert(md("/** # Heading\n * <!-- hidden */") == "\\# Heading\n&lt;!-- hidden")
    assert(md("/** Keep `a<b` code */") == "Keep `a<b` code")
    assert(md("/** Unbalanced ` tick */") == "Unbalanced \\` tick")
    assert(
      md("/**\n * Example:\n * ```\n * @AuraEnabled\n * ```\n * @return y\n */") ==
        "Example:\n```\n@AuraEnabled\n```\n\n**Returns** — y"
    )
    assert(
      md("/**\n * Unclosed:\n * ```\n * @return y\n */") ==
        "Unclosed:\n```\n@return y\n```"
    )
    assert(md("/**\n * ~~~~\n * open\n * @author a\n */") == "~~~~\nopen\n@author a\n~~~~")
  }

  test("Multi-line examples render as code") {
    assert(
      md("/**\n * @example\n * Foo f = new Foo();\n * f.run();\n */") ==
        "**@example**\n```apex\nFoo f = new Foo();\nf.run();\n```"
    )
    assert(md("/** @example Foo.run(); */") == "**@example** Foo.run();")
  }

  test("Long descriptions are capped but tag sections retained") {
    val lines = (1 to 30).map(i => s" * Line $i").mkString("\n")
    val out   = md(s"/**\n$lines\n * @param a first\n * @return sum\n */")
    assert(
      out == (1 to ApexDoc.MaxDescriptionLines).map(i => s"Line $i").mkString("\n") +
        "\n…\n\n**Parameters**\n- `a` — first\n\n**Returns** — sum"
    )

    val word = "word " * 1000
    val long = md(s"/** $word\n * @return sum */")
    assert(long.length < ApexDoc.MaxDescriptionChars + 50)
    assert(long.endsWith("…\n\n**Returns** — sum"))
  }

  test("Capped description closes an open fence") {
    val code = (1 to 20).map(i => s" * code $i").mkString("\n")
    val out  = md(s"/**\n * ```\n$code\n * ```\n * @return sum\n */")
    assert(out.startsWith("```\ncode 1\n"))
    assert(out.endsWith("code 9\n```\n…\n\n**Returns** — sum"))
  }

  test("Tag text and total output are capped") {
    val tagOut = md(s"/** @author ${"x " * 500} */")
    assert(tagOut.length < ApexDoc.MaxTagChars + 20)
    assert(tagOut.endsWith("…"))

    val params = (1 to 200).map(i => s" * @param p$i ${"text " * 20}").mkString("\n")
    val out    = md(s"/**\n$params\n */")
    assert(out.length <= ApexDoc.MaxLength + 10)
    assert(out.startsWith("**Parameters**\n- `p1` — text"))
    assert(out.endsWith("\n…"))
    assert(md(s"/**\n$params\n */") == out)
  }

  test("Malformed input does not fail") {
    Seq(
      "",
      "/**",
      "*/",
      "/**/",
      "/** @",
      "/** {@",
      "/** {@code",
      "/** ``` */",
      "/** @param - */",
      "/**\n@\n*/"
    ).foreach(raw => ApexDoc.render(ApexDoc.parse(raw)))
    assert(ApexDoc.render(ApexDoc.parse("")).isEmpty)
    assert(ApexDoc.render(ApexDoc.parse("/**/")).isEmpty)
    assert(ApexDoc.render(ApexDoc.parse("/** @")).contains("@"))
    assert(ApexDoc.render(ApexDoc.parse("/** {@code")).contains("{@code"))
    assert(ApexDoc.render(ApexDoc.parse("/** ``` */")).contains("```\n```"))
    assert(ApexDoc.render(ApexDoc.parse("/**\n@\n*/")).contains("@"))
    assert(md("/** @ */") == "@")
    assert(md("/** @param - */") == "**Parameters**\n- `-`")
  }

  test("Fence opened on a tag line is tracked") {
    assert(
      md(
        "/**\n * @example ```\n * @AuraEnabled\n * public void x(){}\n * ```\n * @return y\n */"
      ) ==
        "**Returns** — y\n\n**@example**\n```\n@AuraEnabled\npublic void x(){}\n```"
    )
    assert(
      md("/**\n * @description ```\n * @AuraEnabled\n * ```\n * @return y\n */") ==
        "```\n@AuraEnabled\n```\n\n**Returns** — y"
    )
  }

  test("Tabs and aligned continuation lines are dedented") {
    assert(md("/**\n *\tSummary.\n *\n *\tMore detail.\n */") == "Summary.\n\nMore detail.")
    assert(
      md(
        "/**\n * @description Summary\n *              with {@literal <b>}\n *\n *              More.\n */"
      ) ==
        "Summary\nwith &lt;b>\n\nMore."
    )
  }

  test("CRLF line endings render") {
    assert(
      md("/**\r\n * Summary.\r\n * @param a - first\r\n * @return sum\r\n */") ==
        "Summary.\n\n**Parameters**\n- `a` — first\n\n**Returns** — sum"
    )
  }

  test("Linkplain renders as text and em dash separates parameter description") {
    assert(md("/** See {@linkplain Foo#bar the bar} */") == "See Foo\\#bar the bar")
    assert(md("/** @param a — first */") == "**Parameters**\n- `a` — first")
  }

  test("Escaped backticks do not open code spans") {
    assert(md("/** \\`<img src=x onerror=alert(1)>` */") == "\\`&lt;img src=x onerror=alert(1)>\\`")
    assert(md("/** \\`<!--` a\n * b --> c */") == "\\`&lt;!--\\` a\nb --> c")
    assert(md("/** \\<b> */") == "&lt;b>")
  }

  test("Truncation never splits surrogate pairs") {
    val emoji = "\uD83D\uDE00" * 200
    val out   = md(s"/** @author a$emoji */")
    val text  = out.stripPrefix("**@author** ").stripSuffix(" …")
    assert(out.endsWith(" …"))
    assert(!Character.isHighSurrogate(text.last))
    assert(text.codePoints().allMatch(cp => !Character.isSurrogate(cp.toChar)))

    val desc = md(s"/**\n * ${"\uD83D\uDE00" * 600}\n */")
    assert(!Character.isHighSurrogate(desc.stripSuffix("\n…").last))
  }

  test("Over-long unbroken description is hard cut with an ellipsis") {
    val out = md(s"/**\n *   ${"x" * 2000}\n * @return r */")
    assert(out == "x" * ApexDoc.MaxDescriptionChars + "\n…\n\n**Returns** — r")
  }

  test("Long whitespace runs render quickly") {
    val line  = "a" + " " * 50000 + "b"
    val start = System.nanoTime()
    assert(md(s"/**\n * $line\n * $line *\n */").startsWith("a"))
    assert((System.nanoTime() - start) / 1000000 < 1000)
  }
}
