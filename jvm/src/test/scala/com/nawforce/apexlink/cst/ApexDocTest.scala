/*
 * Copyright (c) 2026 Certinia Inc. All rights reserved
 */
package com.nawforce.apexlink.cst

import org.scalatest.funsuite.AnyFunSuite

class ApexDocTest extends AnyFunSuite {

  private def md(raw: String): String = ApexDoc.markdown(raw).get

  test("Delimiters and leading asterisks are stripped") {
    assert(md("/** Single line */") == "Single line")
    assert(
      md("/**\n   * First line\n   *\n   *   indented second\n   */") ==
        "First line\n\n  indented second"
    )
    assert(md("/**\r\n * Windows\r\n */") == "Windows")
    assert(md("/** @description Tagged */") == "Tagged")
  }

  test("Banners and empty comments render nothing") {
    assert(ApexDoc.markdown("/** */").isEmpty)
    assert(ApexDoc.markdown("/*****/").isEmpty)
    assert(ApexDoc.markdown("/**********\n **********\n **********/").isEmpty)
    assert(ApexDoc.markdown("/**\n *\n *\n */").isEmpty)
    assert(ApexDoc.markdown("/**********\n * ======== \n **********/").isEmpty)
  }

  test("Decorative banners and alternate terminators are stripped") {
    assert(md("/*************\n * Banner doc *\n * ----------- *\n *************/") == "Banner doc")
    assert(md("/** Short **/") == "Short")
    assert(md("/**\n No asterisk\n   indented\n */") == "No asterisk\nindented")
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

  test("Repeated return tags render as a list") {
    assert(md("/**\n * @return a\n * @returns b\n */") == "**Returns**\n- a\n- b")
    assert(md("/**\n * @return\n * @return b\n */") == "**Returns**\n- b")
    assert(md("/** @see */") == "**@see**")
  }

  test("Colon separates a parameter from its description") {
    assert(md("/** @param a : first */") == "**Parameters**\n- `a` — first")
  }

  test("Tag values and description lines cannot start block markup") {
    assert(md("/** @see # Heading */") == "**See**\n- \\# Heading")
    assert(
      md(
        "/**\n * @see > quote\n * @see - item\n * @see 1. one\n * @see ---\n * @see [x]: y\n */"
      ) ==
        "**See**\n- \\> quote\n- \\- item\n- 1\\. one\n- \\---\n- \\[x]: y"
    )
    assert(md("/** @see [Docs](https://x.com) */") == "**See**\n- [Docs](https://x.com)")
    assert(md("/**\n * Text\n *\n * [x]: http://x.com\n */") == "Text\n\n\\[x]: http://x.com")
    assert(md("/** ![img](https://x.com/a.png) */") == "\\![img](https://x.com/a.png)")
  }

  test("Output never exceeds the length limit when closing a fence") {
    val ticks = "`" * 900
    val body  = (1 to 50).map(i => s" * ${"x" * 60} $i").mkString("\n")
    val lead  = (1 to 20).map(i => s" * @author ${"a" * 60} $i").mkString("\n")
    val out   = md(s"/**\n$lead\n * @example\n * $ticks\n$body\n * $ticks\n */")
    assert(out.length <= ApexDoc.MaxLength)
    val params = (1 to 200).map(i => s" * @param p$i ${"text " * 20}").mkString("\n")
    assert(md(s"/**\n$params\n */").length <= ApexDoc.MaxLength)
  }

  test("CR-only and Unicode line separators split lines") {
    assert(
      md("/**\r * Summary.\r * @param a first\r */") == "Summary.\n\n**Parameters**\n- `a` — first"
    )
    assert(md("/**\u2028 * Summary.\u2028 * @return r\u2029 */") == "Summary.\n\n**Returns** — r")
  }

  test("Line separator input renders quickly") {
    val raw   = "/** @" + "a\u2028" * 10000 + " */"
    val start = System.nanoTime()
    ApexDoc.markdown(raw)
    ApexDoc.markdown("/** @param" + " \u2028" * 10000 + " */")
    assert((System.nanoTime() - start) / 1000000 < 5000)
  }

  test("Undecorated bold text keeps its emphasis") {
    assert(md("/**\n**Bold** text\n */") == "**Bold** text")
    assert(md("/**\n * **Bold** text\n */") == "**Bold** text")
    assert(md("/**\n *Emphasis* text\n */") == "*Emphasis* text")
  }
}
