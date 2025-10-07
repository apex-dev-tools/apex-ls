package com.nawforce.apexlink.hover

import com.nawforce.pkgforce.path.Location
import org.scalatest.funsuite.AnyFunSuite

class ApexDocFormatterTest extends AnyFunSuite {

  test("ApexDoc formatter renders deterministic Markdown across platforms") {
    val source =
      """public class Example {
         |  /**
         |   * Processes values.
         |   * Supports multi-line summaries.
         |   *
         |   * @param value Input payload.
         |   * @throws ExampleException When processing fails.
         |   * @see https://example.com/docs Example Docs
         |   */
         |  public void run(String value) {}
         |}
         |""".stripMargin

    val location = Location(9, 9, 9, 28)

    val markdown = ApexDocFormatter.format(source, location)

    val expected =
      """**Summary**
         |Processes values. Supports multi-line summaries.
         |**Parameters**
         |- `value` — Input payload.
         |**Throws**
         |- ExampleException — When processing fails.
         |**See Also**
         |[Example Docs](https://example.com/docs)
         |""".stripMargin.trim

    assert(markdown.contains(expected))
    assert(ApexDocFormatter.format(source, location).contains(expected))
  }
}
