/*
 * Copyright (c) 2025
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.apexlink.TestHelper
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class HoverProviderApexDocTest extends AnyFunSuite with TestHelper {

  private def runHover(
    files: Map[String, String],
    cursor: CursorPos,
    cursorPath: String
  )(assertions: (String, String) => Unit): Unit = {
    FileSystemHelper.run(files) { root =>
      val org       = createHappyOrg(root)
      val hoverItem =
        org.unmanaged.getHover(root.join(cursorPath), cursor.line, cursor.offset, None)
      val content   = hoverItem.content.getOrElse(fail("Expected hover content"))
      val signature = content.takeWhile(_ != '\n') match {
        case "" => content
        case sig => sig
      }
      assertions(content, signature)
    }
  }

  test("Hover includes ApexDoc summary, parameters, returns, and sanitized links") {
    val docExample =
      """public class DocExample {
         |  /**
         |   * Processes invoice by number and returns status.
         |   *
         |   * @param invoiceNumber Unique invoice code.
         |   * @return true when processed successfully.
         |   * @see https://example.com/billing Billing Guide
         |   */
         |  public Boolean process(String invoiceNumber) {
         |    return true;
         |  }
         |}
         |""".stripMargin

    val (usageContent, cursor) = withCursorMultiLine(
      s"""public class DocExampleUsage {
         |  public void run() {
         |    new DocExample().pro${CURSOR}cess('INV-001');
         |  }
         |}
         |""".stripMargin
    )

    runHover(
      Map("DocExample.cls" -> docExample, "DocExampleUsage.cls" -> usageContent),
      cursor,
      "DocExampleUsage.cls"
    ) { (content, signature) =>
      val expectedSignature = "public System.Boolean process(System.String invoiceNumber)"
      assert(signature == expectedSignature)
      assert(content.contains("**Summary**"))
      assert(content.contains("Processes invoice by number and returns status."))
      assert(content.contains("**Parameters**"))
      assert(content.contains("- `invoiceNumber` — Unique invoice code."))
      assert(content.contains("**Returns**"))
      assert(content.contains("true when processed successfully."))
      assert(content.contains("**See Also**"))
      assert(content.contains("[Billing Guide](https://example.com/billing)"))
    }
  }

  test("Hover renders valid sections while dropping malformed ApexDoc tags") {
    val docExample =
      """public class PartialDocExample {
         |  /**
         |   * Validates invoice input.
         |   * @param invoiceNumber
         |   * @return
         |   */
         |  public Boolean validate(String invoiceNumber) {
         |    return invoiceNumber != null;
         |  }
         |}
         |""".stripMargin

    val (usageContent, cursor) = withCursorMultiLine(
      s"""public class PartialDocUsage {
         |  public Boolean run(String code) {
         |    return new PartialDocExample().va${CURSOR}lidate(code);
         |  }
         |}
         |""".stripMargin
    )

    runHover(
      Map("PartialDocExample.cls" -> docExample, "PartialDocUsage.cls" -> usageContent),
      cursor,
      "PartialDocUsage.cls"
    ) { (content, signature) =>
      val expectedSignature = "public System.Boolean validate(System.String invoiceNumber)"
      assert(signature == expectedSignature)
      assert(content.contains("**Summary**"))
      assert(content.contains("Validates invoice input."))
      assert(!content.contains("**Parameters**"))
      assert(!content.contains("**Returns**"))
    }
  }

  test("Hover sanitizes inline markup and preserves external links") {
    val docExample =
      """public class SanitizedDocExample {
         |  /**
         |   * <script>alert('ignored');</script> Refer to [Docs](https://example.com/docs?filter=next).
         |   */
         |  public void sanitize() {}
         |}
         |""".stripMargin

    val (usageContent, cursor) = withCursorMultiLine(
      s"""public class SanitizedDocUsage {
         |  public void run() {
         |    new SanitizedDocExample().sani${CURSOR}tize();
         |  }
         |}
         |""".stripMargin
    )

    runHover(
      Map("SanitizedDocExample.cls" -> docExample, "SanitizedDocUsage.cls" -> usageContent),
      cursor,
      "SanitizedDocUsage.cls"
    ) { (content, signature) =>
      val expectedSignature = "public void sanitize()"
      assert(signature == expectedSignature)
      assert(content.contains("**Summary**"))
      assert(!content.contains("<script>"))
      assert(content.contains("[Docs](https://example.com/docs?filter=next)"))
    }
  }

  test("Hover ignores non-ApexDoc comments") {
    val docExample =
      """public class CommentOnlyExample {
         |  /* Not an ApexDoc comment */
         |  public Integer commentOnly() {
         |    return 1;
         |  }
         |}
         |""".stripMargin

    val (usageContent, cursor) = withCursorMultiLine(
      s"""public class CommentOnlyUsage {
         |  public Integer run() {
         |    return new CommentOnlyExample().comm${CURSOR}entOnly();
         |  }
         |}
         |""".stripMargin
    )

    runHover(
      Map("CommentOnlyExample.cls" -> docExample, "CommentOnlyUsage.cls" -> usageContent),
      cursor,
      "CommentOnlyUsage.cls"
    ) { (content, signature) =>
      val expectedSignature = "public System.Integer commentOnly()"
      assert(signature == expectedSignature)
      assert(!content.contains("**Summary**"))
      assert(!content.contains("**Parameters**"))
    }
  }
}
