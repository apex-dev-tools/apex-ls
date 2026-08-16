package io.github.apexdevtools.apexls

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

class CheckForIssuesTest extends AnyFunSuite {

  test("exit status is issues when errors are present") {
    assert(CheckForIssues.exitStatus(hasErrors = true, hasWarnings = false, hasUnused = false) == 4)
    assert(CheckForIssues.exitStatus(hasErrors = true, hasWarnings = true, hasUnused = true) == 4)
  }

  test("exit status is warning-only when warnings are present without errors or unused") {
    assert(CheckForIssues.exitStatus(hasErrors = false, hasWarnings = true, hasUnused = false) == 5)
  }

  test("exit status is unused-only when unused issues are present without errors or warnings") {
    assert(CheckForIssues.exitStatus(hasErrors = false, hasWarnings = false, hasUnused = true) == 6)
  }

  test("exit status is warnings-and-unused when both are present without errors") {
    assert(CheckForIssues.exitStatus(hasErrors = false, hasWarnings = true, hasUnused = true) == 7)
  }

  test("exit status is ok when no errors, warnings, or unused issues are present") {
    assert(
      CheckForIssues.exitStatus(hasErrors = false, hasWarnings = false, hasUnused = false) == 0
    )
  }

  test("JSON diagnostics preserve exact XML element ranges") {
    withInvalidFieldType { root =>
      val (status, output) = runAndCapture(root, "json")
      assert(status == 4)
      val message = ujson
        .read(output)("files")(0)("messages")
        .arr
        .find(_("message").str.contains("Unrecognised type"))
        .getOrElse(fail(output))
      assert(message("start") == ujson.Obj("line" -> 4, "offset" -> 4))
      assert(message("end") == ujson.Obj("line" -> 4, "offset" -> 22))
    }
  }

  test("PMD diagnostics preserve exact XML element ranges") {
    withInvalidFieldType { root =>
      val (status, output) = runAndCapture(root, "pmd")
      assert(status == 4)
      val violation = (scala.xml.XML.loadString(output) \\ "violation")
        .find(_.text.contains("Unrecognised type"))
        .getOrElse(fail(output))
      assert(violation \@ "beginline" == "4")
      assert(violation \@ "endline" == "4")
      assert(violation \@ "begincolumn" == "4")
      assert(violation \@ "endcolumn" == "22")
    }
  }

  private def withInvalidFieldType(verify: PathLike => Unit): Unit = {
    val source =
      """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
        |  <fields>
        |    <fullName>Name__c</fullName>
        |    <type>Silly</type>
        |  </fields>
        |</CustomObject>
        |""".stripMargin
    FileSystemHelper.runTempDir(Map("objects/Foo__c.object" -> source))(verify)
  }

  private def runAndCapture(root: PathLike, format: String): (Int, String) = {
    val bytes  = new ByteArrayOutputStream()
    val stream = new PrintStream(bytes, true, StandardCharsets.UTF_8.name())
    val status = Console.withOut(stream) {
      CheckForIssues.run(format, "none", "errors", nocache = true, Seq.empty, root.toString, "")
    }
    stream.flush()
    (status, new String(bytes.toByteArray, StandardCharsets.UTF_8))
  }
}
