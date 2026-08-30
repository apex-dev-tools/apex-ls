package io.github.apexdevtools.apexls

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

class CheckForIssuesTest extends AnyFunSuite {

  private val errorMessage   = "No type declaration found for 'Silly'"
  private val warningMessage = "Local variable is hiding class field 'value'"
  private val unusedMessage  = "Unused local variable 'unusedLocal'"

  private val formats = Seq("text", "json", "pmd")

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

  test("detail modes select warnings and unused findings independently") {
    assert(
      CheckForIssues.DetailMode.all
        .map(_.name) == Seq("errors", "warnings", "errors-and-unused", "unused")
    )
    assert(!CheckForIssues.DetailMode.errors.includeWarnings)
    assert(!CheckForIssues.DetailMode.errors.includeUnused)
    assert(CheckForIssues.DetailMode.warnings.includeWarnings)
    assert(!CheckForIssues.DetailMode.warnings.includeUnused)
    assert(!CheckForIssues.DetailMode.errorsAndUnused.includeWarnings)
    assert(CheckForIssues.DetailMode.errorsAndUnused.includeUnused)
    assert(CheckForIssues.DetailMode.unused.includeWarnings)
    assert(CheckForIssues.DetailMode.unused.includeUnused)
  }

  test("unused analysis is enabled only for the modes that report unused findings") {
    assert(!CheckForIssues.DetailMode.errors.unusedAnalysis)
    assert(!CheckForIssues.DetailMode.warnings.unusedAnalysis)
    assert(CheckForIssues.DetailMode.errorsAndUnused.unusedAnalysis)
    assert(CheckForIssues.DetailMode.unused.unusedAnalysis)
  }

  test("unknown detail level is rejected") {
    withMixedIssues { root =>
      val (status, _) = runAndCapture(root, "text", "unused-only")
      assert(status == 1)
    }
  }

  test("errors detail reports only errors in every format") {
    withMixedIssues { root =>
      formats.foreach(format => {
        val (status, output) = runAndCapture(root, format, "errors")
        assert(status == 4, format)
        assert(reports(output, format, errorMessage), format)
        assert(!reports(output, format, warningMessage), format)
        assert(!reports(output, format, unusedMessage), format)
      })
    }
  }

  test("warnings detail reports errors and ordinary warnings in every format") {
    withMixedIssues { root =>
      formats.foreach(format => {
        val (status, output) = runAndCapture(root, format, "warnings")
        assert(status == 4, format)
        assert(reports(output, format, errorMessage), format)
        assert(reports(output, format, warningMessage), format)
        assert(!reports(output, format, unusedMessage), format)
      })
    }
  }

  test("errors-and-unused detail reports errors and unused findings in every format") {
    withMixedIssues { root =>
      formats.foreach(format => {
        val (status, output) = runAndCapture(root, format, "errors-and-unused")
        assert(status == 4, format)
        assert(reports(output, format, errorMessage), format)
        assert(!reports(output, format, warningMessage), format)
        assert(reports(output, format, unusedMessage), format)
      })
    }
  }

  test("unused detail reports errors, ordinary warnings and unused findings in every format") {
    withMixedIssues { root =>
      formats.foreach(format => {
        val (status, output) = runAndCapture(root, format, "unused")
        assert(status == 4, format)
        assert(reports(output, format, errorMessage), format)
        assert(reports(output, format, warningMessage), format)
        assert(reports(output, format, unusedMessage), format)
      })
    }
  }

  test("every format surfaces stable IDs and uncatalogued IDs fall back to categories") {
    withMixedIssues { root =>
      formats.foreach(format => {
        val (_, output) = runAndCapture(root, format, "unused")
        assert(identifier(output, format, errorMessage) == "missing-type", format)
        assert(identifier(output, format, warningMessage) == "Warning", format)
        assert(identifier(output, format, unusedMessage) == "unused-local-variable", format)
      })

      val (_, jsonOutput) = runAndCapture(root, "json", "unused")
      val warning         = jsonMessage(jsonOutput, warningMessage)
      assert(warning("category").str == "Warning")
      assert(warning("id").str == "Warning")
    }
  }

  test("exit status matches the reported set when there are no errors") {
    withoutErrors { root =>
      formats.foreach(format => {
        assert(runAndCapture(root, format, "errors")._1 == 0, format)
        assert(runAndCapture(root, format, "warnings")._1 == 5, format)
        assert(runAndCapture(root, format, "errors-and-unused")._1 == 6, format)
        assert(runAndCapture(root, format, "unused")._1 == 7, format)
      })
    }
  }

  test("nothing excluded from the report contributes to the exit status") {
    withoutErrors { root =>
      formats.foreach(format => {
        val (status, output) = runAndCapture(root, format, "errors-and-unused")
        assert(status == 6, format)
        assert(!reports(output, format, warningMessage), format)
      })
    }
  }

  test("configured path exclusions use forceignore semantics and change CLI status") {
    withExclusions("""[{"path": "classes/**"}]""", Map("classes/Bad.cls" -> errorSource)) { root =>
      formats.foreach { format =>
        val (status, output) = runAndCapture(root, format, "errors")
        assert(status == 0, format)
        assert(!reports(output, format, errorMessage), format)
      }
    }
  }

  test("configured severity exclusions filter the DiagnosticCategory") {
    withExclusions("""[{"severity": "Warning"}]""", Map("classes/Warner.cls" -> warningSource)) {
      root =>
        formats.foreach { format =>
          val (status, output) = runAndCapture(root, format, "warnings")
          assert(status == 0, format)
          assert(!reports(output, format, warningMessage), format)
        }
    }
  }

  test("configured ID exclusions filter exact stable IDs") {
    withExclusions("""[{"id": "missing-type"}]""", Map("classes/Bad.cls" -> errorSource)) { root =>
      formats.foreach { format =>
        val (status, output) = runAndCapture(root, format, "errors")
        assert(status == 0, format)
        assert(!reports(output, format, errorMessage), format)
      }
    }
  }

  test("configured selectors are ANDed within an entry") {
    withExclusions(
      """[{"path": "classes/**", "severity": "Missing", "id": "missing-type"}]""",
      Map(
        "classes/Bad.cls" -> errorSource,
        "other/Other.cls" -> errorSource.replace("class Bad", "class Other")
      )
    ) { root =>
      val (status, output) = runAndCapture(root, "json", "errors")
      assert(status == 4)
      val files = ujson.read(output)("files").arr
      assert(files.length == 1)
      assert(files.head("path").str.endsWith("other/Other.cls"))
    }
  }

  test("configured selector entries are ORed") {
    withExclusions(
      """[{"path": "classes/**", "id": "missing-type"}, {"severity": "Warning"}]""",
      Map("classes/Bad.cls" -> errorSource, "other/Warner.cls" -> warningSource)
    ) { root =>
      val (status, output) = runAndCapture(root, "json", "warnings")
      assert(status == 0)
      assert(ujson.read(output)("files").arr.isEmpty)
    }
  }

  test("source suppression happens before configured reporting exclusions") {
    val suppressed =
      "@SuppressWarnings('PMD') public class Dummy {class Inner {Integer b; List<Inner> a; {Integer b = a[null].b;}}}"
    withExclusions(
      """[{"id": "not-the-suppressed-diagnostic"}]""",
      Map("classes/Dummy.cls" -> suppressed)
    ) { root =>
      val (status, output) = runAndCapture(root, "json", "warnings")
      assert(status == 0)
      assert(ujson.read(output)("files").arr.isEmpty)
    }
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

  private val warningSource =
    "public class Warner { public String value; public void run() { String value = 'x'; System.debug(value); } }"

  private val unusedSource =
    "public class Unusd { public void keep() { String unusedLocal; } }"

  private val errorSource =
    "public class Bad { { Silly s = null; System.debug(s); } }"

  private def withMixedIssues(verify: PathLike => Unit): Unit = {
    FileSystemHelper.runTempDir(
      Map(
        "classes/Bad.cls"    -> errorSource,
        "classes/Warner.cls" -> warningSource,
        "classes/Unusd.cls"  -> unusedSource
      )
    )(verify)
  }

  private def withoutErrors(verify: PathLike => Unit): Unit = {
    FileSystemHelper.runTempDir(
      Map("classes/Warner.cls" -> warningSource, "classes/Unusd.cls" -> unusedSource)
    )(verify)
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

  private def withExclusions(exclusions: String, files: Map[String, String])(
    verify: PathLike => Unit
  ): Unit = {
    val project =
      s"""{
         |  "packageDirectories": [{"path": ".", "default": true}],
         |  "plugins": {"apex-ls": {"exclude": $exclusions}},
         |  "sourceApiVersion": "48.0"
         |}""".stripMargin
    FileSystemHelper.runTempDir(files + ("sfdx-project.json" -> project))(verify)
  }

  /** Extract the reported messages from a rendered report, so each renderer is checked against the
    * same expectations.
    */
  private def messages(output: String, format: String): Seq[String] = {
    format match {
      case "json" =>
        ujson
          .read(output)("files")
          .arr
          .flatMap(file => file("messages").arr.map(_("message").str))
          .toSeq
      case "pmd" =>
        (scala.xml.XML.loadString(output) \\ "violation").map(_.text.trim).toSeq
      case _ =>
        output.linesIterator.toSeq
    }
  }

  private def reports(output: String, format: String, message: String): Boolean = {
    messages(output, format).exists(_.contains(message))
  }

  private def identifier(output: String, format: String, message: String): String = {
    format match {
      case "json" => jsonMessage(output, message)("id").str
      case "pmd" =>
        (scala.xml.XML.loadString(output) \\ "violation")
          .find(_.text.contains(message))
          .map(_ \@ "rule")
          .getOrElse(fail(output))
      case _ =>
        output.linesIterator
          .find(_.contains(message))
          .map(_.takeWhile(_ != ':'))
          .getOrElse(fail(output))
    }
  }

  private def jsonMessage(output: String, message: String): ujson.Value = {
    ujson
      .read(output)("files")
      .arr
      .flatMap(file => file("messages").arr)
      .find(_("message").str.contains(message))
      .getOrElse(fail(output))
  }

  private def runAndCapture(
    root: PathLike,
    format: String,
    detail: String = "errors"
  ): (Int, String) = {
    val bytes  = new ByteArrayOutputStream()
    val stream = new PrintStream(bytes, true, StandardCharsets.UTF_8.name())
    val status = Console.withOut(stream) {
      CheckForIssues.run(format, "none", detail, nocache = true, Seq.empty, root.toString, "")
    }
    stream.flush()
    (status, new String(bytes.toByteArray, StandardCharsets.UTF_8))
  }
}
