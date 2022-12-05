/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.diagnostics.{ERROR_CATEGORY, Issue => InternalIssue}
import com.nawforce.pkgforce.path.{Location, PathLike}
import io.github.apexdevtools.apexls.api.Issue
import io.github.apexdevtools.apexls.spi.AnalysisProvider
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class OrgAnalysisTest extends AnyFunSuite with BeforeAndAfter with TestHelper {

  before {
    MockAnalysisProvider.reset()
  }

  test("Custom analysis is not called by default") {
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
      createHappyOrg(root)
      assert(MockAnalysisProvider.requests.isEmpty)
    }
  }

  test("Custom analysis is called when setup") {
    withExternalAnalysis {
      FileSystemHelper.run(Map("foo/Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(MockAnalysisProvider.requests.length == 1)
        assert(MockAnalysisProvider.requests.head._1.toString == root.toString)
        assert(MockAnalysisProvider.requests.head._2.length == 1)
        assert(
          MockAnalysisProvider.requests.head._2.head.toString == root
            .join("foo")
            .join("Dummy.cls")
            .toString
        )
      }
    }
  }

  test("Custom analysis is skipped on syntax error") {
    withExternalAnalysis {
      FileSystemHelper.run(Map("foo/Dummy.cls" -> "public class Dummy {")) { root: PathLike =>
        createOrg(root)
        assert(MockAnalysisProvider.requests.length == 1)
        assert(MockAnalysisProvider.requests.head._1.toString == root.toString)
        assert(MockAnalysisProvider.requests.head._2.isEmpty)
      }
    }
  }

  test("Custom analysis merges returned issues") {
    withExternalAnalysis {
      FileSystemHelper.run(Map("foo/Dummy.cls" -> "public class Dummy {Bar b;}")) {
        root: PathLike =>
          val testClassPath = root.join("foo").join("Dummy.cls")
          MockAnalysisProvider.issues = Array(
            InternalIssue(
              testClassPath,
              ERROR_CATEGORY,
              Location(1, 18),
              "mismatched input '<EOF>' expecting {'extends', 'implements', '{'}"
            )
          )

          createOrg(root)
          assert(
            getMessages(testClassPath) ==
              """Error: line 1 at 18: mismatched input '<EOF>' expecting {'extends', 'implements', '{'}
                |Missing: line 1 at 24-25: No type declaration found for 'Bar'
                |""".stripMargin
          )
      }
    }
  }
}

class MockAnalysisProvider extends AnalysisProvider {
  override def getProviderId: String = "MOCK"

  override def collectIssues(workspacePath: Path, files: Array[Path]): Array[Issue] = {
    MockAnalysisProvider.requests = (workspacePath, files) :: MockAnalysisProvider.requests
    MockAnalysisProvider.issues
  }
}

object MockAnalysisProvider {
  var issues: Array[Issue]                = Array()
  var requests: List[(Path, Array[Path])] = Nil

  def reset(): Unit = {
    issues = Array()
    requests = Nil
  }
}
