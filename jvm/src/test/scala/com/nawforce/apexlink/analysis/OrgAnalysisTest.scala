/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.api.{LoadAndRefreshAnalysis, NoAnalysis, RefreshAnalysis}
import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.diagnostics.{ERROR_CATEGORY, Issue => InternalIssue}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.api.Issue
import io.github.apexdevtools.spi.AnalysisProvider
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Path => JVMPath}

class OrgAnalysisTest extends AnyFunSuite with BeforeAndAfter with TestHelper {

  before {
    MockAnalysisProvider.reset()
  }

  test("Load analysis is not called when all analysis disabled") {
    withExternalAnalysis(NoAnalysis) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(MockAnalysisProvider.requests.isEmpty)
      }
    }
  }

  test("Load analysis is not called when only refresh analysis enabled") {
    withExternalAnalysis(RefreshAnalysis) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(MockAnalysisProvider.requests.isEmpty)
      }
    }
  }

  test("Load analysis is called when setup") {
    withExternalAnalysis(LoadAndRefreshAnalysis) {
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

  test("Load analysis is skipped on syntax error") {
    withExternalAnalysis(LoadAndRefreshAnalysis) {
      FileSystemHelper.run(Map("foo/Dummy.cls" -> "public class Dummy {")) { root: PathLike =>
        createOrg(root)
        assert(MockAnalysisProvider.requests.length == 1)
        assert(MockAnalysisProvider.requests.head._1.toString == root.toString)
        assert(MockAnalysisProvider.requests.head._2.isEmpty)
      }
    }
  }

  test("Load analysis merges returned issues") {
    withExternalAnalysis(LoadAndRefreshAnalysis) {
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

  test("Refresh analysis is not called when all analysis disabled") {
    withExternalAnalysis(NoAnalysis) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        val org = createHappyOrg(root)
        assert(MockAnalysisProvider.requests.isEmpty)
        org.unmanaged.refresh(root.join("Dummy.cls"), highPriority = true)
        assert(MockAnalysisProvider.requests.isEmpty)
      }
    }
  }

  test("Refresh analysis is called when refresh only enabled") {
    withExternalAnalysis(RefreshAnalysis) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: Path =>
        val org = createHappyOrg(root)
        assert(MockAnalysisProvider.requests.isEmpty)
        val dummyPath = root.join("Dummy.cls")
        org.unmanaged.refresh(dummyPath, highPriority = true)
        assert(MockAnalysisProvider.requests.size == 1)
        assert(MockAnalysisProvider.requests.head._1 == root.native)
        assert(MockAnalysisProvider.requests.head._2 sameElements Array(dummyPath.native))
      }
    }
  }

  test("Refresh analysis is called when LoadAndRefresh enabled") {
    withExternalAnalysis(LoadAndRefreshAnalysis) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: Path =>
        val org = createHappyOrg(root)
        assert(MockAnalysisProvider.requests.size == 1)
        val dummyPath = root.join("Dummy.cls")
        org.unmanaged.refresh(dummyPath, highPriority = true)
        assert(MockAnalysisProvider.requests.size == 2)
        assert(MockAnalysisProvider.requests.head._1 == root.native)
        assert(MockAnalysisProvider.requests.head._2 sameElements Array(dummyPath.native))
        assert(MockAnalysisProvider.requests(1)._1 == root.native)
        assert(MockAnalysisProvider.requests(1)._2 sameElements Array(dummyPath.native))
      }
    }
  }

}

class MockAnalysisProvider extends AnalysisProvider {
  override def getProviderId: String = "MOCK"

  override def collectIssues(workspacePath: JVMPath, files: Array[JVMPath]): Array[Issue] = {
    MockAnalysisProvider.requests = (workspacePath, files) :: MockAnalysisProvider.requests
    MockAnalysisProvider.issues
  }
}

object MockAnalysisProvider {
  var issues: Array[Issue]                      = Array()
  var requests: List[(JVMPath, Array[JVMPath])] = Nil

  def reset(): Unit = {
    issues = Array()
    requests = Nil
  }
}
