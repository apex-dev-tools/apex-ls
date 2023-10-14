/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink.analysis

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.{LoadAndRefreshAnalysis, NoAnalysis, RefreshAnalysis}
import com.nawforce.pkgforce.diagnostics.{ERROR_CATEGORY, Issue => InternalIssue}
import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation}
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Path
import io.github.apexdevtools.api.Issue
import io.github.apexdevtools.spi.AnalysisProvider
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Path => JVMPath}
import java.util
import scala.jdk.CollectionConverters._

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
              ERROR_CATEGORY,
              PathLocation(testClassPath, Location(1, 18)),
              "mismatched input '<EOF>' expecting {'extends', 'implements', '{'}"
            )
          )

          createOrg(root)
          val messageParts = getMessages(testClassPath).split('\n')
          assert(messageParts.length == 2)
          assert(
            messageParts.head.startsWith(
              "Error: line 1 at 18: mismatched input '<EOF>' expecting {'extends', 'implements',"
            )
          )
          assert(messageParts(1) == "Missing: line 1 at 24-25: No type declaration found for 'Bar'")
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

  test("Provider not configured with bad id") {
    withExternalAnalysis(LoadAndRefreshAnalysis, Map("BAD" -> List(("param", List("a", "b"))))) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(MockAnalysisProvider.config == Nil)
        assert(MockAnalysisProvider.requests.length == 1)
      }
    }
  }

  test("Provider not configured with empty list") {
    withExternalAnalysis(LoadAndRefreshAnalysis, Map("MOCK" -> List())) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(MockAnalysisProvider.config == Nil)
        assert(MockAnalysisProvider.requests.length == 1)
      }
    }
  }

  test("Provider configured") {
    withExternalAnalysis(LoadAndRefreshAnalysis, Map("MOCK" -> List(("param", List("a", "b"))))) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(MockAnalysisProvider.config == List(("param", List("a", "b"))))
        assert(MockAnalysisProvider.requests.length == 1)
      }
    }
  }

  test("Provider configured with multiple params") {
    withExternalAnalysis(
      LoadAndRefreshAnalysis,
      Map("MOCK" -> List(("param", List("a", "b")), ("param", List("c", "d"))))
    ) {
      FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
        createHappyOrg(root)
        assert(
          MockAnalysisProvider.config == List(("param", List("c", "d")), ("param", List("a", "b")))
        )
        assert(MockAnalysisProvider.requests.length == 1)
      }
    }
  }

}

class MockAnalysisProvider extends AnalysisProvider {
  override def getProviderId: String = "MOCK"

  override def isConfigured(workspacePath: JVMPath): java.lang.Boolean = true

  override def setConfiguration(name: String, values: util.List[String]): Unit = {
    MockAnalysisProvider.config = (name, values.asScala.toList) :: MockAnalysisProvider.config
  }

  override def collectIssues(workspacePath: JVMPath, files: Array[JVMPath]): Array[Issue] = {
    MockAnalysisProvider.requests = (workspacePath, files) :: MockAnalysisProvider.requests
    MockAnalysisProvider.issues
  }
}

object MockAnalysisProvider {
  var config: List[(String, List[String])]      = Nil
  var issues: Array[Issue]                      = Array()
  var requests: List[(JVMPath, Array[JVMPath])] = Nil

  def reset(): Unit = {
    config = Nil
    issues = Array()
    requests = Nil
  }
}
