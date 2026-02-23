/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.rpc

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.org.OPM.PackageImpl
import com.nawforce.apexlink.{ParserHelper, TestHelper}
import com.nawforce.pkgforce.diagnostics.LoggerOps.{DEBUG_LOGGING, NO_LOGGING}
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.names.{Name, TypeIdentifier, TypeName}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.{Environment, Path}
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.{Assertion, BeforeAndAfterEach}

import scala.concurrent.Future

class OrgAPITest extends AsyncFunSuite with BeforeAndAfterEach with TestHelper {

  val samplesDir: Path = {
    var dir = Path(".").join("samples")
    if (!dir.isDirectory) {
      dir = Path("..").join("samples")
    }
    assert(dir.isDirectory, s"Failed to locate samples from ${Path(".").toString}")
    dir
  }

  def deleteDir(path: PathLike): Unit = {
    path.splitDirectoryEntries()._1.foreach(file => file.delete())
    path.splitDirectoryEntries()._2.foreach(dir => deleteDir(dir))
    path.delete()
  }

  override def beforeEach(): Unit = {
    ParserHelper.setParser()
  }

  test("Version not empty") {
    val orgAPI = OrgAPI()
    for {
      result <- orgAPI.version()
    } yield {
      assert(result.nonEmpty)
    }
  }

  test("Set cache dir empty") {
    val orgAPI = OrgAPI()
    for {
      _ <- orgAPI.setCacheDirectory(None)
    } yield {
      assert(Environment.cacheDir.isEmpty)
      assert(!ServerOps.isAutoFlushEnabled)
    }
  }

  test("Set cache dir pwd") {
    val orgAPI = OrgAPI()
    Path("").createDirectory("cacheTest")
    val testPath = Path("").join("cacheTest")
    for {
      _ <- orgAPI.setCacheDirectory(Some(testPath.toString))
    } yield {
      val result: Assertion =
        assert(Environment.cacheDir.contains(testPath) && ServerOps.isAutoFlushEnabled)
      result.onComplete(_ => {
        Environment.setCacheDirOverride(None)
        testPath.delete()
      })
      result
    }
  }

  test("Open bad directory") {
    val orgAPI = OrgAPI()
    for {
      result <- orgAPI.open("/silly")
      issues <- orgAPI.getIssues(includeWarnings = false, maxIssuesPerFile = 0)
    } yield {
      assert(result.error.isEmpty)

      val path = Path("/silly")
      assert(
        issues.issues sameElements Array(
          Issue(path, Diagnostic(ERROR_CATEGORY, Location.empty, s"No directory at $path"))
        )
      )
    }
  }

  test("Open MDAPI directory") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      issues <- orgAPI.getIssues(includeWarnings = false, maxIssuesPerFile = 0)
    } yield {
      assert(result.error.isEmpty)
      assert(issues.issues.forall(_.diagnostic.category != ERROR_CATEGORY))
    }
  }

  test("Open sfdx directory (relative)") {
    val orgAPI = OrgAPI()
    for {
      result <- orgAPI.open(samplesDir.join("sfdx-test").toString())
      issues <- orgAPI.getIssues(includeWarnings = false, maxIssuesPerFile = 0)
    } yield {
      assert(result.error.isEmpty && result.namespaces.sameElements(Array("")))
      assert(issues.issues.forall(_.diagnostic.category != ERROR_CATEGORY))
    }
  }

  test("Open sfdx directory (absolute)") {
    val workspace = samplesDir.join("sfdx-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      issues <- orgAPI.getIssues(includeWarnings = false, maxIssuesPerFile = 0)
    } yield {
      assert(result.error.isEmpty && result.namespaces.sameElements(Array("")))
      assert(issues.issues.forall(_.diagnostic.category != ERROR_CATEGORY))
    }
  }

  test("Open sfdx directory with ns (relative)") {
    val orgAPI = OrgAPI()
    for {
      result <- orgAPI.open(samplesDir.join("sfdx-ns-test").toString())
      issues <- orgAPI.getIssues(includeWarnings = false, maxIssuesPerFile = 0)
    } yield {
      assert(result.error.isEmpty && result.namespaces.sameElements(Array("sfdx_test", "")))
      assert(!issues.issues.exists(_.diagnostic.category == ERROR_CATEGORY))
    }
  }

  test("Open sfdx directory with ns (absolute)") {
    val workspace = samplesDir.join("sfdx-ns-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      issues <- orgAPI.getIssues(includeWarnings = false, maxIssuesPerFile = 0)
    } yield {
      assert(result.error.isEmpty && result.namespaces.sameElements(Array("sfdx_test", "")))
      assert(!issues.issues.exists(_.diagnostic.category == ERROR_CATEGORY))
    }
  }

  test("Open with default options") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    val options   = OpenOptions.default()
    for {
      result <- orgAPI.open(workspace.toString, options)
    } yield {
      assert(result.error.isEmpty)
      assert(LoggerOps.getLoggingLevel == NO_LOGGING)
      assert(ServerOps.getCurrentParser == OutlineParserMultithreaded)
      assert(ServerOps.getExternalAnalysis.mode == RefreshAnalysis)
      assert(ServerOps.isAutoFlushEnabled)
      assert(Environment.getCacheDirOverride.isEmpty)
    }
  }

  final class TestNullLogger extends Logger {
    override def info(message: String): Unit  = {}
    override def debug(message: String): Unit = {}
    override def trace(message: String): Unit = {}
  }

  test("Open with changed options") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    val cacheDir  = Path("testCacheDir")
    val options = OpenOptions
      .default()
      .withParser("OutlineSingle")
      .withLoggingLevel("DEBUG")
      .withExternalAnalysisMode(NoAnalysis.shortName, Map())
      .withCacheDirectory(cacheDir.toString)
      .withIndexerConfiguration(1, 2)
    val oldLogger = LoggerOps.setLogger(new TestNullLogger())
    for {
      result <- orgAPI.open(workspace.toString, options)
    } yield {
      assert(result.error.isEmpty)
      assert(LoggerOps.getLoggingLevel == DEBUG_LOGGING)
      assert(ServerOps.getCurrentParser == OutlineParserSingleThreaded)
      assert(ServerOps.getExternalAnalysis.mode == NoAnalysis)
      assert(ServerOps.isAutoFlushEnabled)
      assert(ServerOps.getIndexerConfiguration == IndexerConfiguration(1, 2))
      assert(Environment.getCacheDirOverride.contains(Some(cacheDir)))

      LoggerOps.setLoggingLevel(NO_LOGGING)
      LoggerOps.setLogger(oldLogger)
      ServerOps.setCurrentParser(OutlineParserMultithreaded)
      ServerOps.setIndexerConfiguration(IndexerConfiguration(0, 0))
      Environment.setCacheDirOverride(None)
      deleteDir(cacheDir)
      assert(1 == 1)
    }
  }

  test("Get Issues") {
    val workspace = samplesDir.join("sfdx-ns-test")
    val orgAPI    = OrgAPI()

    val pkg: Future[Assertion] = orgAPI.open(workspace.toString) map { result =>
      assert(result.error.isEmpty && result.namespaces.sameElements(Array("sfdx_test", "")))
    }

    pkg flatMap { _ =>
      orgAPI.getIssues(includeWarnings = true, maxIssuesPerFile = 0) map { issuesResult =>
        assert(issuesResult.issues.length == 3)
        assert(issuesResult.issues.count(_.path.toString.contains("SingleError")) == 1)
        assert(issuesResult.issues.count(_.path.toString.contains("DoubleError")) == 2)
      }
    }
  }

  test("Get Dependency Graph (zero depth)") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      graph <- orgAPI.dependencyGraph(
        IdentifiersRequest(Array(TypeIdentifier(None, TypeName(Name("Hello"))))),
        depth = 0,
        apexOnly = true,
        IdentifiersRequest(Array())
      )
    } yield {
      assert(result.error.isEmpty)
      assert(graph.nodeData.length == 1)
      assert(graph.linkData.isEmpty)
    }
  }

  test("Get Dependency Graph (some depth)") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      graph <- orgAPI.dependencyGraph(
        IdentifiersRequest(Array(TypeIdentifier(None, TypeName(Name("Hello"))))),
        depth = 1,
        apexOnly = true,
        IdentifiersRequest(Array())
      )
    } yield {
      assert(result.error.isEmpty)
      val helloSize = samplesDir
        .join("mdapi-test")
        .join("Hello.cls")
        .readBytes()
        .toOption
        .map(_.length)
        .getOrElse(0)
      val worldSize = samplesDir
        .join("mdapi-test")
        .join("World.cls")
        .readBytes()
        .toOption
        .map(_.length)
        .getOrElse(0)

      assert(
        graph.nodeData sameElements Array(
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("Hello"))),
            helloSize,
            "class",
            1,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("World"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("World"))),
            worldSize,
            "class",
            0,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array()
          )
        )
      )
      assert(graph.linkData sameElements Array(DependencyLink(0, 1, "uses")))
    }
  }

  test("Get Dependency Graph (some depth) with ignored identifiers") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      graph <- orgAPI.dependencyGraph(
        IdentifiersRequest(Array(TypeIdentifier(None, TypeName(Name("Hello"))))),
        depth = 1,
        apexOnly = true,
        IdentifiersRequest(Array(TypeIdentifier(None, TypeName(Name("World")))))
      )
    } yield {
      assert(result.error.isEmpty)
      val helloSize = samplesDir
        .join("mdapi-test")
        .join("Hello.cls")
        .readBytes()
        .toOption
        .map(_.length)
        .getOrElse(0)
      assert(
        graph.nodeData sameElements Array(
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("Hello"))),
            helloSize,
            "class",
            0,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array()
          )
        )
      )
      assert(graph.linkData.isEmpty)
    }
  }

  test("Get Dependency Graph (bad identifier))") {
    val workspace = samplesDir.join("mdapi-test")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      graph <- orgAPI.dependencyGraph(
        IdentifiersRequest(Array(TypeIdentifier(None, TypeName(Name("Dummy"))))),
        depth = 0,
        apexOnly = true,
        IdentifiersRequest(Array())
      )
    } yield {
      assert(result.error.isEmpty)
      assert(graph.nodeData.isEmpty)
      assert(graph.linkData.isEmpty)
    }
  }

  test("Get Test Class Names (with test class)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/HelloTest.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.length == 1)
      assert(classes.testClassesWithPath(0)._1 == "HelloTest")
    }
  }

  test("Get Test Class Names (find test class)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/Hello.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.length == 1)
      assert(classes.testClassesWithPath(0)._1 == "HelloTest")
    }
  }

  test("Get Test Class Names (no test class)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/NoTest.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.isEmpty)
    }
  }

  test("Get Test Class Names (indirect to inner interface)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/ServiceImpl.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.length == 1)
      assert(classes.testClassesWithPath(0)._1 == "ServiceAPITest")
    }
  }

  test("Get Test Class Names (indirect to interface)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/APIImpl.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.length == 1)
      assert(classes.testClassesWithPath(0)._1 == "APITest")
    }
  }

  test("Get Test Class Names (indirect to inner implementation)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/InnerServiceImpl.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.length == 1)
      assert(classes.testClassesWithPath(0)._1 == "ServiceAPITest")
    }
  }

  test("Get Test Class Names (service)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/Service.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.map(_._1).toSet == Set("ServiceAPITest", "ServiceTest"))
    }
  }

  test("Get Test Class Names (with superclass)") {
    val workspace = samplesDir.join("test-classes")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      classes <- orgAPI.getTestClassNames(
        new GetTestClassNamesRequest(
          Array(workspace.toString + "/force-app/main/default/classes/Derived.cls")
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(classes.testClassesWithPath.map(_._1).toSet == Set("APITest", "DerivedTest"))
    }
  }

  test("Get DependencyCounts") {
    val workspace = samplesDir.join("dependency-counts")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      dependencyCounts <- orgAPI.getDependencyCounts(
        new GetDependencyCountsRequest(
          Array(
            workspace.toString + "/force-app/main/default/classes/NoDeps.cls",
            workspace.toString + "/force-app/main/default/classes2/SingleDep.cls",
            workspace.toString + "/force-app/main/default/classes/TransDep.cls",
            workspace.toString + "/force-app/main/default/classes/TestDep.cls"
          ),
          false
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(dependencyCounts.counts.length == 4)
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("TestDep")).map(_.count).apply(0) == 2
      )
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("TransDep")).map(_.count).apply(0) == 2
      )
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("SingleDep")).map(_.count).apply(0) == 1
      )
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("NoDeps")).map(_.count).apply(0) == 0
      )
    }
  }

  test("Get DependencyCounts (exclude tests)") {
    val workspace = samplesDir.join("dependency-counts")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      dependencyCounts <- orgAPI.getDependencyCounts(
        new GetDependencyCountsRequest(
          Array(
            workspace.toString + "/force-app/main/default/classes/NoDeps.cls",
            workspace.toString + "/force-app/main/default/classes2/SingleDep.cls",
            workspace.toString + "/force-app/main/default/classes/TransDep.cls",
            workspace.toString + "/force-app/main/default/classes/TestDep.cls"
          ),
          true
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(dependencyCounts.counts.forall(_.maxDependencyCount.isLeft))
      assert(dependencyCounts.counts.flatMap(_.maxDependencyCount.swap.toOption).flatten.isEmpty)
      assert(dependencyCounts.counts.length == 3)
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("TransDep")).map(_.count).apply(0) == 2
      )
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("SingleDep")).map(_.count).apply(0) == 1
      )
      assert(
        dependencyCounts.counts.filter(c => c.path.contains("NoDeps")).map(_.count).apply(0) == 0
      )
    }
  }

  test("Get DependencyCounts with maxDependencyCount info from SFDX project") {
    FileSystemHelper.runWithCopy(samplesDir.join("dependency-counts")) { root: PathLike =>
      root.createFile(
        "sfdx-project.json",
        """
          |{
          |  "packageDirectories": [
          |    {
          |      "path": "force-app",
          |      "default": true
          |    }
          |  ],
          |  "namespace": "",
          |  "sfdcLoginUrl": "https://login.salesforce.com",
          |  "sourceApiVersion": "48.0",
          |  "plugins" : {
          |     "maxDependencyCount" : 12
          |  }
          |}
          |""".stripMargin
      )

      val orgAPI = createOrg(root)
      assert(
        orgAPI
          .getDependencyCounts(
            Array(
              root.toString + "force-app/main/default/classes/NoDeps.cls",
              root.toString + "force-app/main/default/classes/SingleDep.cls",
              root.toString + "force-app/main/default/classes/TransDep.cls",
              root.toString + "force-app/main/default/classes/TestDep.cls"
            ),
            excludeTestClasses = true
          )
          .map(_.maxDependencyCount)
          .forall(d => d.isRight && d.toOption.get == 12)
      )
    }
  }

  test("Get DependencyCounts with MaxDependencyCount comment") {
    FileSystemHelper.runWithCopy(samplesDir.join("dependency-counts")) { root: PathLike =>
      root.createFile(
        "sfdx-project.json",
        """
                                                  |{
                                                  |  "packageDirectories": [
                                                  |    {
                                                  |      "path": "force-app",
                                                  |      "default": true
                                                  |    }
                                                  |  ],
                                                  |  "namespace": "",
                                                  |  "sfdcLoginUrl": "https://login.salesforce.com",
                                                  |  "sourceApiVersion": "48.0",
                                                  |  "plugins" : {
                                                  |     "maxDependencyCount" : 12
                                                  |  }
                                                  |}
                                                  |""".stripMargin
      )
      root.createFile(
        root.join("force-app/main/default/classes/Test.cls").toString,
        """
        |//MaxDependencyCount(60)
        |public class Test {}""".stripMargin
      )

      val orgAPI = createOrg(root)
      assert(
        orgAPI
          .getDependencyCounts(
            Array(root.join("force-app/main/default/classes/Test.cls").toString),
            excludeTestClasses = true
          )
          .map(_.maxDependencyCount)
          .forall(d => d.isRight && d.toOption.get == 60)
      )
    }
  }

  test("Get DependencyCounts with MaxDependencyCount comment with negative integer") {
    FileSystemHelper.runWithCopy(samplesDir.join("dependency-counts")) { root: PathLike =>
      root.createFile(
        root.toString + "/force-app/main/default/classes/Test.cls",
        """
          |//MaxDependencyCount(-1)
          |public class Test {}
          |""".stripMargin
      )
      val orgAPI = createOrg(root)
      assert(
        orgAPI
          .getDependencyCounts(
            Array(root.toString + "/force-app/main/default/classes/Test.cls"),
            excludeTestClasses = true
          )
          .map(_.maxDependencyCount)
          .forall(d => d.isLeft && d.swap.toOption.flatten.get == "'-1' must be >=0")
      )

    }
  }

  test("Get DependencyCounts with MaxDependencyCount comment with non integer") {
    FileSystemHelper.runWithCopy(samplesDir.join("dependency-counts")) { root: PathLike =>
      root.createFile(
        root.toString + "/force-app/main/default/classes/Test.cls",
        """
        |//MaxDependencyCount(abc)
        |public class Test {}""".stripMargin
      )

      val orgAPI = createOrg(root)
      assert(
        orgAPI
          .getDependencyCounts(
            Array(root.toString + "/force-app/main/default/classes/Test.cls"),
            excludeTestClasses = true
          )
          .map(_.maxDependencyCount)
          .forall(d => d.isLeft && d.swap.toOption.flatten.get == "'abc' is not an integer value")
      )

    }
  }

  test("Get correct transitive count with gulped data") {
    FileSystemHelper.runWithCopy(samplesDir.join("dependency-counts")) { root: PathLike =>
      val orgAPI = createHappyOrg(root)
      val transitiveCount = orgAPI
        .getDependencyCounts(
          Array(root.join("/force-app/main/default/classes/GulpTransDep.cls").toString),
          excludeTestClasses = false
        )
        .map(_.count)
      assert(orgAPI.packages.collect({ case impl: PackageImpl => impl }).exists(_.isGulped))
      assert(transitiveCount.head == 4)
    }
  }

  test("Get All DependencyCounts") {
    val workspace = samplesDir.join("dependency-counts")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      dependencyCounts <- orgAPI.getAllDependencyCounts(
        new GetAllDependencyCountsRequest(workspace.toString, excludeTestClasses = false)
      )
    } yield {
      assert(result.error.isEmpty)
      assert(
        dependencyCounts.counts.map(dependency => Path(dependency.path)).toSet == Set(
          ".apexlink/gulp/ns1/classes/Dep.cls",
          ".apexlink/gulp/ns1/classes/Dep2.cls",
          "force-app/main/default/classes2/SingleDep.cls",
          "force-app/main/default/classes/NoDeps.cls",
          "force-app/main/default/classes/TransDep.cls",
          "force-app/main/default/classes/GulpTransDep.cls",
          "force-app/main/default/classes/TestDep.cls",
          "force-app/main/default/triggers/AccountTrigger.trigger"
        ).map(workspace.join(_))
      )
    }
  }

  test("Get All DependencyCounts excluding tests") {
    val workspace = samplesDir.join("dependency-counts")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      dependencyCounts <- orgAPI.getAllDependencyCounts(
        new GetAllDependencyCountsRequest(workspace.toString, excludeTestClasses = true)
      )
    } yield {
      assert(result.error.isEmpty)
      assert(
        dependencyCounts.counts.map(dependency => Path(dependency.path)).toSet == Set(
          ".apexlink/gulp/ns1/classes/Dep.cls",
          ".apexlink/gulp/ns1/classes/Dep2.cls",
          "force-app/main/default/classes2/SingleDep.cls",
          "force-app/main/default/classes/NoDeps.cls",
          "force-app/main/default/classes/TransDep.cls",
          "force-app/main/default/classes/GulpTransDep.cls",
          "force-app/main/default/triggers/AccountTrigger.trigger"
        ).map(workspace.join(_))
      )
    }
  }

  test("Get All DependencyCounts specific directory") {
    val workspace = samplesDir.join("dependency-counts")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      dependencyCounts <- orgAPI.getAllDependencyCounts(
        new GetAllDependencyCountsRequest(
          workspace.join("force-app/main/default/classes").toString,
          excludeTestClasses = true
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(
        dependencyCounts.counts.map(dependency => Path(dependency.path)).toSet == Set(
          "force-app/main/default/classes/NoDeps.cls",
          "force-app/main/default/classes/TransDep.cls",
          "force-app/main/default/classes/GulpTransDep.cls"
        ).map(workspace.join(_))
      )
    }
  }

  test("Get All DependencyCounts specific directory outside workspace") {
    val workspace = samplesDir.join("dependency-counts")
    val orgAPI    = OrgAPI()
    for {
      result <- orgAPI.open(workspace.toString)
      dependencyCounts <- orgAPI.getAllDependencyCounts(
        new GetAllDependencyCountsRequest(
          workspace.join("../test-classes").toString,
          excludeTestClasses = true
        )
      )
    } yield {
      assert(result.error.isEmpty)
      assert(dependencyCounts.counts.isEmpty)
    }
  }

}
