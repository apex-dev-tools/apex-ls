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
package com.nawforce.pkgforce.sfdx

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class ApexConfigTest extends AnyFunSuite with BeforeAndAfter {

  private var logger: CatchingLogger = _

  before {
    logger = new CatchingLogger
  }

  // additionalNamespaces tests
  test("Empty additionalNamespaces") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": []}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.get.apexConfig.plugins.size == 1)
      assert(project.get.apexConfig.plugins.contains("additionalNamespaces"))
      assert(project.get.apexConfig.additionalNamespaces.isEmpty)
    }
  }

  test("additionalNamespaces not an array") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": 42}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)

      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(ERROR_CATEGORY, Location(1, 37), "'additionalNamespaces' should be an array")
          )
        )
      )
    }
  }

  test("additionalNamespaces single ns") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"ns\"]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)

      assert(project.get.apexConfig.additionalNamespaces sameElements Array(Some(Name("ns"))))
      val globs = project.get.metadataGlobs
      assert(globs.length == 1)
      assert(globs.exists(_.startsWith(".apexlink/gulp/ns/**/")))
    }
  }

  test("additionalNamespaces multiple ns") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"ns1\", \"unmanaged\"]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)

      assert(
        project.get.apexConfig.additionalNamespaces sameElements Array(Some(Name("ns1")), None)
      )
      val globs = project.get.metadataGlobs
      assert(globs.length == 2)
      assert(globs.exists(_.startsWith(".apexlink/gulp/ns1/**/")))
      assert(globs.exists(_.startsWith(".apexlink/gulp/unmanaged/**/")))
    }
  }

  test("additionalNamespaces bad path element") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"ns\", 10]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)

      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(
              ERROR_CATEGORY,
              Location(1, 44),
              "'additionalNamespaces' entries should all be strings"
            )
          )
        )
      )
    }
  }

  test("additionalNamespaces duplicate") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"other\", \"ns\", \"another\", \"ns\"]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)

      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(
              ERROR_CATEGORY,
              Location(1, 37),
              "namespace 'ns' is duplicated in additionalNamespaces'"
            )
          )
        )
      )
    }
  }

  test("additionalNamespaces duplicate (unmanaged)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"other\", \"unmanaged\", \"another\", \"unmanaged\"]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)

      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(
              ERROR_CATEGORY,
              Location(1, 37),
              "namespace 'unmanaged' is duplicated in additionalNamespaces'"
            )
          )
        )
      )
    }
  }

  // maxDependencyCount tests
  test("max dependency count valid") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          | "packageDirectories": [],
          | "plugins": {
          |   "maxDependencyCount": 123
          | }
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.get.apexConfig.maxDependencyCount.contains(123))
    }
  }

  test("max dependency count zero valid") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          | "packageDirectories": [],
          | "plugins": {
          |   "maxDependencyCount": 0
          | }
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.get.apexConfig.maxDependencyCount.contains(0))
    }
  }

  test("max dependency count negative invalid") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          | "packageDirectories": [],
          | "plugins": {
          |   "maxDependencyCount": -2
          | }
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(
              ERROR_CATEGORY,
              Location(4, 25),
              "'maxDependencyCount' value '-2' should be a positive integer"
            )
          )
        )
      )
    }
  }

  test("max dependency count too big invalid") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          | "packageDirectories": [],
          | "plugins": {
          |   "maxDependencyCount": 2147483648
          | }
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(
              ERROR_CATEGORY,
              Location(4, 25),
              "'maxDependencyCount' value '2147483648' is not an integer"
            )
          )
        )
      )
    }
  }

  test("max dependency count not number") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
          | "packageDirectories": [],
          | "plugins": {
          |   "maxDependencyCount": "foo"
          | }
          |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(
              ERROR_CATEGORY,
              Location(4, 25),
              "'maxDependencyCount' value '\"foo\"' should be a positive integer"
            )
          )
        )
      )
    }
  }

  // options parsing tests
  test("options parsing - empty object") {
    FileSystemHelper.run(
      Map("sfdx-project.json" -> "{\"plugins\": {\"options\": {}}, \"packageDirectories\": []}")
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.options.isEmpty)
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V2) // Default value
    }
  }

  test("options parsing - with forceIgnoreVersion v1") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v1\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v1"))
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V1)
    }
  }

  test("options parsing - with forceIgnoreVersion v2") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v2\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v2"))
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V2)
    }
  }

  test("options parsing - with multiple options") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v1\", \"customOption\": \"value\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(
        project.get.apexConfig.options == Map(
          "forceIgnoreVersion" -> "v1",
          "customOption"       -> "value"
        )
      )
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V1)
    }
  }

  test("options parsing - invalid forceIgnoreVersion") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v3\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail due to invalid version
      assert(
        logger.issues.exists(
          _.diagnostic.message
            .contains("'options.forceIgnoreVersion' must be one of 'v1', 'v2', got 'v3'")
        )
      )
    }
  }

  test("options parsing - non-string value") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": 123}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail due to non-string value
      assert(
        logger.issues.exists(
          _.diagnostic.message.contains("'options.forceIgnoreVersion' should be a string value")
        )
      )
    }
  }

  test("options parsing - non-object options") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": \"invalid\"}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty)
      assert(logger.issues.exists(_.diagnostic.message.contains("'options' should be an object")))
    }
  }

  test("options parsing - missing options") {
    FileSystemHelper.run(
      Map("sfdx-project.json" -> "{\"plugins\": {}, \"packageDirectories\": []}")
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.options.isEmpty)
    }
  }

  // apex-ls configuration tests
  test("apex-ls configuration - simple options") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"options\": {\"forceIgnoreVersion\": \"v1\"}}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.options.contains("forceIgnoreVersion"))
      assert(project.get.apexConfig.options("forceIgnoreVersion") == "v1")
    }
  }

  test("apex-ls configuration - additionalNamespaces") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"additionalNamespaces\": [\"ns1\", \"ns2\"]}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(
        project.get.apexConfig.additionalNamespaces sameElements Array(
          Some(Name("ns1")),
          Some(Name("ns2"))
        )
      )
    }
  }

  test("apex-ls configuration - apex-ls not an object") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": \"invalid\"}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty)
      assert(
        logger.issues == ArraySeq(
          Issue(
            root.join("sfdx-project.json"),
            Diagnostic(ERROR_CATEGORY, Location(1, 24), "'plugins.apex-ls' should be an object")
          )
        )
      )
    }
  }

  test("apex-ls configuration - backward compatibility (legacy plugins)") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"legacy\"], \"options\": {\"forceIgnoreVersion\": \"v1\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.additionalNamespaces sameElements Array(Some(Name("legacy"))))
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v1"))
    }
  }

  test("apex-ls configuration - apex-ls takes precedence over legacy") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"legacy\"], \"apex-ls\": {\"additionalNamespaces\": [\"new\"]}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      // apex-ls config should take precedence
      assert(project.get.apexConfig.additionalNamespaces sameElements Array(Some(Name("new"))))
    }
  }

  test("apex-ls configuration - maxDependencyCount") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"maxDependencyCount\": 50}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isDefined)
      assert(project.get.apexConfig.maxDependencyCount.contains(50))
    }
  }
}
