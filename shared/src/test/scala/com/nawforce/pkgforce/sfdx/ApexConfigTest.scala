/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
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

import scala.collection.compat.immutable.ArraySeq

class ApexConfigTest extends AnyFunSuite with BeforeAndAfter {

  private var logger: CatchingLogger = _

  before {
    logger = new CatchingLogger
  }

  test("Legacy configuration - empty plugins") {
    FileSystemHelper.run(
      Map("sfdx-project.json" -> "{\"plugins\": {}, \"packageDirectories\": []}")
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.plugins.isEmpty)
      assert(project.get.apexConfig.dependencies.isEmpty)
      assert(project.get.apexConfig.additionalNamespaces.isEmpty)
      assert(project.get.apexConfig.maxDependencyCount.isEmpty)
      assert(project.get.apexConfig.options.isEmpty)
    }
  }

  test("Legacy configuration - with dependencies") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"dependencies\": [{\"namespace\": \"test\"}]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.dependencies.length == 1)
      assert(project.get.apexConfig.dependencies.head.namespace.contains(Name("test")))
    }
  }

  test("Legacy configuration - with maxDependencyCount") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"maxDependencyCount\": 42}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.maxDependencyCount.contains(42))
    }
  }

  test("Legacy configuration - with options") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v1\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v1"))
    }
  }

  test("Namespaced configuration - apex-ls key") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"dependencies\": [{\"namespace\": \"namespaced\"}]}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.dependencies.length == 1)
      assert(project.get.apexConfig.dependencies.head.namespace.contains(Name("namespaced")))
    }
  }

  test("Precedence - apex-ls takes precedence over legacy") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "maxDependencyCount": 10,
            |    "apex-ls": {
            |      "maxDependencyCount": 20
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.maxDependencyCount.contains(20)) // apex-ls value wins
    }
  }

  test("Precedence - dependencies") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "dependencies": [{"namespace": "legacy"}],
            |    "apex-ls": {
            |      "dependencies": [{"namespace": "namespaced"}]
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.dependencies.length == 1)
      assert(
        project.get.apexConfig.dependencies.head.namespace.contains(Name("namespaced"))
      ) // apex-ls value wins
    }
  }

  test("Precedence - options") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "options": {"forceIgnoreVersion": "v1", "legacyOption": "legacy"},
            |    "apex-ls": {
            |      "options": {"forceIgnoreVersion": "v2", "newOption": "new"}
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      // apex-ls options completely replace legacy options (not merged)
      assert(
        project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v2", "newOption" -> "new")
      )
    }
  }

  test("Error handling - apex-ls not an object") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": \"invalid\"}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail
      assert(
        logger.issues.exists(_.diagnostic.message.contains("'plugins.apex-ls' should be an object"))
      )
    }
  }

  test("Error handling - invalid dependencies in apex-ls") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"dependencies\": \"invalid\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail
      assert(
        logger.issues.exists(_.diagnostic.message.contains("'dependencies' should be an array"))
      )
    }
  }

  test("Error handling - invalid maxDependencyCount in apex-ls") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"maxDependencyCount\": \"invalid\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail
      assert(logger.issues.exists(_.diagnostic.message.contains("'maxDependencyCount' value")))
    }
  }

  test("Error handling - invalid options in apex-ls") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"options\": \"invalid\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail
      assert(logger.issues.exists(_.diagnostic.message.contains("'options' should be an object")))
    }
  }

  test("Mixed configuration - some apex-ls, some legacy") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "maxDependencyCount": 10,
            |    "options": {"legacyOption": "value"},
            |    "apex-ls": {
            |      "dependencies": [{"namespace": "test"}]
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      // apex-ls has dependencies, legacy has maxDependencyCount and options
      assert(project.get.apexConfig.dependencies.length == 1)
      assert(project.get.apexConfig.dependencies.head.namespace.contains(Name("test")))
      assert(project.get.apexConfig.maxDependencyCount.isEmpty) // Not in apex-ls config
      assert(project.get.apexConfig.options.isEmpty)            // Not in apex-ls config
    }
  }

  test("Additional namespaces - legacy configuration") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"additionalNamespaces\": [\"ns1\", \"unmanaged\"]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.additionalNamespaces.length == 2)
      assert(project.get.apexConfig.additionalNamespaces(0).contains(Name("ns1")))
      assert(project.get.apexConfig.additionalNamespaces(1).isEmpty) // "unmanaged" -> None
    }
  }

  test("Additional namespaces - apex-ls configuration") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"additionalNamespaces\": [\"ns2\", \"ns3\"]}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.additionalNamespaces.length == 2)
      assert(project.get.apexConfig.additionalNamespaces(0).contains(Name("ns2")))
      assert(project.get.apexConfig.additionalNamespaces(1).contains(Name("ns3")))
    }
  }

  test("Additional namespaces - precedence") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "additionalNamespaces": ["legacy"],
            |    "apex-ls": {
            |      "additionalNamespaces": ["namespaced"]
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.additionalNamespaces.length == 1)
      assert(
        project.get.apexConfig.additionalNamespaces(0).contains(Name("namespaced"))
      ) // apex-ls wins
    }
  }

  test("Additional namespaces - duplicate error in apex-ls") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"apex-ls\": {\"additionalNamespaces\": [\"test\", \"test\"]}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(project.isEmpty) // Should fail
      assert(logger.issues.exists(_.diagnostic.message.contains("namespace 'test' is duplicated")))
    }
  }

  test("Unpackaged metadata - legacy configuration") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"unpackagedMetadata\": [\"metadata1\", \"metadata2\"]}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.unpackagedMetadata.length == 2)
      assert(project.get.apexConfig.unpackagedMetadata(0).relativePath == "metadata1")
      assert(project.get.apexConfig.unpackagedMetadata(1).relativePath == "metadata2")
    }
  }

  test("Unpackaged metadata - apex-ls precedence") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "unpackagedMetadata": ["legacy"],
            |    "apex-ls": {
            |      "unpackagedMetadata": ["namespaced"]
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.unpackagedMetadata.length == 1)
      assert(
        project.get.apexConfig.unpackagedMetadata(0).relativePath == "namespaced"
      ) // apex-ls wins
    }
  }

  // ForceIgnoreVersion integration tests - verify end-to-end functionality works
  test("ForceIgnoreVersion - default behavior") {
    FileSystemHelper.run(Map("sfdx-project.json" -> "{\"packageDirectories\": []}")) {
      root: PathLike =>
        val project = SFDXProject(root, logger)
        assert(logger.issues.isEmpty)
        assert(project.nonEmpty)
        assert(project.get.apexConfig.options.isEmpty)
        assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V2) // Default value
    }
  }

  test("ForceIgnoreVersion - empty options") {
    FileSystemHelper.run(
      Map("sfdx-project.json" -> "{\"plugins\": {\"options\": {}}, \"packageDirectories\": []}")
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.options.isEmpty)
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V2) // Default value
    }
  }

  test("ForceIgnoreVersion - v1 setting works end-to-end") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v1\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v1"))
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V1) // End-to-end functionality
    }
  }

  test("ForceIgnoreVersion - v2 setting works end-to-end") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v2\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v2"))
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V2) // End-to-end functionality
    }
  }

  test("ForceIgnoreVersion - with multiple options") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" -> "{\"plugins\": {\"options\": {\"forceIgnoreVersion\": \"v1\", \"customOption\": \"value\"}}, \"packageDirectories\": []}"
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(
        project.get.apexConfig.options == Map(
          "forceIgnoreVersion" -> "v1",
          "customOption"       -> "value"
        )
      )
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V1) // End-to-end functionality
    }
  }

  test("ForceIgnoreVersion - invalid value rejected") {
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

  test("ForceIgnoreVersion - non-string value rejected") {
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

  test("ForceIgnoreVersion - apex-ls precedence") {
    FileSystemHelper.run(
      Map(
        "sfdx-project.json" ->
          """{
            |  "plugins": {
            |    "options": {"forceIgnoreVersion": "v1"},
            |    "apex-ls": {
            |      "options": {"forceIgnoreVersion": "v2"}
            |    }
            |  },
            |  "packageDirectories": []
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val project = SFDXProject(root, logger)
      assert(logger.issues.isEmpty)
      assert(project.nonEmpty)
      assert(project.get.apexConfig.options == Map("forceIgnoreVersion" -> "v2"))
      assert(project.get.forceIgnoreVersion == ForceIgnoreVersion.V2) // apex-ls wins
    }
  }
}
