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
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.diagnostics.IssuesManager
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class DocumentIndexTest extends AnyFunSuite with BeforeAndAfter {

  private var logger: IssuesManager = _

  before {
    logger = new IssuesManager()
  }

  test("bad dir has no files") {
    FileSystemHelper.run(Map[String, String]()) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("foo"))
      assert(logger.isEmpty)
      assert(index.size == 0)
    }
  }

  test("empty dir has no files") {
    FileSystemHelper.run(Map[String, String]()) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root)
      assert(logger.isEmpty)
      assert(index.size == 0)
    }
  }

  test("dot dir is ignored") {
    FileSystemHelper.run(Map[String, String](".pkg/Foo.cls" -> "")) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root)
      assert(logger.isEmpty)
      assert(index.size == 0)
    }
  }

  test("node_modules dir is ignored") {
    FileSystemHelper.run(Map[String, String]("node_modules/Foo.cls" -> "")) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root)
      assert(logger.isEmpty)
      assert(index.size == 0)
    }
  }

  test("class file found") {
    FileSystemHelper.run(Map[String, String]("pkg/Foo.cls" -> "public class Foo {}")) {
      root: PathLike =>
        val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
        assert(logger.isEmpty)
        assert(index.get(ApexNature).size == 1)
        assert(
          index.getControllingDocuments(ApexNature) ==
            List(ApexClassDocument(root.join("pkg").join("Foo.cls"), Name("Foo")))
        )
    }
  }

  test("nested class file found") {
    FileSystemHelper.run(Map[String, String]("pkg/foo/Foo.cls" -> "public class Foo {}")) {
      root: PathLike =>
        val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
        assert(logger.isEmpty)
        assert(index.get(ApexNature).size == 1)
        assert(
          index.getControllingDocuments(ApexNature) ==
            List(ApexClassDocument(root.join("pkg").join("foo").join("Foo.cls"), Name("Foo")))
        )
    }
  }

  test("multiple classes found") {
    FileSystemHelper.run(
      Map[String, String](
        "/pkg/Foo.cls"     -> "public class Foo {}",
        "/pkg/bar/Bar.cls" -> "public class Bar {}"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(
        index.getControllingDocuments(ApexNature).map(_.toString()).toSet == Set(
          ApexClassDocument(root.join("pkg").join("Foo.cls"), Name("Foo")).toString,
          ApexClassDocument(root.join("pkg").join("bar").join("Bar.cls"), Name("Bar")).toString
        )
      )
    }
  }

  test("duplicate classes error") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/foo/Foo.cls"  -> "public class Foo {}",
        "pkg/bar/Foo.cls" -> "public class Foo {}"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(index.get(ApexNature).size == 1)
      val issues = logger.issuesForFiles(null, includeWarnings = false, 10)
      assert(issues.length == 1)
      assert(
        issues.head.toString.contains("Error: line 1: Duplicate for type 'Foo' found in ")
      )
    }
  }

  test("duplicate labels no error") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/foo/CustomLabels.labels" -> "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>",
        "/pkg/bar/CustomLabels.labels" -> "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(index.getControllingDocuments(LabelNature).size == 2)
    }
  }

  test("standard object file found and correctly named after file") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/objects/Foo.object" -> "<CustomObject xmlns=\\\"http://soap.sforce.com/2006/04/metadata\\\"/>"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(
        index.getControllingDocuments(SObjectNature) ==
          List(SObjectDocument(root.join("pkg", "objects", "Foo.object"), Name("Foo")))
      )
    }
  }

  test("standard object file found and correctly named after dir (sfdx)") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/objects/Bar/Foo.object-meta.xml" -> "<CustomObject xmlns=\\\"http://soap.sforce.com/2006/04/metadata\\\"/>"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(
        index.getControllingDocuments(SObjectNature) ==
          List(
            SObjectDocument(root.join("pkg", "objects", "Bar", "Foo.object-meta.xml"), Name("Bar"))
          )
      )
    }
  }

  test("custom object file found and correctly named after file") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/objects/Foo__c.object" -> "<CustomObject xmlns=\\\"http://soap.sforce.com/2006/04/metadata\\\"/>"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(
        index.getControllingDocuments(SObjectNature) ==
          List(SObjectDocument(root.join("pkg", "objects", "Foo__c.object"), Name("Foo__c")))
      )
    }
  }

  test("custom object file found and correctly named after dir (sfdx)") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/objects/Bar__c/Foo__c.object-meta.xml" -> "<CustomObject xmlns=\\\"http://soap.sforce.com/2006/04/metadata\\\"/>"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(
        index.getControllingDocuments(SObjectNature) ==
          List(
            SObjectDocument(
              root.join("pkg", "objects", "Bar__c", "Foo__c.object-meta.xml"),
              Name("Bar__c")
            )
          )
      )
    }
  }

  test("object file not in objects dir ignored") {
    FileSystemHelper.run(
      Map[String, String](
        "pkg/Bar/Foo.object" -> "<CustomObject xmlns=\\\"http://soap.sforce.com/2006/04/metadata\\\"/>"
      )
    ) { root: PathLike =>
      val index = DocumentIndex(logger, None, isGulped = false, root.join("pkg"))
      assert(logger.isEmpty)
      assert(index.getControllingDocuments(SObjectNature).isEmpty)
    }
  }

}
