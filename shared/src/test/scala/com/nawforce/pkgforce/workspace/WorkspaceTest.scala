/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.workspace

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents.SourceInfo
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path._
import com.nawforce.pkgforce.stream._
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Path
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq

class WorkspaceTest extends AnyFunSuite with Matchers {

  // Helper to create SFDX project structure with multiple package directories
  private def withMultiPackageSFDXProject(files: Map[String, String]): Map[String, String] = {
    val sfdxProject = """{
      "packageDirectories": [
        {"path": "force-app", "default": true},
        {"path": "sub/force-app"}
      ],
      "sfdcLoginUrl": "https://login.salesforce.com",
      "sourceApiVersion": "48.0"
    }"""
    files + ("sfdx-project.json" -> sfdxProject)
  }

  // Helper methods for common SFDX paths
  private def sfdxPath(root: PathLike, components: String*): PathLike = {
    root.join("force-app" +: "main" +: "default" +: components: _*)
  }

  private def subSfdxPath(root: PathLike, components: String*): PathLike = {
    root.join("sub" +: "force-app" +: "main" +: "default" +: components: _*)
  }

  private def classPath(root: PathLike, className: String): PathLike = {
    sfdxPath(root, "classes", s"$className.cls")
  }

  private def triggerPath(root: PathLike, triggerName: String): PathLike = {
    sfdxPath(root, "triggers", s"$triggerName.trigger")
  }

  private def pagePath(root: PathLike, pageName: String): PathLike = {
    sfdxPath(root, "pages", s"$pageName.page")
  }

  private def componentPath(root: PathLike, componentName: String): PathLike = {
    sfdxPath(root, "components", s"$componentName.component")
  }

  private def flowPath(root: PathLike, flowName: String): PathLike = {
    sfdxPath(root, "flows", s"$flowName.flow")
  }

  private def labelsPath(root: PathLike): PathLike = {
    sfdxPath(root, "labels", "CustomLabels.labels")
  }

  private def objectPath(root: PathLike, objectName: String): PathLike = {
    sfdxPath(root, "objects", objectName, s"$objectName.object-meta.xml")
  }

  private def subObjectPath(root: PathLike, objectName: String): PathLike = {
    subSfdxPath(root, "objects", objectName, s"$objectName.object-meta.xml")
  }

  // Common XML content patterns
  private val emptyCustomObject =
    """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata"/>"""

  private def customObjectWithMetadata(content: String): String = {
    s"""<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
       |$content
       |</CustomObject>
       |""".stripMargin
  }

  private def masterDetailField(referenceTo: String, relationshipName: String): String = {
    s"""  <fields>
       |     <fullName>Lookup__c</fullName>
       |     <type>MasterDetail</type>
       |     <referenceTo>$referenceTo</referenceTo>
       |     <relationshipName>$relationshipName</relationshipName>
       |   </fields>""".stripMargin
  }

  test("Empty dir has no events") {
    FileSystemHelper.run(Map[String, String]()) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      assert(ws.get.events.isEmpty)
    }
  }

  test("Non-existent directory returns error") {
    val nonExistentPath    = Path("/non/existent/directory/path")
    val (ws, issueManager) = Workspace(nonExistentPath)

    assert(ws.isEmpty)
    assert(issueManager.nonEmpty)
    assert(issueManager.hasErrors)

    val diagnostics = issueManager.getDiagnostics(nonExistentPath)
    assert(diagnostics.nonEmpty)
    assert(diagnostics.head.message.contains("No directory at"))
  }

  test("File path instead of directory returns error") {
    FileSystemHelper.run(Map("testfile.txt" -> "content")) { root: PathLike =>
      val filePath           = root.join("testfile.txt")
      val (ws, issueManager) = Workspace(filePath)

      assert(ws.isEmpty)
      assert(issueManager.nonEmpty)
      assert(issueManager.hasErrors)

      val diagnostics = issueManager.getDiagnostics(filePath)
      assert(diagnostics.nonEmpty)
      assert(diagnostics.head.message.contains("No directory at"))
    }
  }

  test("Malformed sfdx-project.json is handled gracefully") {
    FileSystemHelper.run(Map("sfdx-project.json" -> "{ invalid json content")) { root: PathLike =>
      val (ws, issueManager) = Workspace(root)

      // Should fall back to creating workspace without SFDX config
      // The logger should capture any parsing errors
      assert(issueManager.nonEmpty || ws.nonEmpty) // Either errors logged or workspace created
    }
  }

  test("Valid SFDX project with external metadata creates external path filter") {
    FileSystemHelper.run(Map("sfdx-project.json" -> """{
        "packageDirectories": [{"path": "force-app"}],
        "plugins": {
          "externalMetadata": ["vendor", "external"]
        }
      }""")) { root: PathLike =>
      val (ws, issueManager) = Workspace(root)

      assert(ws.nonEmpty)
      assert(issueManager.externalPathFilter.nonEmpty)

      // Test that the filter works correctly
      val filter = issueManager.externalPathFilter.get
      assert(filter(root.join("vendor/some/file.cls")))
      assert(filter(root.join("external/lib/class.cls")))
      assert(!filter(root.join("force-app/main/classes/Test.cls")))
    }
  }

  test("SFDX project without external metadata has no external path filter") {
    FileSystemHelper.run(Map("sfdx-project.json" -> """{
        "packageDirectories": [{"path": "force-app"}]
      }""")) { root: PathLike =>
      val (ws, issueManager) = Workspace(root)

      assert(ws.nonEmpty)
      assert(issueManager.externalPathFilter.isEmpty)
    }
  }

  test("FileSystemHelper auto-creates sfdx-project.json when missing") {
    // Test demonstrates that FileSystemHelper now auto-creates sfdx-project.json
    FileSystemHelper.run(Map("classes/TestClass.cls" -> "public class TestClass {}")) {
      root: PathLike =>
        // Verify sfdx-project.json was automatically created
        assert(root.join("sfdx-project.json").exists)

        val (ws, issueManager) = Workspace(root)

        // Should now succeed due to auto-created sfdx-project.json
        assert(ws.nonEmpty)
        assert(issueManager.isEmpty)
    }
  }

  test("Label file event") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/labels/CustomLabels.labels" -> "<CustomLabels xmlns=\"http://soap.sforce.com/2006/04/metadata\"/>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(LabelFileEvent(SourceInfo(PathLocation(labelsPath, Location.all), _)))
            if labelsPath == root
              .join("force-app")
              .join("main")
              .join("default")
              .join("labels")
              .join("CustomLabels.labels") =>
      }
    }
  }

  test("Label parse error") {
    FileSystemHelper.run(
      Map[String, String]("force-app/main/default/labels/CustomLabels.labels" -> "<CustomLabels")
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              IssuesEvent(
                ArraySeq(Issue(labelsFile, Diagnostic(ERROR_CATEGORY, Location(1, _, 1, _), _), _))
              )
            ) if labelsFile == labelsPath(root) =>
      }
    }
  }

  test("Label events") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/labels/CustomLabels.labels" ->
          """<?xml version="1.0" encoding="UTF-8"?>
          |<CustomLabels xmlns="http://soap.sforce.com/2006/04/metadata">
          |    <labels><fullName>TestLabel1</fullName><protected>false</protected></labels>
          |    <labels><fullName>TestLabel2</fullName><protected>true</protected></labels>
          |</CustomLabels>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)

      ws.get.events.toList should matchPattern {
        case List(
              LabelEvent(
                PathLocation(labelsPath1, Location(3, 0, 3, 0)),
                Name("TestLabel1"),
                false
              ),
              LabelEvent(PathLocation(labelsPath2, Location(4, 0, 4, 0)), Name("TestLabel2"), true),
              LabelFileEvent(SourceInfo(PathLocation(labelsPath3, Location.all), _))
            )
            if labelsPath1 == labelsPath(root) &&
              labelsPath2 == labelsPath1 && labelsPath3 == labelsPath1 =>
      }
    }
  }

  test("Page event") {
    FileSystemHelper.run(
      Map[String, String]("force-app/main/default/pages/MyPage.page" -> "<apex:page/>")
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(PageEvent(SourceInfo(PathLocation(path, Location(1, 0, 1, 17)), _), _, _))
            if path == pagePath(root, "MyPage") =>
      }
    }
  }

  test("Page event with controller") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/pages/MyPage.page" -> "<apex:page controller='MyController'/>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              PageEvent(
                SourceInfo(PathLocation(pagePath, Location(1, 0, 1, 43)), _),
                controllers,
                _
              )
            )
            if pagePath == this.pagePath(root, "MyPage") &&
              controllers == ArraySeq(LocationAnd(Location(1, 11, 1, 36), Name("MyController"))) =>
      }
    }
  }

  test("Page event with controller & extensions") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/pages/MyPage.page" -> "<apex:page controller='MyController' extensions='Ext1, Ext2'/>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              PageEvent(
                SourceInfo(PathLocation(pagePath, Location(1, 0, 1, 67)), _),
                controllers,
                _
              )
            )
            if pagePath == this.pagePath(root, "MyPage") &&
              controllers == ArraySeq(
                LocationAnd(Location(1, 11, 1, 36), Name("MyController")),
                LocationAnd(Location(1, 37, 1, 60), Name("Ext1")),
                LocationAnd(Location(1, 37, 1, 60), Name("Ext2"))
              ) =>
      }
    }
  }

  test("Page event with attribute expressions") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/pages/MyPage.page" -> "<apex:page a = '{!foo}'><a href='{!bar}' other='{!baz}'/></apex:page>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(PageEvent(SourceInfo(PathLocation(pagePath, Location(1, 0, 1, 74)), _), _, exprs))
            if pagePath == this.pagePath(root, "MyPage") &&
              (exprs.toSet == Set(
                LocationAnd(Location(1, 33, 1, 39), "bar"),
                LocationAnd(Location(1, 48, 1, 54), "baz"),
                LocationAnd(Location(1, 16, 1, 22), "foo")
              )) =>
      }
    }
  }

  test("Page event with char data expressions") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/pages/MyPage.page" -> s"<apex:page>{!foo} xx <a> {!bar} </a>{!baz}</apex:page>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(PageEvent(SourceInfo(PathLocation(pagePath, Location(1, 0, 1, 59)), _), _, exprs))
            if pagePath == this.pagePath(root, "MyPage") &&
              (exprs.toSet == Set(
                LocationAnd(Location(1, 24, 1, 32), "bar"),
                LocationAnd(Location(1, 36, 1, 42), "baz"),
                LocationAnd(Location(1, 11, 1, 21), "foo")
              )) =>
      }
    }
  }

  test("Flow event") {
    FileSystemHelper.run(Map[String, String]("force-app/main/default/flows/MyFlow.flow" -> "")) {
      root: PathLike =>
        val (ws, logger) = Workspace(root)
        assert(logger.isEmpty)
        assert(ws.nonEmpty)
        ws.get.events.toList should matchPattern {
          case List(FlowEvent(SourceInfo(PathLocation(flowPath, Location.all), _)))
              if flowPath == this.flowPath(root, "MyFlow") =>
        }
    }
  }

  test("Component event") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/components/MyComponent.component" ->
          """<apex:component>
            |  <apex:attribute name="test" type="String"/>
            |</apex:component>
            |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              ComponentEvent(
                SourceInfo(PathLocation(componentPath, Location(1, 0, 4, 5)), _),
                ArraySeq(Name("test")),
                _,
                _
              )
            ) if componentPath == this.componentPath(root, "MyComponent") =>
      }
    }
  }

  test("Component event with controller") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/components/MyComponent.component" ->
          """<apex:component controller='MyController'>
          |  <apex:attribute name="test" type="String"/>
          |</apex:component>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              ComponentEvent(
                SourceInfo(PathLocation(componentPath, Location(1, 0, 4, 5)), _),
                ArraySeq(Name("test")),
                controllers,
                _
              )
            )
            if componentPath == this.componentPath(root, "MyComponent") &&
              controllers == ArraySeq(LocationAnd(Location(1, 16, 1, 41), Name("MyController"))) =>
      }
    }
  }

  test("Component event with attribute expressions") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/components/MyComponent.component" -> "<apex:component a = '{!foo}'><a href='{!bar}' other='{!baz}'/></apex:component>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              ComponentEvent(
                SourceInfo(PathLocation(componentPath, Location(1, 0, 1, 84)), _),
                _,
                _,
                exprs
              )
            )
            if componentPath == this.componentPath(root, "MyComponent") &&
              (exprs.toSet == Set(
                LocationAnd(Location(1, 38, 1, 44), "bar"),
                LocationAnd(Location(1, 53, 1, 59), "baz"),
                LocationAnd(Location(1, 21, 1, 27), "foo")
              )) =>
      }
    }
  }

  test("Component event with char data expressions") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/components/MyComponent.component" -> s"<apex:component>{!foo} xx <a> {!bar} </a>{!baz}</apex:component>"
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              ComponentEvent(
                SourceInfo(PathLocation(componentPath, Location(1, 0, 1, 69)), _),
                _,
                _,
                exprs
              )
            )
            if componentPath == this.componentPath(root, "MyComponent") &&
              (exprs.toSet == Set(
                LocationAnd(Location(1, 29, 1, 37), "bar"),
                LocationAnd(Location(1, 41, 1, 47), "baz"),
                LocationAnd(Location(1, 16, 1, 26), "foo")
              )) =>
      }
    }
  }

  test("Component parse error") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/components/MyComponent.component" ->
          """<apex:component
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              IssuesEvent(
                ArraySeq(
                  Issue(
                    componentFile,
                    Diagnostic(
                      SYNTAX_CATEGORY,
                      Location(2, 0, 2, 0),
                      "no viable alternative at input '<apex:component'"
                    ),
                    _
                  )
                )
              )
            ) if componentFile == this.componentPath(root, "MyComponent") =>
      }
    }
  }

  test("Component structure error") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/components/MyComponent.component" ->
          """<apex:foo/>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              IssuesEvent(
                ArraySeq(
                  Issue(
                    componentFile,
                    Diagnostic(
                      ERROR_CATEGORY,
                      Location(1, 0, 2, 5),
                      "Root element must be 'apex:component'"
                    ),
                    _
                  )
                )
              )
            ) if componentFile == this.componentPath(root, "MyComponent") =>
      }
    }
  }

  test("Custom Object event") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          emptyCustomObject
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(SObjectEvent(sourceInfo, name, false, None, None))
            if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Custom Object event parse error") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          "<CustomObject xmlns=\"http://soap.sforce.com/2006/04/metadata\""
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(
              IssuesEvent(
                ArraySeq(Issue(objectPath, Diagnostic(ERROR_CATEGORY, Location(1, _, 1, _), _), _))
              )
            )
            if objectPath == root.join(
              "force-app",
              "main",
              "default",
              "objects",
              "MyObject__c",
              "MyObject__c.object-meta.xml"
            ) =>
      }
    }
  }

  test("List Custom Setting") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <customSettingsType>List</customSettingsType>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(SObjectEvent(sourceInfo, name, false, Some(ListCustomSetting), None))
            if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Hierarchical Custom Setting") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <customSettingsType>Hierarchy</customSettingsType>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(SObjectEvent(sourceInfo, name, false, Some(HierarchyCustomSetting), None))
            if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Bad Custom Setting") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <customSettingsType>Bad</customSettingsType>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              IssuesEvent(
                ArraySeq(Issue(objectPath, Diagnostic(ERROR_CATEGORY, Location(1, _, 1, _), _), _))
              )
            )
            if sourceInfo.get.location == location && name == Name(
              "MyObject__c"
            ) && objectPath == path =>
      }
    }
  }

  test("ReadWrite SharingModel") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <sharingModel>ReadWrite</sharingModel>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(SObjectEvent(sourceInfo, name, false, None, Some(ReadWriteSharingModel)))
            if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Bad SharingModel") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |   <sharingModel>Something</sharingModel>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              IssuesEvent(
                ArraySeq(Issue(objectPath, Diagnostic(ERROR_CATEGORY, Location(1, _, 1, _), _), _))
              )
            )
            if sourceInfo.get.location == location && name == Name(
              "MyObject__c"
            ) && objectPath == path =>
      }
    }
  }

  test("Custom Object with fields") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <fields><fullName>Name__c</fullName><type>Text</type></fields>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              CustomFieldEvent(_, Name("Name__c"), Name("Text"), None, None)
            ) if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Custom Object with fieldsset") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <fieldSets><fullName>Name</fullName></fieldSets>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(SObjectEvent(sourceInfo, name, false, None, None), FieldsetEvent(_, Name("Name")))
            if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Custom Object with sharing reason") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <sharingReasons><fullName>Name</fullName></sharingReasons>
          |</CustomObject>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val path = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyObject__c",
        "MyObject__c.object-meta.xml"
      )
      val location = PathLocation(path, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              SharingReasonEvent(_, Name("Name"))
            ) if sourceInfo.get.location == location && name == Name("MyObject__c") =>
      }
    }
  }

  test("Custom Object with fields (sfdx)") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" -> emptyCustomObject,
        "force-app/main/default/objects/MyObject__c/fields/Name__c.field-meta.xml" ->
          s"""<?xml version="1.0" encoding="UTF-8"?>
          |<CustomField xmlns="http://soap.sforce.com/2006/04/metadata">
          |    <fullName>Name__c</fullName>
          |    <type>Text</type>
          |</CustomField>
          |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)

      val objectPath = root.join("force-app", "main", "default", "objects", "MyObject__c")
      val objectLocation =
        PathLocation(objectPath.join("MyObject__c.object-meta.xml"), Location.all)
      val fieldPath     = objectPath.join("fields", "Name__c.field-meta.xml")
      val fieldLocation = PathLocation(fieldPath, Location.all)

      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              CustomFieldEvent(sourceInfoField, Name("Name__c"), Name("Text"), None, None)
            )
            if sourceInfo.get.location == objectLocation &&
              sourceInfoField.location == fieldLocation && name == Name("MyObject__c") =>
      }
    }
  }

  test("Custom Object with fieldSet (sfdx)") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" -> emptyCustomObject,
        "force-app/main/default/objects/MyObject__c/fieldSets/Name.fieldSet-meta.xml" ->
          s"""<?xml version="1.0" encoding="UTF-8"?>
             |<FieldSet xmlns="http://soap.sforce.com/2006/04/metadata">
             |    <fullName>Name</fullName>
             |</FieldSet>
             |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)

      val objectBasePath   = sfdxPath(root, "objects", "MyObject__c")
      val objectLocation   = PathLocation(this.objectPath(root, "MyObject__c"), Location.all)
      val fieldsetPath     = objectBasePath.join("fieldSets", "Name.fieldSet-meta.xml")
      val fieldsetLocation = PathLocation(fieldsetPath, Location.all)

      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              FieldsetEvent(sourceInfoFieldset, Name("Name"))
            )
            if sourceInfo.get.location == objectLocation &&
              sourceInfoFieldset.location == fieldsetLocation
              && name == Name("MyObject__c") =>
      }
    }
  }

  test("Custom Object with sharingReason (sfdx)") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/objects/MyObject__c/MyObject__c.object-meta.xml" -> emptyCustomObject,
        "force-app/main/default/objects/MyObject__c/sharingReasons/Name.sharingReason-meta.xml" ->
          s"""<?xml version="1.0" encoding="UTF-8"?>
             |<SharingReason xmlns="http://soap.sforce.com/2006/04/metadata">
             |    <fullName>Name</fullName>
             |</SharingReason>
             |""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)

      val objectBasePath  = sfdxPath(root, "objects", "MyObject__c")
      val objectLocation  = PathLocation(this.objectPath(root, "MyObject__c"), Location.all)
      val sharingPath     = objectBasePath.join("sharingReasons", "Name.sharingReason-meta.xml")
      val sharingLocation = PathLocation(sharingPath, Location.all)

      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo, name, false, None, None),
              SharingReasonEvent(sourceInfoSharingReason, Name("Name"))
            )
            if sourceInfo.get.location == objectLocation &&
              sourceInfoSharingReason.location == sharingLocation
              && name == Name("MyObject__c") =>
      }
    }
  }

  test("Master/Detail natural ordered") {
    FileSystemHelper.run(
      withMultiPackageSFDXProject(Map[String, String](
        "force-app/main/default/objects/MyMaster__c/MyMaster__c.object-meta.xml" -> customObjectWithMetadata(
          ""
        ),
        "sub/force-app/main/default/objects/MyDetail__c/MyDetail__c.object-meta.xml" ->
          customObjectWithMetadata(masterDetailField("MyMaster__c", "Master"))
      ))
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val masterPath = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyMaster__c",
        "MyMaster__c.object-meta.xml"
      )
      val masterLocation = PathLocation(masterPath, Location.all)
      val detailPath = root.join(
        "sub",
        "force-app",
        "main",
        "default",
        "objects",
        "MyDetail__c",
        "MyDetail__c.object-meta.xml"
      )
      val detailLocation = PathLocation(detailPath, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo1, masterName, false, None, None),
              SObjectEvent(sourceInfo2, detailName, false, None, None),
              CustomFieldEvent(
                _,
                Name("Lookup__c"),
                Name("MasterDetail"),
                Some((Name("MyMaster__c"), Name("Master"))),
                None
              )
            )
            if sourceInfo1.get.location == masterLocation &&
              sourceInfo2.get.location == detailLocation &&
              masterName == Name("MyMaster__c") && detailName == Name("MyDetail__c") =>
      }
    }
  }

  test("Master/Detail reverse ordered") {
    FileSystemHelper.run(
      withMultiPackageSFDXProject(Map[String, String](
        "sub/force-app/main/default/objects/MyMaster__c/MyMaster__c.object-meta.xml" -> customObjectWithMetadata(
          ""
        ),
        "force-app/main/default/objects/MyDetail__c/MyDetail__c.object-meta.xml" ->
          customObjectWithMetadata(masterDetailField("MyMaster__c", "Master"))
      ))
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val masterPath = root.join(
        "sub",
        "force-app",
        "main",
        "default",
        "objects",
        "MyMaster__c",
        "MyMaster__c.object-meta.xml"
      )
      val masterLocation = PathLocation(masterPath, Location.all)
      val detailPath = root.join(
        "force-app",
        "main",
        "default",
        "objects",
        "MyDetail__c",
        "MyDetail__c.object-meta.xml"
      )
      val detailLocation = PathLocation(detailPath, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo2, detailName, false, None, None),
              CustomFieldEvent(
                _,
                Name("Lookup__c"),
                Name("MasterDetail"),
                Some((Name("MyMaster__c"), Name("Master"))),
                None
              ),
              SObjectEvent(sourceInfo1, masterName, false, None, None)
            )
            if sourceInfo1.get.location == masterLocation &&
              sourceInfo2.get.location == detailLocation &&
              masterName == Name("MyMaster__c") && detailName == Name("MyDetail__c") =>
      }
    }
  }

  test("Master/Detail related field") {
    FileSystemHelper.run(
      withMultiPackageSFDXProject(Map[String, String](
        "force-app/main/default/objects/MyMaster__c/MyMaster__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <fields>
            |     <fullName>MyDetailSummary__c</fullName>
            |     <type>Summary</type>
            |     <summarizedField>MyDetail__c.Child__c</summarizedField>
            |   </fields>
            |</CustomObject>
            |""".stripMargin,
        "sub/force-app/main/default/objects/MyDetail__c/MyDetail__c.object-meta.xml" ->
          """<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <fields>
            |     <fullName>Lookup__c</fullName>
            |     <type>MasterDetail</type>
            |     <referenceTo>MyMaster__c</referenceTo>
            |     <relationshipName>Master</relationshipName>
            |   </fields>
            |   <fields>
            |     <fullName>Child__c</fullName>
            |     <type>Number</type>
            |   </fields>
            |</CustomObject>
            |""".stripMargin
      ))
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      val masterPath     = this.objectPath(root, "MyMaster__c")
      val masterLocation = PathLocation(masterPath, Location.all)
      val detailPath     = this.subObjectPath(root, "MyDetail__c")
      val detailLocation = PathLocation(detailPath, Location.all)
      ws.get.events.toList should matchPattern {
        case List(
              SObjectEvent(sourceInfo1, masterName, false, None, None),
              CustomFieldEvent(
                _,
                Name("MyDetailSummary__c"),
                Name("Summary"),
                None,
                Some((Name("MyDetail__c"), Name("Child__c")))
              ),
              SObjectEvent(sourceInfo2, detailName, false, None, None),
              CustomFieldEvent(
                _,
                Name("Lookup__c"),
                Name("MasterDetail"),
                Some((Name("MyMaster__c"), Name("Master"))),
                None
              ),
              CustomFieldEvent(_, Name("Child__c"), Name("Number"), None, None)
            )
            if sourceInfo1.get.location == masterLocation &&
              sourceInfo2.get.location == detailLocation &&
              masterName == Name("MyMaster__c") && detailName == Name("MyDetail__c") =>
      }
    }
  }

  test("Apex event") {
    FileSystemHelper.run(Map[String, String]("force-app/main/default/classes/MyClass.cls" -> "")) {
      root: PathLike =>
        val (ws, logger) = Workspace(root)
        assert(logger.isEmpty)
        assert(ws.nonEmpty)
        ws.get.events.toList should matchPattern {
          case List(ApexEvent(path)) if path == this.classPath(root, "MyClass") =>
        }
    }
  }

  test("Deleted apex event ") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/classes/MyClass.cls" -> "",
        "force-app/main/default/classes/MyClass.cls-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
          |<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
          |  <apiVersion>52.0</apiVersion>
          |  <status>Deleted</status>
          |</ApexClass>""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      assert(ws.get.events.isEmpty)
    }
  }

  test("Trigger event") {
    FileSystemHelper.run(
      Map[String, String]("force-app/main/default/triggers/MyTrigger.trigger" -> "")
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      ws.get.events.toList should matchPattern {
        case List(TriggerEvent(triggerPath))
            if triggerPath == this.triggerPath(root, "MyTrigger") =>
      }
    }
  }

  test("Deleted trigger event ") {
    FileSystemHelper.run(
      Map[String, String](
        "force-app/main/default/triggers/MyTrigger.trigger" -> "",
        "force-app/main/default/triggers/MyTrigger.trigger-meta.xml" ->
          """<?xml version="1.0" encoding="UTF-8"?>
            |<ApexClass xmlns="http://soap.sforce.com/2006/04/metadata">
            |  <apiVersion>52.0</apiVersion>
            |  <status>Deleted</status>
            |</ApexClass>""".stripMargin
      )
    ) { root: PathLike =>
      val (ws, logger) = Workspace(root)
      assert(logger.isEmpty)
      assert(ws.nonEmpty)
      assert(ws.get.events.isEmpty)
    }
  }

}
