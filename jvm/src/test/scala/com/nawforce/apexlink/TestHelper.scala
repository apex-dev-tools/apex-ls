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
package com.nawforce.apexlink

import com.nawforce.apexlink.TestHelper.{CURSOR, locToString}
import com.nawforce.apexlink.api.{AnalysisMode, Org, ServerOps, TypeSummary}
import com.nawforce.apexlink.org.{OPM, OrgInfo}
import com.nawforce.apexlink.plugins.{PluginsManager, UnusedPlugin}
import com.nawforce.apexlink.rpc.{LocationLink, TargetLocation}
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexFullDeclaration, FullDeclaration}
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.apexlink.types.schema.SObjectDeclaration
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Path

import scala.collection.immutable.ArraySeq

trait TestHelper {

  private var defaultOrg: OPM.OrgImpl = _
  protected var root: PathLike        = _

  def createOrg(path: PathLike): OPM.OrgImpl = {
    val plugins = PluginsManager.overridePlugins(Seq())
    try {
      ParserHelper.setParser()
      defaultOrg = Org.newOrg(path).asInstanceOf[OPM.OrgImpl]
      defaultOrg
    } finally {
      PluginsManager.overridePlugins(plugins)
    }
  }

  def createOrgWithUnused(path: PathLike): OPM.OrgImpl = {
    val plugins = PluginsManager.overridePlugins(Seq(classOf[UnusedPlugin]))
    try {
      ParserHelper.setParser()
      defaultOrg = Org.newOrg(path).asInstanceOf[OPM.OrgImpl]
      defaultOrg
    } finally {
      PluginsManager.overridePlugins(plugins)
    }
  }

  def createHappyOrg(path: PathLike): OPM.OrgImpl = {
    createOrg(path)
    val messages = getMessages()
    if (messages.nonEmpty)
      println(getMessages())
    assert(!hasIssues)
    defaultOrg
  }

  def emptyOrg(): OPM.OrgImpl = {
    FileSystemHelper.run(Map[String, String]()) { root: PathLike =>
      this.root = root
      createOrg(root)
    }
  }

  def withOrg[T](op: OPM.OrgImpl => T): T = {
    OrgInfo.current.withValue(defaultOrg) {
      op(defaultOrg)
    }
  }

  def withManualFlush[T](op: => T): T = {
    val current = ServerOps.setAutoFlush(false)
    try {
      op
    } finally {
      ServerOps.setAutoFlush(current)
    }
  }

  def withExternalAnalysis[T](mode: AnalysisMode)(op: => T): T = {
    val current = ServerOps.setExternalAnalysisMode(mode)
    try {
      op
    } finally {
      ServerOps.setExternalAnalysisMode(current)
    }
  }

  def withEmptyOrg[T](op: OPM.OrgImpl => T): T = {
    val org = emptyOrg()
    OrgInfo.current.withValue(org) {
      op(org)
    }
  }

  def typeDeclarations(classes: Map[String, String]): Seq[ApexClassDeclaration] = {
    try {
      ServerOps.setAutoFlush(false)
      FileSystemHelper.run(classes) { root: PathLike =>
        this.root = root
        createOrg(root)
        classes.keys
          .map(cls => unmanagedClass(cls.replace(".cls", "")).get)
          .toSeq
      }
    } finally {
      ServerOps.setAutoFlush(true)
    }
  }

  def happyTypeDeclaration(clsText: String): ApexClassDeclaration = {
    val td = typeDeclarations(Map("Dummy.cls" -> clsText)).headOption
    assert(td.nonEmpty)
    assert(!hasIssues)
    td.get
  }

  def typeDeclaration(clsText: String): ApexClassDeclaration = {
    typeDeclarations(Map("Dummy.cls" -> clsText)).head
  }

  def typeDeclarationInner(clsText: String): TypeDeclaration = {
    typeDeclaration(clsText).nestedTypes.head
  }

  def classSummary(text: String, hasMessages: Boolean = false): TypeSummary = {
    val td = typeDeclaration(text)
    assert(hasIssues == hasMessages)
    td.asInstanceOf[FullDeclaration].summary
  }

  def triggerDeclaration(text: String): TypeDeclaration = {
    try {
      ServerOps.setAutoFlush(false)
      FileSystemHelper.run(Map("Dummy.trigger" -> text)) { root: PathLike =>
        this.root = root
        createOrg(root)
        unmanagedType(TypeName(Name("__sfdc_trigger/Dummy"))).get
      }
    } finally {
      ServerOps.setAutoFlush(true)
    }
  }

  def triggerSummary(text: String, hasMessages: Boolean = false): TypeSummary = {
    val td = triggerDeclaration(text)
    assert(hasIssues == hasMessages)
    td.asInstanceOf[ApexFullDeclaration].summary
  }

  def unmanagedType(typeName: TypeName): Option[TypeDeclaration] = {
    defaultOrg.unmanaged.orderedModules.head.findModuleType(typeName)
  }

  def unmanagedClass(name: String): Option[ApexClassDeclaration] = {
    unmanagedType(TypeName(Name(name))).map(_.asInstanceOf[ApexClassDeclaration])
  }

  def unmanagedSObject(name: String): Option[SObjectDeclaration] = {
    unmanagedType(TypeName(Name(name), Nil, Some(TypeName(Names.Schema))))
      .map(_.asInstanceOf[SObjectDeclaration])
  }

  def packagedType(namespace: Some[Name], typeName: TypeName): Option[TypeDeclaration] = {
    defaultOrg.packagesByNamespace(namespace).orderedModules.head.findModuleType(typeName)
  }

  def packagedType(namespace: Name, typeName: TypeName): Option[TypeDeclaration] = {
    packagedType(Some(namespace), typeName)
  }

  def packagedCustomType(namespace: String, name: String): Option[TypeDeclaration] = {
    packagedType(
      Some(Name(namespace)),
      TypeName(Name(name), Seq(), Some(TypeName(Name(namespace))))
    )
  }

  def packagedClass(namespace: String, name: String): Option[ApexClassDeclaration] = {
    packagedCustomType(namespace, name).map(_.asInstanceOf[ApexClassDeclaration])
  }

  def packagedSObject(namespace: String, name: String): Option[SObjectDeclaration] = {
    packagedType(Name(namespace), TypeName(Name(name), Nil, Some(TypeName(Names.Schema))))
      .map(_.asInstanceOf[SObjectDeclaration])
  }

  def hasIssues: Boolean = defaultOrg.issues.nonEmpty

  def getMessages(org: OPM.OrgImpl = defaultOrg): String = {
    val messages = org.issues
      .issuesForFilesInternal(paths = null, includeWarnings = true, maxIssuesPerFile = 10)
      .mkString("\n")
    // For backward compatability with earlier behaviour
    if (messages.nonEmpty) messages + "\n" else ""
  }

  def getMessages(path: PathLike): String = {
    val messages = defaultOrg.issues
      .issuesForFileInternal(path)
      .map(_.asString())
      .mkString("\n")
    // For backward compatibility with earlier behaviour
    if (messages.nonEmpty) messages + "\n" else ""
  }

  def dummyIssues: String = getMessages(root.join("Dummy.cls"))

  def createDirectories(dir: PathLike, names: String*): PathLike = {
    if (names.isEmpty) {
      dir
    } else {
      dir.createDirectory(names.head) match {
        case Left(err)   => assert(false, err); Path("")
        case Right(path) => createDirectories(path, names.tail: _*)
      }
    }
  }

  def createFile(dir: PathLike, name: String, content: String, subDirs: String*): PathLike = {
    createDirectories(dir, subDirs: _*).createFile(name, content).toOption.get
  }

  def createFiles(dir: PathLike, files: Map[String, String]): List[PathLike] = {
    files
      .map(pathAndContent => {
        val parts = ArraySeq.unsafeWrapArray(pathAndContent._1.split('/'))
        createFile(dir, parts.last, pathAndContent._2, parts.take(parts.length - 1): _*)
      })
      .toList
  }

  def customObject(
    label: String,
    fields: Seq[(String, Option[String], Option[String])],
    fieldSets: Set[String] = Set(),
    sharingReason: Set[String] = Set(),
    sharingModel: String = "",
    extending: Boolean = false
  ): String = {
    val fieldMetadata = fields.map(field => {
      s"""
         |    <fields>
         |        <fullName>${field._1}</fullName>
         |        ${if (field._2.nonEmpty) s"<type>${field._2.get}</type>" else ""}
         |        ${if (field._3.nonEmpty) s"<referenceTo>${field._3.get}</referenceTo>" else ""}
         |        ${if (field._3.nonEmpty)
          s"<relationshipName>${field._1.replaceAll("__c$", "")}</relationshipName>"
        else ""}
         |    </fields>
         |""".stripMargin
    })

    val fieldSetMetadata = fieldSets.map(fieldSet => {
      s"""
         |    <fieldSets>
         |        <fullName>$fieldSet</fullName>
         |    </fieldSets>
         |""".stripMargin
    })

    val sharingReasonMetadata = sharingReason.map(sharingReason => {
      s"""
         |    <sharingReasons>
         |        <fullName>$sharingReason</fullName>
         |    </sharingReasons>
         |""".stripMargin
    })

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<CustomObject xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$label</fullName>
       |    ${if (!extending) "<label/>" else ""}
       |    ${if (!extending) "<pluralLabel/>" else ""}
       |    ${if (!extending) "<nameField/>" else ""}
       |    ${if (!extending) "<deploymentStatus/>" else ""}
       |    ${if (sharingModel.nonEmpty) s"<sharingModel>$sharingModel</sharingModel>" else ""}
       |    $fieldMetadata
       |    $fieldSetMetadata
       |    $sharingReasonMetadata
       |</CustomObject>
       |""".stripMargin
  }

  def customField(
    name: String,
    fieldType: String,
    relationshipName: Option[String],
    xml: Option[String] = None
  ): String = {
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<CustomField xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$name</fullName>
       |    <type>$fieldType</type>
       |    ${if (relationshipName.nonEmpty) s"<referenceTo>${relationshipName.get}</referenceTo>"
      else ""}
       |    ${if (relationshipName.nonEmpty)
        s"<relationshipName>${name.replaceAll("__c$", "")}</relationshipName>"
      else ""}
       |    ${if (xml.nonEmpty) s"$xml"}
       |</CustomField>
       |""".stripMargin
  }

  def customFieldSet(name: String): String = {
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<FieldSet xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$name</fullName>
       |</FieldSet>
       |""".stripMargin
  }

  def customSharingReason(name: String): String = {
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<SharingReason xmlns="http://soap.sforce.com/2006/04/metadata">
       |    <fullName>$name</fullName>
       |</SharingReason>
       |""".stripMargin
  }

  def withCursor(content: String): (String, Int) = {
    (content.replace(CURSOR, ""), content.indexOf(CURSOR))
  }

  def withCursorMultiLine(content: String): (String, CursorPos) = {
    val lineWithCursor = content.split('\n').find(contentLine => contentLine.contains(CURSOR)).get
    val line           = content.split('\n').indexOf(lineWithCursor) + 1
    val offset         = lineWithCursor.indexOf(CURSOR)

    (content.replace(CURSOR, ""), CursorPos(line, offset))
  }

  case class CursorPos(line: Int, offset: Int)
}

object TestHelper {
  final val CURSOR = "$"
  def locToString(source: String, loc: Location): String = {
    val lines = source.linesWithSeparators.toArray
    if (loc.startLine == loc.endLine) {
      return lines(loc.startLine - 1).substring(loc.startCharOffset(), loc.endCharOffset())
    }
    lines(loc.startLine - 1).substring(loc.startPosition, lines(loc.startLine - 1).length) + lines(
      loc.endLine - 1
    ).substring(0, loc.endPosition)
  }
}

case class TargetLocationString(targetPath: String, target: String)
object TargetLocationString {
  def apply(root: PathLike, loc: TargetLocation): TargetLocationString = {
    val source = root.join(loc.targetPath).readSourceData().map(_.asString).toOption.get
    TargetLocationString(loc.targetPath, locToString(source, loc.range))
  }
}

case class LocationLinkString(
  origin: String,
  targetPath: String,
  target: String,
  targetSelection: String
)

object LocationLinkString {

  def apply(
    root: PathLike,
    source: String,
    target: String,
    loc: LocationLink
  ): LocationLinkString = {
    LocationLinkString(
      locToString(source, loc.origin),
      root.join(loc.targetPath).toString,
      locToString(target, loc.target),
      locToString(target, loc.targetSelection)
    )
  }

  def apply(root: PathLike, source: String, loc: LocationLink): LocationLinkString = {
    val target = root.join(loc.targetPath).readSourceData().map(_.asString).toOption.get
    apply(root, source, target, loc)
  }

  def apply(root: PathLike, sourceClsName: PathLike, loc: LocationLink): LocationLinkString = {
    val source = sourceClsName.readSourceData().map(_.asString).toOption.get
    apply(root, source, loc)
  }
}
