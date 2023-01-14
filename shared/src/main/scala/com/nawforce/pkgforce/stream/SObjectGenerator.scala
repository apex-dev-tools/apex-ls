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

package com.nawforce.pkgforce.stream

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.names.{DotName, Name}
import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation}
import com.nawforce.pkgforce.xml.{XMLDocumentLike, XMLElementLike, XMLException, XMLFactory}
import com.nawforce.runtime.xml.XMLDocument

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

sealed abstract class SharingModel(val value: String)
case object PrivateSharingModel              extends SharingModel("Private")
case object ReadSharingModel                 extends SharingModel("Read")
case object ReadWriteSharingModel            extends SharingModel("ReadWrite")
case object ReadWriteTransferSharingModel    extends SharingModel("ReadWriteTransfer")
case object FullAccessSharingModel           extends SharingModel("FullAccess")
case object ControlledByParentSharingModel   extends SharingModel("ControlledByParent")
case object ControlledByCampaignSharingModel extends SharingModel("ControlledByCampaign")
case object ControlledByLeadOrContractSharingModel
    extends SharingModel("ControlledByLeadOrContract")

sealed trait CustomSettingType
case object ListCustomSetting      extends CustomSettingType
case object HierarchyCustomSetting extends CustomSettingType

final case class SObjectEvent(
  sourceInfo: Option[SourceInfo],
  // TODO: This might not be right due to multiple dirs
  reportingPath: PathLike, // SFDX SObject directory or MDAPI .object file
  isDefining: Boolean,     // Metadata is defining a new SObject
  customSettingsType: Option[CustomSettingType],
  sharingModel: Option[SharingModel]
) extends PackageEvent
final case class CustomFieldEvent(
  sourceInfo: SourceInfo,
  name: Name,
  rawType: Name,
  referenceTo: Option[(Name, Name)],
  relatedField: Option[(Name, Name)]
) extends PackageEvent
final case class FieldsetEvent(sourceInfo: SourceInfo, name: Name)      extends PackageEvent
final case class SharingReasonEvent(sourceInfo: SourceInfo, name: Name) extends PackageEvent

/** Convert SObject documents/folders into PackageEvents. We must call this even if there is not
  * object-meta.xml file present to collect the SFDX fields, fieldSets and sharingRules.
  */
object SObjectGenerator {

  def iterator(index: DocumentIndex): Iterator[PackageEvent] = {
    // Convert SObjectLike things to events
    val sObjectEvents: mutable.Map[Name, Array[PackageEvent]] =
      index
        .get(SObjectNature)
        .map(docInfo => (Name(docInfo._1), toEvents(docInfo._2).toArray))
        .to(mutable.Map)

    // SObjects need ordering so lookup target is output before the object using lookup
    val emitted = new mutable.HashSet[Name]()
    val output  = new mutable.ArrayBuffer[PackageEvent]()

    var found = true
    while (found && sObjectEvents.nonEmpty) {
      found = false
      sObjectEvents.foreach(kv => {
        val depends = kv._2
          .collect { case CustomFieldEvent(_, _, _, Some((referenceTo, _)), _) =>
            Name(s"schema.$referenceTo")
          }
          .filter(sObjectEvents.contains)
        if (depends.forall(d => emitted.contains(d))) {
          sObjectEvents.remove(kv._1)
          emitted.add(kv._1)
          output.appendAll(kv._2)
          found = true
        }
      })
    }

    // If ordering failed, apply any left to end, this will fail on deploy
    sObjectEvents.foreach(kv => output.appendAll(kv._2))
    output.iterator
  }

  private def toEvents(docs: List[PathLike]): Iterator[PackageEvent] = {
    val documents = docs.flatMap(MetadataDocument(_))

    // Parse controlling doc, if we have one
    val controllingDoc  = documents.find(_.nature == SObjectNature)
    val controllingPath = controllingDoc.map(_.path)
    val sourceData      = controllingPath.flatMap(_.readSourceData().toOption)
    val sourceInfo =
      sourceData.map(source => SourceInfo(PathLocation(controllingPath.get, Location.all), source))
    val parsed = sourceData.map(source => XMLDocument(controllingPath.get, source))
    if (parsed.nonEmpty && parsed.get.issues.nonEmpty)
      return IssuesEvent.iterator(parsed.get.issues)

    // Extract some needed info
    val controllingContent = parsed.flatMap(_.value)
    val customSettingsType =
      controllingContent
        .map(content => extractCustomSettingsType(content))
        .getOrElse(IssuesAnd(None))
    val sharingModelType =
      controllingContent.map(content => extractSharingModel(content)).getOrElse(IssuesAnd(None))
    val isDefining =
      controllingContent.exists(content => content.rootElement.getChildren("label").nonEmpty)

    // Collect whatever we can find into the stream, this is deliberately lax we are not trying to find errors here
    Iterator(
      SObjectEvent(
        sourceInfo,
        getReportingPath(controllingPath, documents),
        isDefining,
        customSettingsType.value,
        sharingModelType.value
      )
    ) ++
      IssuesEvent.iterator(customSettingsType.issues) ++
      IssuesEvent.iterator(sharingModelType.issues) ++
      controllingContent
        .map(content => {
          val rootElement = content.rootElement
          rootElement
            .getChildren("fields")
            .flatMap(field => {
              createField(
                SourceInfo(PathLocation(controllingPath.get, Location(field.line)), sourceData.get),
                field,
                controllingPath.get
              )
            }) ++
            rootElement
              .getChildren("fieldSets")
              .flatMap(fieldSet => {
                createFieldSet(
                  SourceInfo(
                    PathLocation(controllingPath.get, Location(fieldSet.line)),
                    sourceData.get
                  ),
                  fieldSet,
                  controllingPath.get
                )
              }) ++
            rootElement
              .getChildren("sharingReasons")
              .flatMap(sharingReason => {
                createSharingReason(
                  SourceInfo(
                    PathLocation(controllingPath.get, Location(sharingReason.line)),
                    sourceData.get
                  ),
                  sharingReason,
                  controllingPath.get
                )
              })
        })
        .getOrElse(Iterator()) ++
      collectMetadata(documents, FieldNature, "CustomField", createField).iterator ++
      collectMetadata(documents, FieldSetNature, "FieldSet", createFieldSet).iterator ++
      collectMetadata(documents, SharingReasonNature, "SharingReason", createSharingReason).iterator
  }

  private def getReportingPath(
    controllingPath: Option[PathLike],
    documents: List[MetadataDocument]
  ): PathLike = {
    if (controllingPath.exists(p => p.toString.endsWith(".object")))
      controllingPath.get
    else if (documents.head.nature == SObjectNature)
      documents.head.path.parent
    else
      documents.head.path.parent.parent
  }

  private def extractCustomSettingsType(
    doc: XMLDocumentLike
  ): IssuesAnd[Option[CustomSettingType]] = {
    doc.rootElement.getOptionalSingleChildAsString("customSettingsType") match {
      case Some("List")      => IssuesAnd(Some(ListCustomSetting))
      case Some("Hierarchy") => IssuesAnd(Some(HierarchyCustomSetting))
      case Some(x) =>
        IssuesAnd(
          ArraySeq(
            Issue(
              doc.path,
              ERROR_CATEGORY,
              Location(doc.rootElement.line),
              s"Unexpected customSettingsType value '$x', should be 'List' or 'Hierarchy'"
            )
          ),
          None
        )
      case None => IssuesAnd(None)
    }
  }

  private val allSharingModels = Seq(
    PrivateSharingModel,
    ReadSharingModel,
    ReadWriteSharingModel,
    ReadWriteTransferSharingModel,
    FullAccessSharingModel,
    ControlledByParentSharingModel,
    ControlledByCampaignSharingModel,
    ControlledByLeadOrContractSharingModel
  )

  private def extractSharingModel(doc: XMLDocumentLike): IssuesAnd[Option[SharingModel]] = {
    val sharingModel = doc.rootElement.getOptionalSingleChildAsString("sharingModel")
    if (sharingModel.nonEmpty) {
      val matched = allSharingModels.find(_.value == sharingModel.get)
      if (matched.nonEmpty) {
        IssuesAnd(matched)
      } else {
        IssuesAnd(
          ArraySeq(
            Issue(
              doc.path,
              ERROR_CATEGORY,
              Location(doc.rootElement.line),
              s"Unexpected sharingModel value '${sharingModel.get}'"
            )
          ),
          None
        )
      }
    } else {
      IssuesAnd(None)
    }
  }

  private def createField(
    sourceInfo: SourceInfo,
    elem: XMLElementLike,
    path: PathLike
  ): Iterator[PackageEvent] = {
    catchXMLExceptions(path) {
      val name = Name(elem.getSingleChildAsString("fullName").trim)

      // We only need custom fields
      if (!name.toString.endsWith("__c"))
        return Iterator()

      val rawType = elem.getSingleChildAsString("type").trim
      if (!fieldTypes.contains(rawType)) {
        return IssuesEvent.iterator(
          ArraySeq(
            Issue(
              path,
              Diagnostic(
                ERROR_CATEGORY,
                Location(elem.line),
                s"Unrecognised type '$rawType' on custom field '$name'"
              )
            )
          )
        )
      }

      // Create additional fields & lookup relationships for special fields
      val target = rawType match {
        case "Lookup" | "MasterDetail" | "MetadataRelationship" =>
          Some(
            (
              Name(elem.getSingleChildAsString("referenceTo").trim),
              Name(elem.getSingleChildAsString("relationshipName").trim)
            )
          )
        case _ => None
      }

      // Child relationship field references
      val related = rawType match {
        case "Summary" =>
          elem
            .getOptionalSingleChildAsString("summarizedField")
            .map(fieldStr => DotName(fieldStr.trim))
            .map(field => (field.firstName, field.lastName))
        case _ => None
      }

      Iterator(CustomFieldEvent(sourceInfo, name, Name(rawType), target, related))
    }
  }

  private def createFieldSet(
    sourceInfo: SourceInfo,
    elem: XMLElementLike,
    path: PathLike
  ): Iterator[PackageEvent] = {
    catchXMLExceptions(path) {
      Iterator(FieldsetEvent(sourceInfo, Name(elem.getSingleChildAsString("fullName"))))
    }
  }

  private def createSharingReason(
    sourceInfo: SourceInfo,
    elem: XMLElementLike,
    path: PathLike
  ): Iterator[PackageEvent] = {
    catchXMLExceptions(path) {
      Iterator(SharingReasonEvent(sourceInfo, Name(elem.getSingleChildAsString("fullName"))))
    }
  }

  // TODO: Use doc nature
  private def collectMetadata(
    docs: List[MetadataDocument],
    nature: MetadataNature,
    rootElement: String,
    op: (SourceInfo, XMLElementLike, PathLike) => Iterator[PackageEvent]
  ): List[PackageEvent] = {
    docs
      .filter(_.nature == nature)
      .flatMap(doc => {
        catchXMLExceptions(doc.path) {
          doc.path.readSourceData() match {
            case Left(err) =>
              IssuesEvent
                .iterator(ArraySeq(Issue(doc.path, Diagnostic(ERROR_CATEGORY, Location(0), err))))
            case Right(sourceData) =>
              XMLFactory.parse(doc.path) match {
                case IssuesAnd(issues, content) if content.isEmpty => IssuesEvent.iterator(issues)
                case IssuesAnd(_, content) =>
                  content.get.rootElement.checkIsOrThrow(rootElement)
                  op(
                    SourceInfo(PathLocation(doc.path, Location.all), sourceData),
                    content.get.rootElement,
                    doc.path
                  )
              }
          }
        }
      })
  }

  private def catchXMLExceptions(
    path: PathLike
  )(op: => Iterator[PackageEvent]): Iterator[PackageEvent] = {
    try {
      op
    } catch {
      case e: XMLException =>
        IssuesEvent.iterator(ArraySeq(Issue(path, Diagnostic(ERROR_CATEGORY, e.where, e.msg))))
    }
  }

  private val fieldTypes = Set[String](
    "MasterDetail",
    "Lookup",
    "MetadataRelationship",
    "AutoNumber",
    "Checkbox",
    "Currency",
    "Date",
    "DateTime",
    "Email",
    "EncryptedText",
    "Number",
    "Percent",
    "Phone",
    "Picklist",
    "MultiselectPicklist",
    "Summary",
    "Text",
    "TextArea",
    "LongTextArea",
    "Url",
    "File",
    "Location",
    "Time",
    "Html"
  )
}
