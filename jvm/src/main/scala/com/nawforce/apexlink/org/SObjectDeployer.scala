/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.org

import com.nawforce.apexlink.diagnostics.IssueOps
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.names.XNames.NameUtils
import com.nawforce.apexlink.names.{TypeNames, XNames}
import com.nawforce.apexlink.org.SObjectDeployer.{feedFieldsFor, historyFieldsFor, shareFieldsFor}
import com.nawforce.apexlink.types.core.{FieldDeclaration, TypeDeclaration}
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.apexlink.types.schema.{SObjectNature, _}
import com.nawforce.apexlink.types.synthetic.{
  CustomFieldDeclaration,
  LocatableCustomFieldDeclaration
}
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.names._
import com.nawforce.pkgforce.stream._

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.{BufferedIterator, mutable}

/** 'Deploy' a module from a stream of PackageEvents. Deploying here really means constructing a set
  * of TypeDeclarations and validating them against each other. This process mutates the passed
  * types Map for compatibility with dependency analysis code. The class handling code here is
  * performance sensitive so this may also aid with efficiency.
  *
  * FUTURE: Remove Module dependency.
  */
class SObjectDeployer(module: OPM.Module) {

  def createSObjects(events: BufferedIterator[PackageEvent]): Array[SObjectLikeDeclaration] = {
    val objectsEvents = bufferEvents(
      Set(
        classOf[SObjectEvent],
        classOf[CustomFieldEvent],
        classOf[FieldsetEvent],
        classOf[SharingReasonEvent]
      ),
      events
    ).iterator.buffered

    val createdSObjects = mutable.Map[TypeName, SObjectLikeDeclaration]()
    val derivedFields   = ArrayBuffer[(TypeName, CustomFieldEvent)]()

    while (objectsEvents.hasNext) {
      val sObjectEvent = objectsEvents.next().asInstanceOf[SObjectEvent]
      val encodedName  = EncodedName(sObjectEvent.name).defaultNamespace(module.namespace)
      val typeName     = TypeName(encodedName.fullName, Nil, Some(TypeNames.Schema))
      val nature       = SObjectNature(sObjectEvent.name, sObjectEvent)

      val fieldEvents         = bufferEvents[CustomFieldEvent](objectsEvents)
      val fieldSetEvents      = bufferEvents[FieldsetEvent](objectsEvents)
      val sharingReasonEvents = bufferEvents[SharingReasonEvent](objectsEvents)
      val fieldSets           = fieldSetEvents.map(_.name)
      val sharingReasons      = sharingReasonEvents.map(_.name)

      val sources: Array[SourceInfo] =
        sObjectEvent.sourceInfo.toArray ++
          fieldEvents.map(_.sourceInfo) ++
          fieldSetEvents.map(_.sourceInfo) ++
          sharingReasonEvents.map(_.sourceInfo)

      val (derived, nonDerived) = fieldEvents.partitionMap {
        case e if SObjectDeployer.derivedFieldTypes.contains(e.rawType) =>
          Left((typeName, e))
        case f => Right(f)
      }

      derivedFields.addAll(derived)
      val fields = nonDerived
        .filter(f => module.isGulped || isCustomField(f))
        .flatMap(createCustomField)

      val sobjects =
        if (encodedName.ext.nonEmpty)
          createCustomObject(
            sources,
            sObjectEvent,
            typeName,
            nature,
            fields,
            fieldSets,
            sharingReasons
          )
        else
          createReplacementSObject(sources, typeName, nature, fields, fieldSets, sharingReasons)
      sobjects.foreach(sobject => createdSObjects.put(sobject.typeName, sobject))
    }

    addDerivedFieldsToObjects(derivedFields, createdSObjects)

    createdSObjects.values.toArray
  }

  private def isCustomField(field: CustomFieldEvent): Boolean = {
    field.name.toString.endsWith("__c")
  }

  private def createCustomField(field: CustomFieldEvent): Array[FieldDeclaration] = {
    val name         = defaultNamespace(field.name)
    val location     = field.sourceInfo.location
    val maybeRefType = field.referenceTo.map(to => schemaTypeNameOf(to._1))

    val fieldType        = SObjectDeployer.platformTypeOfFieldType(field).typeName
    val fieldDeclaration = LocatableCustomFieldDeclaration(location, name, fieldType, maybeRefType)

    // Create additional fields & lookup relationships for special fields types
    field.rawType.value match {
      case "Lookup" | "MasterDetail" | "MetadataRelationship" =>
        Array(
          fieldDeclaration,
          LocatableCustomFieldDeclaration(
            location,
            name.replaceAll("__c$", "__r"),
            maybeRefType.get,
            None,
            asStatic = false,
            Some(field.referenceTo.get._2.toString)
          )
        )
      case "Location" =>
        Array(
          fieldDeclaration,
          LocatableCustomFieldDeclaration(
            location,
            name.replaceAll("__c$", "__latitude__s"),
            TypeNames.Double,
            None
          ),
          LocatableCustomFieldDeclaration(
            location,
            name.replaceAll("__c$", "__longitude__s"),
            TypeNames.Double,
            None
          )
        )
      case _ => Array(fieldDeclaration)
    }
  }

  private def addDerivedFieldsToObjects(
    fields: ArrayBuffer[(TypeName, CustomFieldEvent)],
    createdSObjects: mutable.Map[TypeName, SObjectLikeDeclaration]
  ): Unit = {
    fields
      .groupMap(_._1) {
        // group fields by obj name to overwrite each obj only once
        case (objName, field) =>
          val ghosted = field.relatedField.exists(f => module.isGhostedFieldName(f._2))
          val relField =
            field.relatedField.flatMap {
              case _ if ghosted => None
              case f            => findRelatedField(f, createdSObjects, fields)
            }

          createDerivedField(objName, field, relField, ghosted)
      }
      .foreach(a => addFieldsToSObject(a._1, a._2.toArray, createdSObjects))
  }

  private def createDerivedField(
    objectName: TypeName,
    field: CustomFieldEvent,
    relatedField: Option[FieldDeclaration],
    ghostRelated: Boolean
  ): FieldDeclaration = {
    val location         = field.sourceInfo.location
    val relatedFieldType = relatedField.map(_.typeName)

    if (relatedField.isEmpty && field.relatedField.nonEmpty && !ghostRelated) {
      val (relObj, relField) = field.relatedField.get
      OrgInfo.logError(
        location,
        s"Related field '$relObj.$relField' required by $objectName is not defined"
      )
    }

    LocatableCustomFieldDeclaration(
      location,
      defaultNamespace(field.name),
      relatedFieldType.getOrElse(SObjectDeployer.platformTypeOfFieldType(field).typeName),
      None
    )
  }

  private def findRelatedField(
    field: (Name, Name),
    createdSObjects: mutable.Map[TypeName, SObjectLikeDeclaration],
    derivedFields: ArrayBuffer[(TypeName, CustomFieldEvent)]
  ): Option[FieldDeclaration] = {
    val objName = schemaTypeNameOf(field._1)
    val relObj  = createdSObjects.get(objName)

    relObj
      .flatMap(_.findField(defaultNamespace(field._2), Some(false)))
      .orElse(
        // rare case of chained related fields
        derivedFields
          .find { case (obj, f) =>
            obj == objName && f.name == field._2
          }
          .flatMap(rf =>
            rf._2.relatedField.flatMap(findRelatedField(_, createdSObjects, derivedFields))
          )
      )
  }

  private def addFieldsToSObject(
    objectName: TypeName,
    fields: Array[FieldDeclaration],
    createdSObjects: mutable.Map[TypeName, SObjectLikeDeclaration]
  ): Unit = {
    createdSObjects.get(objectName).foreach { case td: SObjectDeclaration =>
      val newFields = fields.filter(f => !td.fields.exists(_.name == f.name))

      createdSObjects.put(
        td.typeName,
        extendExistingSObject(
          Some(td),
          Array(),
          td.typeName,
          td.sobjectNature,
          ArraySeq.unsafeWrapArray(newFields),
          ArraySeq(),
          ArraySeq()
        )
      )
    }
  }

  private def schemaTypeNameOf(name: Name): TypeName = {
    TypeName(defaultNamespace(name), Nil, Some(TypeNames.Schema))
  }

  private def createCustomObject(
    sources: Array[SourceInfo],
    event: SObjectEvent,
    typeName: TypeName,
    nature: SObjectNature,
    fields: ArraySeq[FieldDeclaration],
    fieldSets: ArraySeq[Name],
    sharingReasons: ArraySeq[Name]
  ): Array[SObjectDeclaration] = {

    if (module.isGhostedType(typeName))
      Array(
        extendExistingSObject(None, sources, typeName, nature, fields, fieldSets, sharingReasons)
      )
    else {
      val sobjectType = resolveBaseType(typeName)
      (event.isDefining, sobjectType.isEmpty) match {
        case (true, true) =>
          createNewSObject(
            sources,
            typeName,
            nature,
            fields,
            fieldSets,
            sharingReasons,
            event.sharingModel
          )
        case (false, false) =>
          createReplacementSObject(sources, typeName, nature, fields, fieldSets, sharingReasons)
        case (false, true) =>
          // Limit logging to once per file
          val paths = sources.map(_.location.path).distinct
          paths.foreach(path => {
            sources
              .find(_.location.path == path)
              .foreach(source => {
                OrgInfo.log(IssueOps.extendingUnknownSObject(source.location, event.name))
              })
          })
          Array.empty
        case (true, false) =>
          OrgInfo.log(IssueOps.redefiningSObject(sources.head.location, event.name))
          createReplacementSObject(sources, typeName, nature, fields, fieldSets, sharingReasons)
      }
    }
  }

  /** Create a new SObject along with it's supporting objects as needed. */
  private def createNewSObject(
    sources: Array[SourceInfo],
    typeName: TypeName,
    nature: SObjectNature,
    fields: ArraySeq[FieldDeclaration],
    fieldSets: ArraySeq[Name],
    sharingReasons: ArraySeq[Name],
    sharingModel: Option[SharingModel]
  ): Array[SObjectDeclaration] = {
    val syntheticSObjects =
      if (nature == CustomObjectNature) {
        // FUTURE: Check fields & when these should be available
        Array(
          createShare(sources, typeName, sharingReasons),
          createFeed(sources, typeName),
          createHistory(sources, typeName)
        )
      } else {
        Array[SObjectDeclaration]()
      }

    val hasOwner: Boolean = sharingModel match {
      case Some(ControlledByParentSharingModel)         => false
      case Some(ControlledByLeadOrContractSharingModel) => false
      case Some(ControlledByCampaignSharingModel)       => false
      case _                                            => true
    }

    syntheticSObjects :+
      new SObjectDeclaration(
        sources,
        module,
        typeName,
        nature,
        fieldSets,
        ArraySeq(),
        collectFields(typeName, nature, fields, hasOwner),
        _isComplete = true
      )
  }

  private def createShare(
    sources: Array[SourceInfo],
    typeName: TypeName,
    sharingReasons: ArraySeq[Name]
  ): SObjectDeclaration = {
    SObjectDeclaration(
      sources,
      module,
      typeName.withNameReplace("__c$", "__Share"),
      CustomObjectNature,
      ArraySeq(),
      sharingReasons,
      shareFieldsFor(typeName),
      _isComplete = true,
      isSynthetic = true
    )
  }

  private def createFeed(sources: Array[SourceInfo], typeName: TypeName): SObjectDeclaration = {
    SObjectDeclaration(
      sources,
      module,
      typeName.withNameReplace("__c$", "__Feed"),
      CustomObjectNature,
      ArraySeq(),
      ArraySeq(),
      feedFieldsFor(typeName),
      _isComplete = true,
      isSynthetic = true
    )
  }

  private def createHistory(sources: Array[SourceInfo], typeName: TypeName): SObjectDeclaration = {
    SObjectDeclaration(
      sources,
      module,
      typeName.withNameReplace("__c$", "__History"),
      CustomObjectNature,
      ArraySeq(),
      ArraySeq(),
      historyFieldsFor(typeName),
      _isComplete = true,
      isSynthetic = true
    )
  }

  /** Create an SObject to replace some existing SObject so that it can be extended. If no existing
    * can be found then an error is raised and the operation is new SObject is not created.
    */
  private def createReplacementSObject(
    sources: Array[SourceInfo],
    typeName: TypeName,
    nature: SObjectNature,
    fields: ArraySeq[FieldDeclaration],
    fieldSets: ArraySeq[Name],
    sharingReasons: ArraySeq[Name]
  ): Array[SObjectDeclaration] = {

    if (typeName == TypeNames.Activity) {
      // Fake Activity as applying to Task & Event, how bizarre is that
      createReplacementSObject(
        sources,
        TypeNames.Task,
        nature,
        fields,
        fieldSets,
        sharingReasons
      ) ++
        createReplacementSObject(
          sources,
          TypeNames.Event,
          nature,
          fields,
          fieldSets,
          sharingReasons
        )
    } else {
      val sobjectType = resolveBaseType(typeName)
      if (
        sobjectType.isEmpty || !sobjectType.get.superClassDeclaration
          .exists(superClass => superClass.typeName == TypeNames.SObject)
      ) {
        sources.headOption.foreach(source => {
          OrgInfo.logError(source.location, s"No SObject declaration found for '$typeName'")
        })
        return Array()
      }
      Array(
        extendExistingSObject(
          sobjectType,
          sources,
          typeName,
          nature,
          fields,
          fieldSets,
          sharingReasons
        )
      )
    }
  }

  /** Search for a type in dependent modules. Note. this skips the SObject handling in TypeFinder
    * deliberately.
    */
  private def resolveBaseType(typeName: TypeName): Option[TypeDeclaration] = {
    val td = module.baseModules.headOption.flatMap(_.findType(typeName).toOption)
    if (td.nonEmpty)
      return td

    val pkgTd = module.basePackages.headOption.flatMap(basePkg =>
      basePkg.modules.headOption.flatMap(_.findType(typeName).toOption)
    )
    if (pkgTd.nonEmpty)
      return pkgTd

    PlatformTypes.get(typeName, None).toOption
  }

  /** Create an SObject by extending a base SObject with new fields, fieldSets and sharing reasons.
    * If you don't pass a base object than this will extend System.SObject.
    */
  def extendExistingSObject(
    base: Option[TypeDeclaration],
    sources: Array[SourceInfo],
    typeName: TypeName,
    nature: SObjectNature,
    fields: ArraySeq[FieldDeclaration],
    fieldSets: ArraySeq[Name],
    sharingReasons: ArraySeq[Name]
  ): SObjectDeclaration = {

    // Check we are not trying to extend a non-extensible over a namespace boundary
    val crossModule = base.flatMap(_.moduleDeclaration).exists(_ != module)
    if (crossModule && !nature.extendable) {
      val baseNS = base.flatMap(_.moduleDeclaration).flatMap(_.namespace)
      if (baseNS != module.namespace) {
        OrgInfo.log(
          IssueOps.extendingOverNamespace(
            sources.head.location,
            nature,
            baseNS.getOrElse(Names.Empty),
            module.namespace.getOrElse(Names.Empty)
          )
        )
      }
    }

    // FUTURE: Add type for platform sobjects so we don't need this hackery
    def asSObject: Option[SObjectDeclaration] = {
      if (base.nonEmpty && base.get.isInstanceOf[SObjectDeclaration])
        base.map(_.asInstanceOf[SObjectDeclaration])
      else {
        None
      }
    }

    val extend          = base.getOrElse(PlatformTypes.sObjectType)
    val combinedSources = asSObject.map(_.sources).getOrElse(Array()) ++ sources
    val combinedFields =
      collectFields(typeName, nature, extend.fields ++ fields, hasOwner = base.isEmpty)
    val combinedFieldsets = ArraySeq.unsafeWrapArray(
      fieldSets
        .foldLeft(asSObject.map(_.fieldSets).getOrElse(ArraySeq()).toSet)((acc, fieldset) =>
          acc + fieldset
        )
        .toArray
    )
    val combinedSharingReasons = ArraySeq.unsafeWrapArray(
      sharingReasons
        .foldLeft(asSObject.map(_.sharingReasons).getOrElse(ArraySeq()).toSet)(
          (acc, sharingReason) => acc + sharingReason
        )
        .toArray
    )

    val newSObject = new SObjectDeclaration(
      combinedSources,
      module,
      typeName,
      nature,
      combinedFieldsets,
      combinedSharingReasons,
      combinedFields,
      base.nonEmpty && base.get.isComplete
    )

    // If we are extending over a module boundary then link via dependencies for refresh handling
    if (crossModule) {
      newSObject.addDependency(base.get)
    }

    newSObject
  }

  /** Collect fields for an SObject by folding custom fields over standard fields. */
  def collectFields(
    typeName: TypeName,
    nature: SObjectNature,
    fields: ArraySeq[FieldDeclaration],
    hasOwner: Boolean
  ): ArraySeq[FieldDeclaration] = {
    nature match {
      case ListCustomSettingNature       => customObjectFields(typeName, nature, fields, hasOwner)
      case HierarchyCustomSettingsNature => customObjectFields(typeName, nature, fields, hasOwner)
      case CustomObjectNature            => customObjectFields(typeName, nature, fields, hasOwner)
      case CustomMetadataNature          => customMetadataFields(typeName, fields)
      case BigObjectNature               => fields
      case PlatformEventNature           => platformEventFields(typeName, fields)
      case PlatformObjectNature          => fields
    }
  }

  /** Construct a full set of fields for a custom objects from the custom fields defined in the
    * event.
    */
  private def customObjectFields(
    typeName: TypeName,
    nature: SObjectNature,
    fields: ArraySeq[FieldDeclaration],
    hasOwner: Boolean
  ): ArraySeq[FieldDeclaration] = {

    def fieldFilter(field: FieldDeclaration): Boolean = {
      if (hasOwner)
        false
      else
        field.name.value == "OwnerId" || field.name.value == "Owner"
    }

    deDuplicateFields(
      ArraySeq(
        CustomFieldDeclaration(
          Names.SObjectType,
          TypeNames.sObjectType$(typeName),
          None,
          asStatic = true
        ),
        CustomFieldDeclaration(
          Names.Fields,
          TypeNames.sObjectFields$(typeName),
          None,
          asStatic = true
        ),
        CustomFieldDeclaration(Names.Id, TypeNames.IdType, Some(typeName))
      ) ++
        SObjectDeployer.standardCustomObjectFields.filterNot(fieldFilter) ++
        fields ++
        (if (nature == HierarchyCustomSettingsNature)
           ArraySeq(CustomFieldDeclaration(Names.SetupOwnerId, PlatformTypes.idType.typeName, None))
         else
           ArraySeq[FieldDeclaration]())
    )
  }

  /** Construct a full set of fields for a custom metadata from the custom fields defined in the
    * event.
    */
  private def customMetadataFields(
    typeName: TypeName,
    fields: ArraySeq[FieldDeclaration]
  ): ArraySeq[FieldDeclaration] = {
    deDuplicateFields(
      ArraySeq(
        CustomFieldDeclaration(Names.Id, TypeNames.IdType, Some(typeName)),
        CustomFieldDeclaration(
          Names.SObjectType,
          TypeNames.sObjectType$(typeName),
          None,
          asStatic = true
        )
      ) ++
        SObjectDeployer.standardCustomMetadataFields ++
        fields
    )
  }

  /** Construct a full set of fields for a platform event from the custom fields defined in the
    * event.
    */
  private def platformEventFields(
    typeName: TypeName,
    fields: ArraySeq[FieldDeclaration]
  ): ArraySeq[FieldDeclaration] = {
    deDuplicateFields(
      SObjectDeployer.standardPlatformEventFields ++
        fields :+
        CustomFieldDeclaration(
          Names.SObjectType,
          TypeNames.sObjectType$(typeName),
          None,
          asStatic = true
        )
    )
  }

  private def deDuplicateFields(fields: ArraySeq[FieldDeclaration]): ArraySeq[FieldDeclaration] = {
    // FUTURE: Duplicates are likely OK due to deploy handling, but we should warn?
    ArraySeq.unsafeWrapArray(fields.map(field => (field.name, field)).toMap.values.toArray)
  }

  private def defaultNamespace(name: Name): Name = {
    EncodedName(name).defaultNamespace(module.namespace).fullName
  }

}

object SObjectDeployer {

  /** Standard fields for custom objects, this is a superset, filtering may be needed to trim do to
    * available.
    */
  val standardCustomObjectFields: ArraySeq[FieldDeclaration] = {
    PlatformTypes.sObjectType.fields ++
      ArraySeq(
        CustomFieldDeclaration(Names.Id, TypeNames.IdType, None),
        CustomFieldDeclaration(Names.NameName, TypeNames.String, None),
        CustomFieldDeclaration(XNames.RecordTypeId, TypeNames.IdType, None),
        CustomFieldDeclaration(XNames.RecordType, TypeNames.RecordType, None),
        CustomFieldDeclaration(XNames.OwnerId, TypeNames.IdType, None),
        CustomFieldDeclaration(XNames.Owner, TypeNames.NameSObject, None),
        CustomFieldDeclaration(XNames.CurrencyIsoCode, TypeNames.String, None),
        CustomFieldDeclaration(XNames.CreatedBy, TypeNames.NameSObject, None),
        CustomFieldDeclaration(XNames.CreatedById, TypeNames.IdType, None),
        CustomFieldDeclaration(XNames.CreatedDate, TypeNames.Datetime, None),
        CustomFieldDeclaration(XNames.LastModifiedBy, TypeNames.User, None),
        CustomFieldDeclaration(XNames.LastModifiedById, TypeNames.IdType, None),
        CustomFieldDeclaration(XNames.LastModifiedDate, TypeNames.Datetime, None),
        CustomFieldDeclaration(XNames.LastReferencedDate, TypeNames.Datetime, None),
        CustomFieldDeclaration(XNames.LastViewedDate, TypeNames.Datetime, None),
        CustomFieldDeclaration(XNames.LastActivityDate, TypeNames.Datetime, None),
        CustomFieldDeclaration(XNames.Tasks, TypeNames.listOf(TypeNames.Task), None),
        CustomFieldDeclaration(XNames.Notes, TypeNames.listOf(TypeNames.Note), None),
        CustomFieldDeclaration(
          XNames.NotesAndAttachments,
          TypeNames.listOf(TypeNames.NoteAndAttachment),
          None
        ),
        CustomFieldDeclaration(XNames.Attachments, TypeNames.listOf(TypeNames.Attachment), None),
        CustomFieldDeclaration(
          XNames.ContentDocumentLinks,
          TypeNames.listOf(TypeNames.ContentDocumentLink),
          None
        ),
        CustomFieldDeclaration(
          XNames.ProcessSteps,
          TypeNames.listOf(TypeNames.ProcessInstanceHistory),
          None
        ),
        CustomFieldDeclaration(XNames.IsDeleted, TypeNames.Boolean, None),
        CustomFieldDeclaration(XNames.SystemModstamp, TypeNames.Datetime, None)
      )
  }

  /** Standard fields for platform events. */
  private val standardPlatformEventFields: ArraySeq[FieldDeclaration] = {
    ArraySeq(
      CustomFieldDeclaration(Names.ReplayId, TypeNames.String, None),
      CustomFieldDeclaration(XNames.CreatedBy, TypeNames.User, None),
      CustomFieldDeclaration(XNames.CreatedById, TypeNames.IdType, None),
      CustomFieldDeclaration(XNames.CreatedDate, TypeNames.Datetime, None)
    )
  }

  /** Standard fields for custom metadata. */
  private val standardCustomMetadataFields: ArraySeq[FieldDeclaration] = {
    ArraySeq(
      CustomFieldDeclaration(Names.DeveloperName, TypeNames.String, None),
      CustomFieldDeclaration(Names.IsProtected, TypeNames.Boolean, None),
      CustomFieldDeclaration(Names.Label, TypeNames.String, None),
      CustomFieldDeclaration(Names.Language, TypeNames.String, None),
      CustomFieldDeclaration(Names.MasterLabel, TypeNames.String, None),
      CustomFieldDeclaration(Names.NamespacePrefix, TypeNames.String, None),
      CustomFieldDeclaration(Names.QualifiedAPIName, TypeNames.String, None)
    )
  }

  /** Standard fields for a \_\_Share SObject. */
  def shareFieldsFor(typeName: TypeName): ArraySeq[FieldDeclaration] = {
    shareFields ++ commonFieldsFor(typeName)
  }

  private val shareFields: ArraySeq[FieldDeclaration] =
    PlatformTypes.sObjectType.fields ++ ArraySeq(
      CustomFieldDeclaration(XNames.IsDeleted, TypeNames.Boolean, None),
      CustomFieldDeclaration(XNames.LastModifiedBy, TypeNames.User, None),
      CustomFieldDeclaration(XNames.LastModifiedById, TypeNames.IdType, None),
      CustomFieldDeclaration(XNames.LastModifiedDate, TypeNames.Datetime, None),
      CustomFieldDeclaration(Names.AccessLevel, PlatformTypes.stringType.typeName, None),
      CustomFieldDeclaration(Names.ParentId, PlatformTypes.idType.typeName, None),
      CustomFieldDeclaration(Names.RowCause, PlatformTypes.stringType.typeName, None),
      CustomFieldDeclaration(Names.UserOrGroupId, PlatformTypes.idType.typeName, None)
    )

  def feedFieldsFor(typeName: TypeName): ArraySeq[FieldDeclaration] = {
    feedFields ++ commonFieldsFor(typeName)
  }

  private def commonFieldsFor(typeName: TypeName): ArraySeq[FieldDeclaration] = {
    ArraySeq(
      CustomFieldDeclaration(
        Names.SObjectType,
        TypeNames.sObjectType$(typeName),
        None,
        asStatic = true
      ),
      CustomFieldDeclaration(
        Names.Fields,
        TypeNames.sObjectFields$(typeName),
        None,
        asStatic = true
      )
    )
  }

  /** Standard fields for a \_\_Feed SObject. */
  private val feedFields: ArraySeq[FieldDeclaration] = PlatformTypes.sObjectType.fields ++ ArraySeq(
    CustomFieldDeclaration(XNames.BestCommentId, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.Body, TypeNames.String, None),
    CustomFieldDeclaration(XNames.CommentCount, TypeNames.Decimal, None),
    CustomFieldDeclaration(Names.ConnectionId, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.CreatedBy, TypeNames.NameSObject, None),
    CustomFieldDeclaration(XNames.CreatedById, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.CreatedDate, TypeNames.Datetime, None),
    CustomFieldDeclaration(XNames.InsertedById, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.InsertedBy, TypeNames.NameSObject, None),
    CustomFieldDeclaration(XNames.IsDeleted, TypeNames.Boolean, None),
    CustomFieldDeclaration(XNames.IsRichText, TypeNames.Boolean, None),
    CustomFieldDeclaration(XNames.LastModifiedBy, TypeNames.User, None),
    CustomFieldDeclaration(XNames.LastModifiedById, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.LastModifiedDate, TypeNames.Datetime, None),
    CustomFieldDeclaration(XNames.LikeCount, TypeNames.Decimal, None),
    CustomFieldDeclaration(XNames.LinkUrl, TypeNames.String, None),
    CustomFieldDeclaration(XNames.NetworkScope, TypeNames.String, None),
    CustomFieldDeclaration(XNames.ParentId, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.RelatedRecordId, TypeNames.IdType, None),
    CustomFieldDeclaration(XNames.RelatedRecord, TypeNames.SObject, None),
    CustomFieldDeclaration(XNames.Title, TypeNames.String, None),
    CustomFieldDeclaration(XNames.Type, TypeNames.String, None),
    CustomFieldDeclaration(XNames.Visibility, TypeNames.String, None)
  )

  def historyFieldsFor(typeName: TypeName): ArraySeq[FieldDeclaration] =
    historyFields ++ Array(
      CustomFieldDeclaration(
        Names.SObjectType,
        TypeNames.sObjectType$(typeName),
        None,
        asStatic = true
      ),
      CustomFieldDeclaration(
        Names.Fields,
        TypeNames.sObjectFields$(typeName),
        None,
        asStatic = true
      )
    )

  private val historyFields: ArraySeq[FieldDeclaration] =
    PlatformTypes.sObjectType.fields ++ ArraySeq(
      CustomFieldDeclaration(XNames.CreatedBy, TypeNames.NameSObject, None),
      CustomFieldDeclaration(XNames.CreatedById, TypeNames.IdType, None),
      CustomFieldDeclaration(XNames.CreatedDate, TypeNames.Datetime, None),
      CustomFieldDeclaration(XNames.DataType, TypeNames.String, None),
      CustomFieldDeclaration(XNames.Field, TypeNames.String, None),
      CustomFieldDeclaration(XNames.IsDeleted, TypeNames.Boolean, None),
      CustomFieldDeclaration(XNames.NewValue, TypeNames.InternalObject, None),
      CustomFieldDeclaration(XNames.OldValue, TypeNames.InternalObject, None),
      CustomFieldDeclaration(XNames.ParentId, TypeNames.IdType, None)
    )

  private val derivedFieldTypes: Set[Name] =
    Set(Name("Summary"))

  /** Convert a field type string to the platform type used for it in Apex. */
  private def platformTypeOfFieldType(field: CustomFieldEvent): TypeDeclaration = {
    field.rawType.value match {
      case "MasterDetail"        => PlatformTypes.idType
      case "Lookup"              => PlatformTypes.idType
      case "AutoNumber"          => PlatformTypes.stringType
      case "Checkbox"            => PlatformTypes.booleanType
      case "Currency"            => PlatformTypes.decimalType
      case "Date"                => PlatformTypes.dateType
      case "DateTime"            => PlatformTypes.datetimeType
      case "Email"               => PlatformTypes.stringType
      case "EncryptedText"       => PlatformTypes.stringType
      case "Number"              => PlatformTypes.decimalType
      case "Percent"             => PlatformTypes.decimalType
      case "Phone"               => PlatformTypes.stringType
      case "Picklist"            => PlatformTypes.stringType
      case "MultiselectPicklist" => PlatformTypes.stringType
      case "Summary"             => PlatformTypes.decimalType
      case "Text"                => PlatformTypes.stringType
      case "TextArea"            => PlatformTypes.stringType
      case "LongTextArea"        => PlatformTypes.stringType
      case "Url"                 => PlatformTypes.stringType
      case "File"                => PlatformTypes.stringType
      case "Location"            => PlatformTypes.locationType
      case "Time"                => PlatformTypes.timeType
      case "Html"                => PlatformTypes.stringType
      case "MetadataRelationship"
          if field.referenceTo
            .map(_._1.value)
            .exists(to => to == "FieldDefinition" || to == "EntityDefinition") =>
        PlatformTypes.stringType
      case "MetadataRelationship" => PlatformTypes.idType
      // pkgforce validates on loading, so need for default handling here
    }
  }
}
