/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.common.types.schema

import com.nawforce.common.api.{Name, TypeName}
import com.nawforce.common.cst.{GLOBAL_MODIFIER, Modifier, VerifyContext}
import com.nawforce.common.documents._
import com.nawforce.common.finding.TypeResolver
import com.nawforce.common.names.{DotName, Names, TypeNames, _}
import com.nawforce.common.org.{OrgImpl, PackageImpl}
import com.nawforce.common.path.PathLike
import com.nawforce.common.types.core.{BasicTypeDeclaration, FieldDeclaration, MethodDeclaration, TypeDeclaration}
import com.nawforce.common.types.platform.PlatformTypes
import com.nawforce.common.types.synthetic.{CustomFieldDeclaration, CustomMethodDeclaration, CustomParameterDeclaration}

import scala.collection.mutable

final case class SObjectDeclaration(_paths: Seq[PathLike], pkg: PackageImpl, _typeName: TypeName,
                                    sobjectNature: SObjectNature, fieldSets: Set[Name],
                                    override val fields: Seq[FieldDeclaration], override val isComplete: Boolean)
  extends BasicTypeDeclaration(_paths, pkg, _typeName) {

  override val modifiers: Array[Modifier] = Array(GLOBAL_MODIFIER)

  override val superClass: Option[TypeName] = Some(TypeNames.SObject)
  override lazy val superClassDeclaration: Option[TypeDeclaration] = {
    pkg.getTypeFor(TypeNames.SObject, this)
  }

  override def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    super.findFieldSObject(name, staticContext).orElse({
      val field = pkg.schema().relatedLists.findField(typeName, name)
      if (field.nonEmpty && staticContext.contains(true)) {
        Some(CustomFieldDeclaration(field.get.name, TypeNames.sObjectFields$(typeName), None, asStatic = true))
      } else {
        field
      }
    })
  }

  override def findMethod(name: Name, params: Seq[TypeName], staticContext: Option[Boolean],
                          verifyContext: VerifyContext): Seq[MethodDeclaration] = {
    if (staticContext.contains(true)) {
      val customMethods = sobjectNature match {
        case HierarchyCustomSettingsNature => hierarchyCustomSettingsMethods
        case ListCustomSettingNature => listCustomSettingsMethods
        case _ => SObjectDeclaration.sObjectMethodMap
      }
      val customMethod = customMethods.get((name, params.size))
      if (customMethod.nonEmpty)
        return customMethod.toSeq
    }
    defaultFindMethod(name, params, staticContext, verifyContext)
  }

  def defaultFindMethod(name: Name, params: Seq[TypeName], staticContext: Option[Boolean],
                        verifyContext: VerifyContext): Seq[MethodDeclaration] = {
    val clone = cloneMethods.get((name, params.size, staticContext.contains(true)))
    if (clone.nonEmpty)
      clone.toSeq
    else
      PlatformTypes.sObjectType.findMethod(name, params, staticContext, verifyContext)
  }

  private lazy val cloneMethods: Map[(Name, Int, Boolean), MethodDeclaration] = {
    val preserveId = CustomParameterDeclaration(Name("preserveId"), TypeNames.Boolean)
    val isDeepClone = CustomParameterDeclaration(Name("isDeepClone"), TypeNames.Boolean)
    val preserveReadOnlyTimestamps = CustomParameterDeclaration(Name("preserveReadOnlyTimestamps"), TypeNames.Boolean)
    val preserveAutonumber = CustomParameterDeclaration(Name("preserveAutonumber"), TypeNames.Boolean)
    Seq(
      CustomMethodDeclaration(None, Name("clone"), typeName, Array()),
      CustomMethodDeclaration(None, Name("clone"), typeName, Array(preserveId)),
      CustomMethodDeclaration(None, Name("clone"), typeName, Array(preserveId, isDeepClone)),
      CustomMethodDeclaration(None, Name("clone"), typeName, Array(preserveId, isDeepClone, preserveReadOnlyTimestamps)),
      CustomMethodDeclaration(None, Name("clone"), typeName, Array(preserveId, isDeepClone, preserveReadOnlyTimestamps, preserveAutonumber))
    ).map(m => ((m.name, m.parameters.length, m.isStatic),m)).toMap
  }

  private lazy val hierarchyCustomSettingsMethods: Map[(Name, Int), MethodDeclaration] =
    Seq(
      CustomMethodDeclaration(None, Name("getInstance"), typeName, Array()),
      CustomMethodDeclaration(None, Name("getInstance"), typeName, Array(CustomParameterDeclaration(Name("Id"), TypeNames.Id))),
      CustomMethodDeclaration(None, Name("getOrgDefaults"), typeName, Array()),
      CustomMethodDeclaration(None, Name("getValues"), typeName, Array(CustomParameterDeclaration(Name("Id"), TypeNames.Id))),
    ).map(m => ((m.name, m.parameters.length),m)).toMap

  private lazy val listCustomSettingsMethods: Map[(Name, Int), MethodDeclaration] =
    Seq(
      CustomMethodDeclaration(None, Name("getAll"), TypeNames.mapOf(TypeNames.String, typeName), Array()),
      CustomMethodDeclaration(None, Name("getInstance"), typeName, Array()),
      CustomMethodDeclaration(None, Name("getInstance"), typeName, Array(CustomParameterDeclaration(Name("Name"), TypeNames.String))),
      CustomMethodDeclaration(None, Name("getValues"), typeName, Array(CustomParameterDeclaration(Name("Name"), TypeNames.String))),
    ).map(m => ((m.name, m.parameters.length),m)).toMap
}

object SObjectDeclaration {
  private lazy val sObjectMethodMap: Map[(Name, Int), MethodDeclaration] =
    PlatformTypes.sObjectType.methods.map(m => ((m.name, m.parameters.length),m)).toMap

  def create(pkg: PackageImpl, path: PathLike): Seq[TypeDeclaration] = {
    val sobjectDetailsOpt = SObjectDetails.parseSObject(path, pkg)
    if (sobjectDetailsOpt.isEmpty)
      return Seq()
    val sobjectDetails = sobjectDetailsOpt.get

    if (sobjectDetails.isIntroducing(pkg)) {
      createNew(sobjectDetails, pkg)
    } else {
        if (pkg.isGhostedType(sobjectDetails.typeName))
          Seq(extendExisting(sobjectDetails, pkg, None))
        else {
          createExisting(sobjectDetails, path, pkg)
        }
    }
  }

  private def createExisting(sobjectDetails: SObjectDetails, path: PathLike, pkg: PackageImpl) : Seq[TypeDeclaration] = {
    val typeName = sobjectDetails.typeName

    if (typeName.name == Names.Activity) {
      // Fake Activity as applying to Task & Event, how bizarre is that
      createExisting(sobjectDetails.withTypeName(typeName.withName(Names.Task)), path, pkg) ++
        createExisting(sobjectDetails.withTypeName(typeName.withName(Names.Event)), path, pkg)
    } else {
      val sobjectType = TypeResolver(typeName, pkg, excludeSObjects = false).toOption
      if (sobjectType.isEmpty || !sobjectType.get.superClassDeclaration.exists(superClass => superClass.typeName == TypeNames.SObject)) {
        OrgImpl.logError(LineLocationImpl(path.toString, 0), s"No SObject declaration found for '$typeName'")
        return Seq()
      }
      Seq(extendExisting(sobjectDetails, pkg, sobjectType))
    }
  }

  private def createNew(sobjectDetails: SObjectDetails, pkg: PackageImpl): Seq[TypeDeclaration] = {
    val typeName = sobjectDetails.typeName

    val fields =
      if (sobjectDetails.sobjectNature == CustomMetadataNature)
        customMetadataFields(sobjectDetails)
      else if (sobjectDetails.sobjectNature == PlatformEventNature)
        platformEventFields(sobjectDetails)
      else
        customObjectFields(sobjectDetails)

    val supportObjects: Seq[SObjectDeclaration] =
      if (sobjectDetails.isIntroducing(pkg) &&
        sobjectDetails.sobjectNature != CustomMetadataNature && sobjectDetails.sobjectNature != PlatformEventNature) {
        Seq(
          // TODO: Check fields & when should be available
          createShare(pkg, typeName),
          createFeed(pkg, typeName),
          createHistory(pkg, typeName)
        )
      } else Seq()

    // TODO: Provide paths
    val allObjects =
      new SObjectDeclaration(Seq.empty, pkg, typeName, sobjectDetails.sobjectNature, sobjectDetails.fieldSets,
        fields, isComplete = true) +:
      supportObjects

    allObjects.foreach(pkg.schema().sobjectTypes.add)
    allObjects
  }

  private lazy val standardCustomObjectFields: Seq[FieldDeclaration] = {
    Seq(
      CustomFieldDeclaration(Names.NameName, TypeNames.String, None),
      CustomFieldDeclaration(Names.RecordTypeId, TypeNames.Id, None),
      CustomFieldDeclaration(Name("CreatedBy"), TypeNames.User, None),
      CustomFieldDeclaration(Name("CreatedById"), TypeNames.Id, None),
      CustomFieldDeclaration(Name("CreatedDate"), TypeNames.Datetime, None),
      CustomFieldDeclaration(Name("LastModifiedBy"), TypeNames.User, None),
      CustomFieldDeclaration(Name("LastModifiedById"), TypeNames.Id, None),
      CustomFieldDeclaration(Name("LastModifiedDate"), TypeNames.Datetime, None),
      CustomFieldDeclaration(Name("IsDeleted"), TypeNames.Boolean, None),
      CustomFieldDeclaration(Name("SystemModstamp"), TypeNames.Datetime, None)
    ) ++
    PlatformTypes.sObjectType.fields.filterNot(f => f.name == Names.SObjectType)
  }

  private def customObjectFields(sobjectDetails: SObjectDetails): Seq[FieldDeclaration] = {
    Seq(
      CustomFieldDeclaration(Names.SObjectType, TypeNames.sObjectType$(sobjectDetails.typeName), None, asStatic = true),
      CustomFieldDeclaration(Names.Fields, TypeNames.sObjectFields$(sobjectDetails.typeName), None, asStatic = true),
      CustomFieldDeclaration(Names.Id, TypeNames.Id, Some(sobjectDetails.typeName))
    ) ++
      standardCustomObjectFields ++
      sobjectDetails.fields ++
      (if (sobjectDetails.sobjectNature == HierarchyCustomSettingsNature)
        Seq(CustomFieldDeclaration(Names.SetupOwnerId, PlatformTypes.idType.typeName, None))
      else
        Seq()
        )
  }

  private lazy val standardCustomMetadataFields: Seq[FieldDeclaration] = {
    Seq(
      CustomFieldDeclaration(Names.DeveloperName, TypeNames.String, None),
      CustomFieldDeclaration(Names.IsProtected, TypeNames.Boolean, None),
      CustomFieldDeclaration(Names.Label, TypeNames.String, None),
      CustomFieldDeclaration(Names.Language, TypeNames.String, None),
      CustomFieldDeclaration(Names.MasterLabel, TypeNames.String, None),
      CustomFieldDeclaration(Names.NamespacePrefix, TypeNames.String, None),
      CustomFieldDeclaration(Names.QualifiedAPIName, TypeNames.String, None))
  }

  private def customMetadataFields(sobjectDetails: SObjectDetails): Seq[FieldDeclaration] = {
    Seq(
      CustomFieldDeclaration(Names.Id, TypeNames.Id, Some(sobjectDetails.typeName)),
      CustomFieldDeclaration(Names.SObjectType, TypeNames.sObjectType$(sobjectDetails.typeName), None, asStatic = true)
    ) ++
      standardCustomMetadataFields ++
      sobjectDetails.fields
  }

  private lazy val standardPlatformEventFields: Seq[FieldDeclaration] = {
    Seq(CustomFieldDeclaration(Names.ReplayId, TypeNames.String, None))
  }

  private def platformEventFields(sobjectDetails: SObjectDetails): Seq[FieldDeclaration] = {
    standardPlatformEventFields ++
      sobjectDetails.fields :+
      CustomFieldDeclaration(Names.SObjectType, TypeNames.sObjectType$(sobjectDetails.typeName), None, asStatic = true)
  }

  private def extendExisting(sobjectDetails: SObjectDetails, pkg: PackageImpl, base: Option[TypeDeclaration]): TypeDeclaration = {
    val typeName = sobjectDetails.typeName
    val isComplete = base.nonEmpty && pkg.basePackages.forall(!_.isGhosted)

    val fields = collectBaseFields(typeName.asDotName, pkg)
    base.getOrElse(PlatformTypes.sObjectType).fields.foreach(field => fields.put(field.name, field))
    sobjectDetails.fields.foreach(field => {fields.put(field.name, field)})

    // TODO: Collect base fieldsets ?
    // TODO: Provide paths
    val td = new SObjectDeclaration(Seq.empty, pkg, typeName, sobjectDetails.sobjectNature, sobjectDetails.fieldSets,
      fields.values.toSeq, isComplete)
    pkg.schema().sobjectTypes.add(td)
    td
  }

  private def collectBaseFields(sObject: DotName, pkg: PackageImpl): mutable.Map[Name, FieldDeclaration] = {
    val collected: mutable.Map[Name, FieldDeclaration] = mutable.Map()
    pkg.basePackages.filterNot(_.isGhosted).foreach(basePkg => {
      val fields: Seq[FieldDeclaration] = TypeResolver(sObject.asTypeName(), basePkg, excludeSObjects = false).toOption.map {
        case baseTd: SObjectDeclaration => baseTd.fields
        case _ => Nil
      }.getOrElse(Seq())
      fields.foreach(field => {collected.put(field.name, field)})
    })
    collected
  }

  private def createShare(pkg: PackageImpl, typeName: TypeName): SObjectDeclaration = {
    val shareName = typeName.withNameReplace("__c$", "__Share")
    val sobjectDetails = SObjectDetails(CustomObjectNature, shareName, Seq(), Set())

    // TODO: Provide paths
    SObjectDeclaration(Seq.empty, pkg, shareName, CustomObjectNature, Set(),
      customObjectFields(sobjectDetails) ++ shareFields, isComplete = true)
  }

  private lazy val shareFields = PlatformTypes.sObjectType.fields ++ Seq(
    CustomFieldDeclaration(Names.AccessLevel, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.ParentId, PlatformTypes.idType.typeName, None),
    CustomFieldDeclaration(Names.RowCause, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.UserOrGroupId, PlatformTypes.idType.typeName, None)
  )

  private def createFeed(pkg: PackageImpl, typeName: TypeName): SObjectDeclaration = {
    val feedName = typeName.withNameReplace("__c$", "__Feed")
    val sobjectDetails = SObjectDetails(CustomObjectNature, feedName, Seq(), Set())

    // TODO: Provide paths
    SObjectDeclaration(Seq.empty, pkg, feedName, CustomObjectNature, Set(),
      customObjectFields(sobjectDetails) ++ feedFields, isComplete = true)
  }

  private lazy val feedFields = PlatformTypes.sObjectType.fields ++ Seq(
    CustomFieldDeclaration(Names.BestCommentId, PlatformTypes.idType.typeName, None),
    CustomFieldDeclaration(Names.Body, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.CommentCount, PlatformTypes.decimalType.typeName, None),
    CustomFieldDeclaration(Names.ConnectionId, PlatformTypes.idType.typeName, None),
    CustomFieldDeclaration(Names.InsertedById, PlatformTypes.idType.typeName, None),
    CustomFieldDeclaration(Names.IsRichText, PlatformTypes.booleanType.typeName, None),
    CustomFieldDeclaration(Names.LikeCount, PlatformTypes.decimalType.typeName, None),
    CustomFieldDeclaration(Names.LinkUrl, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.NetworkScope, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.ParentId, PlatformTypes.idType.typeName, None),
    CustomFieldDeclaration(Names.RelatedRecordId, PlatformTypes.idType.typeName, None),
    CustomFieldDeclaration(Names.Title, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.Type, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.Visibility, PlatformTypes.stringType.typeName, None),
  )

  private def createHistory(pkg: PackageImpl, typeName: TypeName): SObjectDeclaration = {
    val historyName = typeName.withNameReplace("__c$", "__History")
    val sobjectDetails = SObjectDetails(CustomObjectNature, historyName, Seq(), Set())

    // TODO: Provide paths
    SObjectDeclaration(Seq.empty, pkg, historyName, CustomObjectNature, Set(),
      customObjectFields(sobjectDetails) ++ historyFields, isComplete = true)
  }

  private lazy val historyFields = PlatformTypes.sObjectType.fields ++ Seq(
    CustomFieldDeclaration(Names.Field, PlatformTypes.stringType.typeName, None),
    CustomFieldDeclaration(Names.NewValue, PlatformTypes.objectType.typeName, None),
    CustomFieldDeclaration(Names.OldValue, PlatformTypes.objectType.typeName, None),
  )
}
