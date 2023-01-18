/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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

import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.types.apex.{FullDeclaration, TriggerDeclaration}
import com.nawforce.apexlink.types.core.{DependentType, TypeId}
import com.nawforce.apexlink.types.other.{
  ComponentDeclaration,
  InterviewDeclaration,
  LabelDeclaration,
  PageDeclaration
}
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.names.{EncodedName, Name, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.stream._
import com.nawforce.runtime.parsers.SourceData

import scala.collection.immutable.ArraySeq

trait ModuleRefresh {
  self: OPM.Module =>

  def refreshInternal(existingLabels: LabelDeclaration): Seq[(TypeId, Set[TypeId])] = {
    val newLabels = createLabelDeclaration()
    val holders   = existingLabels.getTypeDependencyHolders
    newLabels.updateTypeDependencyHolders(holders)
    replaceType(newLabels.typeName, Some(newLabels))
    newLabels.validate()
    Seq((newLabels.typeId, holders.toSet))
  }

  /* Replace a path, returns the TypeId of the type that was updated and a Set of TypeIds for the dependency
   * holders of that type. */
  def refreshInternal(path: PathLike): Seq[(TypeId, Set[TypeId])] = {
    PlatformTypes.withLoadingObserver(schemaSObjectType) {

      checkPathInPackageOrThrow(path)
      val doc = MetadataDocument(path).getOrElse(
        throw new IllegalArgumentException(s"Metadata type is not supported for '$path'")
      )
      val sourceOpt = resolveSource(path)
      val typeId    = TypeId(this, doc.typeName(namespace))

      // Update internal document tracking
      if (sourceOpt.isEmpty) {
        pkg.org.issueManager.pop(doc.path)
        if (!index.remove(doc)) {
          LoggerOps.debug(s"Refresh of deleted ${doc.path} was not in use, ignoring.")
          return Seq()
        }
      } else if (!index.upsert(pkg.org.issueManager, doc)) {
        LoggerOps.debug(s"Refresh of ${doc.path} would create duplicate type, ignoring.")
        return Seq()
      }

      // Create type & forward holders to limit need for invalidation chaining
      val newTypes = createTypes(doc, sourceOpt)
      if (newTypes.nonEmpty) {
        newTypes.map(newType => {
          val existingType = getDependentType(newType.typeName)
          val holders = existingType
            .map(_.getTypeDependencyHolders)
            .getOrElse(DependentType.emptyTypeDependencyHolders)
          newType.updateTypeDependencyHolders(holders)

          // Update and validate
          replaceType(newType.typeName, Some(newType))
          newType.validate()
          (typeId, holders.toSet)
        })
      } else {
        val existingType = getDependentType(typeId.typeName)
        val holders = existingType
          .map(_.getTypeDependencyHolders)
          .getOrElse(DependentType.emptyTypeDependencyHolders)
        removeTypes(doc)
        Seq((typeId, holders.toSet))
      }
    }
  }

  private def getDependentType(typeName: TypeName): Option[DependentType] = {
    types
      .get(typeName)
      .flatMap {
        case dt: DependentType => Some(dt)
        case _                 => None
      }
  }

  private def removeTypes(doc: MetadataDocument): Unit = {
    doc match {
      case doc: SObjectDocument =>
        if (doc.path.toString.endsWith("object-meta.xml"))
          removeSObjectTypes(doc.path.parent.basename)
        else
          removeSObjectTypes(doc.path.basename.replaceFirst("\\.object$", ""))
      case _: SObjectFieldDocument | _: SObjectFieldSetDocument | _: SObjectSharingReasonDocument =>
        val sObjectDir = doc.path.parent.parent
        removeSObjectTypes(sObjectDir.basename)
      case _ =>
        types.remove(doc.typeName(namespace)).foreach(_.dead = true)
    }
  }

  private def removeSObjectTypes(sobjectName: String): Unit = {
    val name = EncodedName(sobjectName)
    if (name.ext.contains(Name("c"))) {
      val typeName = TypeName(name.fullName, Nil, Some(TypeNames.Schema))
      val objectNames = Seq(
        typeName,
        typeName.withNameReplace("__c$", "__Share"),
        typeName.withNameReplace("__c$", "__Feed"),
        typeName.withNameReplace("__c$", "__History")
      )
      objectNames.foreach(typeName => schemaSObjectType.remove(typeName.name))
      objectNames.foreach(typeName => types.remove(typeName).foreach(_.dead = true))
    }
  }

  private def createLabelDeclaration(): LabelDeclaration = {
    val events = LabelGenerator.iterator(index)
    val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
    LabelDeclaration(this).merge(stream)
  }

  private def createTypes(doc: MetadataDocument, source: Option[SourceData]): Seq[DependentType] = {
    doc match {
      case doc: ApexClassDocument =>
        source.flatMap(s => FullDeclaration.create(this, doc, s, forceConstruct = true)).toSeq
      case _: ApexClassMetaDocument => Seq()

      case _: ApexTriggerDocument =>
        source.flatMap(s => TriggerDeclaration.create(this, doc.path, s)).toSeq
      case _: ApexTriggerMetaDocument => Seq()

      case _: SObjectLike | _: SObjectFieldDocument | _: SObjectFieldSetDocument |
          _: SObjectSharingReasonDocument =>
        val controllingTypeName = doc.controllingTypeName(namespace)
        val docs                = index.get(SObjectNature, controllingTypeName)
        if (docs.nonEmpty) {
          val deployer = new SObjectDeployer(this)
          val sobjects = deployer.createSObjects(
            SObjectGenerator
              .toEvents(docs)
              .buffered
          )
          sobjects.foreach(sobject =>
            schemaSObjectType.add(sobject.typeName.name, hasFieldSets = true)
          )
          sobjects.toIndexedSeq
        } else {
          Seq()
        }
      case _: LabelsDocument =>
        Seq(createLabelDeclaration())

      case _: PageDocument =>
        val events = PageGenerator.iterator(index)
        val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
        Seq(PageDeclaration(this).merge(stream))

      case _: ComponentDocument =>
        val events = ComponentGenerator.iterator(index)
        val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
        Seq(ComponentDeclaration(this).merge(stream))

      case _: FlowDocument =>
        val events = FlowGenerator.iterator(index)
        val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
        Seq(InterviewDeclaration(this).merge(stream))
    }
  }

  private def checkPathInPackageOrThrow(path: PathLike): Unit = {
    if (!index.isVisibleFile(path))
      throw new IllegalArgumentException(s"Metadata is not part of this package for '$path'")
  }

  private def resolveSource(path: PathLike): Option[SourceData] = {
    path.readSourceData() match {
      case Left(_)     => None
      case Right(data) => Some(data)
    }
  }
}
