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

package com.nawforce.apexlink.types.schema

import com.nawforce.apexlink.cst.VerifyContext
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.finding.TypeResolver.TypeCache
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.org.{OPM, OrgInfo, SObjectDeployer}
import com.nawforce.apexlink.types.core._
import com.nawforce.apexlink.types.schema.SObjectDeclaration.syntheticExtension
import com.nawforce.apexlink.types.synthetic.{
  CustomField,
  CustomMethodDeclaration,
  CustomParameterDeclaration
}
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.{EncodedName, Name, TypeName}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, Nature}
import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation, UnsafeLocatable}
import com.nawforce.pkgforce.stream.{HierarchyCustomSetting, ListCustomSetting, SObjectEvent}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

sealed abstract class SObjectNature(val nature: String, val extendable: Boolean) {
  override def toString: String = nature
}

case object ListCustomSettingNature extends SObjectNature("List Custom Setting", extendable = true)

case object HierarchyCustomSettingsNature
    extends SObjectNature("Hierarchy Custom Setting", extendable = true)

case object CustomObjectNature extends SObjectNature("Custom Object", extendable = true)

case object PlatformEventNature extends SObjectNature("Platform Event", extendable = false)

case object CustomMetadataNature extends SObjectNature("Custom Metadata", extendable = false)

case object BigObjectNature extends SObjectNature("Big Object", extendable = false)

// Special case used for standard objects which need an SObjectDeclaration due to a lookups from elsewhere
case object PlatformObjectNature extends SObjectNature("Platform Object", extendable = true)

object SObjectNature {
  def apply(name: Name, event: SObjectEvent): SObjectNature = {
    name match {
      case name if name.value.endsWith("__mdt") => CustomMetadataNature
      case name if name.value.endsWith("__b")   => BigObjectNature
      case name if name.value.endsWith("__e")   => PlatformEventNature
      case _ =>
        event.customSettingsType match {
          case Some(ListCustomSetting)      => ListCustomSettingNature
          case Some(HierarchyCustomSetting) => HierarchyCustomSettingsNature
          case _                            => CustomObjectNature
        }
    }
  }
}

trait SObjectLikeDeclaration extends DependentType with SObjectFieldFinder

final case class SObjectDeclaration(
  sources: Array[SourceInfo],
  module: OPM.Module,
  typeName: TypeName,
  sobjectNature: SObjectNature,
  fieldSets: ArraySeq[Name],
  sharingReasons: ArraySeq[Name],
  fields: ArraySeq[FieldDeclaration],
  isComplete: Boolean = true,
  crossModuleBase: Option[SObjectDeclaration] = None
) extends SObjectLikeDeclaration
    with SObjectMethods
    with UnsafeLocatable
    with DependencyHolder
    with Dependent {

  override def location: PathLocation                = sources.headOption.map(_.location).orNull
  override val inTest: Boolean                       = false
  override val moduleDeclaration: Option[OPM.Module] = Some(module)
  def isSynthetic: Boolean = EncodedName(typeName.name).ext.exists(syntheticExtension.contains)

  override val paths: ArraySeq[PathLike] =
    ArraySeq.unsafeWrapArray(sources.map(source => source.location.path))
  val sourceHash: Int = MurmurHash3.unorderedHash(sources.map(_.hash), 0)
  private val depends = mutable.Set[Dependent]()

  override val name: Name                      = typeName.name
  override val outerTypeName: Option[TypeName] = None
  override val nature: Nature                  = CLASS_NATURE
  override val modifiers: ArraySeq[Modifier]   = SObjectDeclaration.globalModifiers
  override val interfaces: ArraySeq[TypeName]  = ArraySeq()

  override def nestedTypes: ArraySeq[TypeDeclaration] = TypeDeclaration.emptyTypeDeclarations

  override val blocks: ArraySeq[BlockDeclaration] = BlockDeclaration.emptyBlockDeclarations

  override val superClass: Option[TypeName] = {
    Some(TypeNames.SObject)
  }

  override lazy val superClassDeclaration: Option[TypeDeclaration] =
    TypeResolver(superClass.get, this).toOption

  override def validate(): Unit = {
    // Check field types, can be ignored for Feed, Share & History synthetic SObjects
    if (isSynthetic) return

    // Lookup like and summary field need specific validations
    fields.foreach {
      case field: CustomField if field.relationshipName.nonEmpty =>
        validateLookupLike(field, this.sources)
      case field: CustomField if field.derivedFrom.nonEmpty =>
        validateSummary(field)
      case _ => ()
    }

    // Update dependencies from field types
    fields.map(_.typeName).toSet.filterNot(_ == typeName).foreach(updateDependencies)
    propagateDependencies()
    propagateOuterDependencies(new TypeCache())
  }

  /** Custom validation steps for lookup fields. */
  private def validateLookupLike(field: CustomField, additionalSources: Array[SourceInfo]): Unit = {

    // We might need to create a module specific version of the field typeName for related list support. However
    // we might also be validating because that target has been replaced at source, in which case any current
    // module specific version needs to be replaced by a fresh copy.
    //
    // To complicate a little more we pass some additional sources to the clone so that it is guaranteed to
    // have some files from this module. If it didn't the refresh mechanism in PackageAPI.reValidate() would
    // not function for the clone.

    val sobject = TypeResolver(field.typeName, module).toOption match {
      case Some(td: SObjectDeclaration) => Some(td)
      case _                            => None
    }
    sobject match {
      case Some(sobject: SObjectDeclaration) if sobject.crossModuleBase.nonEmpty =>
        module.nextModule
          .flatMap(next => TypeResolver(field.typeName, next).toOption)
          .collect { case sobject: SObjectDeclaration => sobject }
          .foreach(currentBase => {
            if (currentBase ne sobject.crossModuleBase.get)
              cloneSObject(currentBase, additionalSources)
          })
      case Some(sobject: SObjectDeclaration) if !sobject.moduleDeclaration.contains(module) =>
        cloneSObject(sobject, additionalSources)
      case Some(_) => ()
      case None =>
        if (module.isGhostedType(field.typeName)) {
          // Create ghost SObject for later validations
          val ghostedSObject = GhostSObjectDeclaration(module, field.typeName)
          module.types.put(field.typeName, ghostedSObject)
          module.schemaSObjectType.add(ghostedSObject.typeName.name, hasFieldSets = true)
        } else {
          OrgInfo.logMissing(
            field.location,
            s"Lookup object ${field.typeName} does not exist for field '${field.name}'"
          )
        }
    }

    if (field.typeName != typeName)
      updateDependencies(field.typeName)
  }

  private def cloneSObject(
    baseSObject: SObjectDeclaration,
    additionalSources: Array[SourceInfo]
  ): SObjectDeclaration = {
    val deployer = new SObjectDeployer(module)
    val replacement = deployer.extendExistingSObject(
      Some(baseSObject),
      additionalSources,
      baseSObject.typeName,
      baseSObject.sobjectNature,
      ArraySeq(),
      ArraySeq(),
      ArraySeq()
    )
    replacement.propagateOuterDependencies(new TypeCache())
    module.types.put(replacement.typeName, replacement)
    module.schemaSObjectType.add(replacement.typeName.name, hasFieldSets = true)
    replacement
  }

  /** Custom validation steps for summary fields. */
  private def validateSummary(field: CustomField): Unit = {
    // Depend on the SObjects the field is derived from
    field.derivedFrom.foreach(updateDependencies)
  }

  private def updateDependencies(typeName: TypeName): Unit = {
    TypeResolver(typeName, module) match {
      case Right(d: Dependent) => addDependency(d)
      case _                   => ()
    }
    typeName.params.foreach(updateDependencies)
  }

  def addDependency(dependent: Dependent): Unit = depends.add(dependent)

  override def dependencies(): Iterable[Dependent] = depends

  override def gatherDependencies(
    dependsOn: mutable.Set[TypeId],
    apexOnly: Boolean,
    outerTypesOnly: Boolean,
    typeCache: TypeCache
  ): Unit = {
    DependentType.dependentsToTypeIds(module, depends, apexOnly, outerTypesOnly, dependsOn)
  }

  override def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    findSObjectField(name, staticContext)
  }

  override val methods: ArraySeq[MethodDeclaration] = MethodDeclaration.emptyMethodDeclarations
  override val constructors: ArraySeq[ConstructorDeclaration] =
    ConstructorDeclaration.emptyConstructorDeclarations

  override def findMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    verifyContext: VerifyContext
  ): Either[String, MethodDeclaration] = {
    // Some types of SObject have special static methods that use the typeName
    val customMethods = if (staticContext.contains(true)) {
      sobjectNature match {
        case HierarchyCustomSettingsNature => Some(hierarchyCustomSettingsMethods)
        case ListCustomSettingNature       => Some(listCustomSettingsMethods)
        case CustomMetadataNature          => Some(customMetadataMethods)
        case _                             => None
      }
    } else { None }
    val method = customMethods.flatMap(_.get((name, params.length)))
    if (method.nonEmpty)
      Right(method.get)
    else
      defaultFindMethod(name, params, staticContext, verifyContext)
  }

  private lazy val hierarchyCustomSettingsMethods: Map[(Name, Int), MethodDeclaration] =
    Seq(
      CustomMethodDeclaration(
        Location.empty,
        Name("getInstance"),
        typeName,
        CustomMethodDeclaration.emptyParameters
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getInstance"),
        typeName,
        ArraySeq(CustomParameterDeclaration(Name("Id"), TypeNames.IdType))
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getOrgDefaults"),
        typeName,
        CustomMethodDeclaration.emptyParameters
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getValues"),
        typeName,
        ArraySeq(CustomParameterDeclaration(Name("Id"), TypeNames.IdType))
      )
    ).map(m => ((m.name, m.parameters.length), m)).toMap

  private lazy val listCustomSettingsMethods: Map[(Name, Int), MethodDeclaration] =
    Seq(
      CustomMethodDeclaration(
        Location.empty,
        Name("getAll"),
        TypeNames.mapOf(TypeNames.String, typeName),
        CustomMethodDeclaration.emptyParameters
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getInstance"),
        typeName,
        CustomMethodDeclaration.emptyParameters
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getInstance"),
        typeName,
        ArraySeq(CustomParameterDeclaration(Name("Name"), TypeNames.String))
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getValues"),
        typeName,
        ArraySeq(CustomParameterDeclaration(Name("Name"), TypeNames.String))
      )
    ).map(m => ((m.name, m.parameters.length), m)).toMap

  private lazy val customMetadataMethods: Map[(Name, Int), MethodDeclaration] =
    Seq(
      CustomMethodDeclaration(
        Location.empty,
        Name("getAll"),
        TypeNames.mapOf(TypeNames.String, typeName),
        CustomMethodDeclaration.emptyParameters
      ),
      CustomMethodDeclaration(
        Location.empty,
        Name("getInstance"),
        typeName,
        ArraySeq(CustomParameterDeclaration(Name("Name"), TypeNames.String))
      )
    ).map(m => ((m.name, m.parameters.length), m)).toMap

}

object SObjectDeclaration {
  val globalModifiers: ArraySeq[Modifier] = ArraySeq(GLOBAL_MODIFIER)

  val syntheticExtension: Set[Name] = Set(Name("Share"), Name("Feed"), Name("History"))
}
