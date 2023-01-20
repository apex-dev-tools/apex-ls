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

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.names._
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.parsers.SourceData

import scala.collection.compat.immutable.ArraySeq

/** The types of metadata we understand */
sealed abstract class MetadataNature

case object LabelNature         extends MetadataNature
case object ApexNature          extends MetadataNature
case object ApexMetaNature      extends MetadataNature
case object TriggerNature       extends MetadataNature
case object TriggerMetaNature   extends MetadataNature
case object ComponentNature     extends MetadataNature
case object PageNature          extends MetadataNature
case object FlowNature          extends MetadataNature
case object SObjectNature       extends MetadataNature
case object FieldNature         extends MetadataNature
case object FieldSetNature      extends MetadataNature
case object SharingReasonNature extends MetadataNature

/** A piece of Metadata described in a file */
abstract class MetadataDocument(val path: PathLike, val name: Name) {

  /** Type of metadata, this could be stored but we prefer to save space ;-) */
  def nature: MetadataNature

  /** The nature this metadata is considered a part of, e.g. fields are parts of SObjects */
  def controllingNature: MetadataNature = nature

  /** Return true to avoid indexing bad metadata such as empty files */
  def ignorable(): Boolean = false

  /** Typename for this specific metadata document */
  def typeName(namespace: Option[Name]): TypeName

  /** Typename that this metadata document is considered part of, e.g. a field is part of an SObject
    */
  def controllingTypeName(namespace: Option[Name]): TypeName = typeName(namespace)

  /** Obtain source data for this document */
  def source: IssuesAnd[Option[SourceData]] = {
    path.readSourceData() match {
      case Left(err) =>
        IssuesAnd(ArraySeq(Issue(path, Diagnostic(ERROR_CATEGORY, Location.empty, err))), None)
      case Right(data) => IssuesAnd(Some(data))
    }
  }
}

final case class LabelsDocument(_path: PathLike, _name: Name)
    extends MetadataDocument(_path, _name) {

  override def nature: MetadataNature = LabelNature

  override def typeName(namespace: Option[Name]): TypeName = TypeName.Label
}

abstract class ClassDocument(_path: PathLike, _name: Name) extends MetadataDocument(_path, _name)

final case class ApexClassDocument(_path: PathLike, _name: Name)
    extends ClassDocument(_path, _name) {

  override def nature: MetadataNature = ApexNature

  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(name, Seq(), namespace.map(TypeName(_)))
  }
}

object ApexClassDocument {
  def apply(path: PathLike): ApexClassDocument = {
    assert(path.basename.toLowerCase.endsWith(".cls"))
    new ApexClassDocument(path, Name(path.basename.replaceFirst("(?i)\\.cls$", "")))
  }
}

final case class ApexClassMetaDocument(_path: PathLike, _name: Name)
    extends MetadataDocument(_path, _name) {

  override def nature: MetadataNature = ApexMetaNature

  override def controllingNature: MetadataNature = ApexNature

  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(name, Seq(), namespace.map(TypeName(_)))
  }
}

object ApexClassMetaDocument {
  def apply(path: PathLike): ApexClassMetaDocument = {
    assert(path.basename.toLowerCase.endsWith(".cls-meta.xml"))
    new ApexClassMetaDocument(path, Name(path.basename.replaceFirst("(?i)\\.cls-meta.xml$", "")))
  }
}

final case class ApexTriggerDocument(_path: PathLike, _name: Name)
    extends MetadataDocument(_path, _name) {

  override def nature: MetadataNature = TriggerNature

  override def typeName(namespace: Option[Name]): TypeName = {
    val qname: String = namespace
      .map(ns => s"__sfdc_trigger/${ns.value}/${name.value}")
      .getOrElse(s"__sfdc_trigger/${name.value}")
    TypeName(Name(qname))
  }
}

object ApexTriggerDocument {
  def apply(path: PathLike): ApexTriggerDocument = {
    assert(path.basename.toLowerCase.endsWith(".trigger"))
    new ApexTriggerDocument(path, Name(path.basename.replaceFirst("(?i)\\.trigger$", "")))
  }
}

final case class ApexTriggerMetaDocument(_path: PathLike, _name: Name)
    extends ClassDocument(_path, _name) {

  override def nature: MetadataNature = TriggerMetaNature

  override def controllingNature: MetadataNature = TriggerNature

  override def typeName(namespace: Option[Name]): TypeName = {
    val qname: String = namespace
      .map(ns => s"__sfdc_trigger/${ns.value}/${name.value}")
      .getOrElse(s"__sfdc_trigger/${name.value}")
    TypeName(Name(qname))
  }
}

object ApexTriggerMetaDocument {
  def apply(path: PathLike): ApexTriggerMetaDocument = {
    assert(path.basename.toLowerCase.endsWith(".trigger-meta.xml"))
    new ApexTriggerMetaDocument(
      path,
      Name(path.basename.replaceFirst("(?i)\\.trigger-meta.xml$", ""))
    )
  }
}

final case class ComponentDocument(_path: PathLike, _name: Name)
    extends MetadataDocument(_path, _name) {

  override def nature: MetadataNature = ComponentNature

  override def typeName(namespace: Option[Name]): TypeName = {
    namespace
      .map(ns => TypeName(name, Nil, Some(TypeName(ns, Nil, Some(TypeName.Component)))))
      .getOrElse(TypeName(name, Nil, Some(TypeName.Component)))
  }
}

abstract class SObjectLike(_path: PathLike, _name: Name) extends MetadataDocument(_path, _name) {
  override def nature: MetadataNature = SObjectNature
}

final case class SObjectDocument(_path: PathLike, _name: Name) extends SObjectLike(_path, _name) {

  private val isCustom            = name.value.endsWith("__c")
  override val ignorable: Boolean = path.size == 0
  override def typeName(namespace: Option[Name]): TypeName = {
    val prefix =
      if (isCustom)
        namespace.map(ns => s"${ns}__").getOrElse("")
      else
        ""
    TypeName(Name(prefix + name), Nil, Some(TypeName.Schema))
  }
}

object NamespacePrefix {
  def apply(namespace: Option[Name], name: String): Name = {
    Name(namespace.map(ns => s"${ns}__").getOrElse("") + name)
  }
}

abstract class SimpleSObjectLike(_path: PathLike, _name: Name) extends SObjectLike(_path, _name) {
  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(NamespacePrefix(namespace, name.value), Nil, Some(TypeName.Schema))
  }
}

final case class CustomMetadataDocument(_path: PathLike, _name: Name)
    extends SimpleSObjectLike(_path, _name)

final case class BigObjectDocument(_path: PathLike, _name: Name)
    extends SimpleSObjectLike(_path, _name)

final case class PlatformEventDocument(_path: PathLike, _name: Name)
    extends SimpleSObjectLike(_path, _name)

abstract class SObjectPart(_path: PathLike, _name: Name) extends MetadataDocument(_path, _name) {

  override def controllingNature: MetadataNature = SObjectNature

  override def controllingTypeName(namespace: Option[Name]): TypeName = {
    TypeName(sobjectName(namespace), Nil, Some(TypeName.Schema))
  }

  def sobjectName(namespace: Option[Name]): Name = {
    val sobjectName        = path.parent.parent.basename
    val encodedSObjectName = EncodedName(sobjectName)
    val isStandard         = encodedSObjectName.ext.isEmpty
    val prefix =
      if (isStandard)
        ""
      else
        namespace.map(ns => s"${ns}__").getOrElse("")
    Name(prefix + sobjectName)
  }
}

final case class SObjectFieldDocument(_path: PathLike, _name: Name)
    extends SObjectPart(_path, _name) {

  override def nature: MetadataNature = FieldNature

  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(
      NamespacePrefix(namespace, name.value),
      Nil,
      Some(
        TypeName.sObjectTypeFields$(TypeName(sobjectName(namespace), Nil, Some(TypeName.Schema)))
      )
    )
  }
}

final case class SObjectFieldSetDocument(_path: PathLike, _name: Name)
    extends SObjectPart(_path, _name) {

  override def nature: MetadataNature = FieldSetNature

  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(
      NamespacePrefix(namespace, name.value),
      Nil,
      Some(
        TypeName.sObjectTypeFieldSets$(TypeName(sobjectName(namespace), Nil, Some(TypeName.Schema)))
      )
    )
  }
}

final case class SObjectSharingReasonDocument(_path: PathLike, _name: Name)
    extends SObjectPart(_path, _name) {

  override def nature: MetadataNature = SharingReasonNature

  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(
      NamespacePrefix(namespace, name.value),
      Nil,
      Some(
        TypeName.sObjectTypeRowClause$(TypeName(sobjectName(namespace), Nil, Some(TypeName.Schema)))
      )
    )
  }
}

final case class PageDocument(_path: PathLike, _name: Name) extends MetadataDocument(_path, _name) {

  override def nature: MetadataNature = PageNature

  override def typeName(namespace: Option[Name]): TypeName = {
    TypeName(NamespacePrefix(namespace, name.value), Nil, Some(TypeName.Page))
  }
}

final case class FlowDocument(_path: PathLike, _name: Name) extends MetadataDocument(_path, _name) {

  override def nature: MetadataNature = FlowNature

  override def typeName(namespace: Option[Name]): TypeName = {
    namespace
      .map(ns => TypeName(name, Nil, Some(TypeName(ns, Nil, Some(TypeName.Interview)))))
      .getOrElse(TypeName(name, Nil, Some(TypeName.Interview)))
  }
}

object MetadataDocument {
  // These are slightly over general, additional constraints are applied below
  private val extensions: Seq[String] = Seq(
    "cls",
    "cls-meta.xml",
    "trigger",
    "trigger-meta.xml",
    "component",
    "object",
    "object-meta.xml",
    "field",
    "field-meta.xml",
    "fieldSet",
    "fieldSet-meta.xml",
    "sharingReason",
    "sharingReason-meta.xml",
    "flow",
    "flow-meta.xml",
    "labels",
    "labels-meta.xml",
    "page"
  )

  def extensionsGlob: String = s"{${extensions.mkString(",")}}"

  def apply(path: PathLike): Option[MetadataDocument] = {
    var parts = path.basename.split('.')

    // If we have over split, likely due to '.' in name, try to recombine
    while (parts.length > 3 || (parts.length == 3 && parts(2) != "xml")) {
      parts = Array(s"${parts.head}.${parts(1)}") ++ parts.takeRight(parts.length - 2)
    }

    // Used to test correct structuring of SObjects & components of
    lazy val isObjectsChild           = path.parent.basename == "objects"
    lazy val isObjectsGrandChild      = path.parent.parent.basename == "objects"
    lazy val isObjectsGrandGrandChild = path.parent.parent.parent.basename == "objects"
    lazy val isFieldsChild            = isObjectsGrandGrandChild && path.parent.basename == "fields"
    lazy val isFieldSetChild = isObjectsGrandGrandChild && path.parent.basename == "fieldSets"
    lazy val isSharingReasonChild =
      isObjectsGrandGrandChild && path.parent.basename == "sharingReasons"

    // For some docs the name just comes from the filename, for SObjects we use enclosing directory name so
    // we can detect errors between that, the filename and the fullName in the doc itself.
    val name = Name(parts.head)

    if (parts.length == 2) {
      lazy val sObjectName = Name(parts.head)
      parts(1) match {
        case "cls"     => Some(ApexClassDocument(path, name))
        case "trigger" => Some(ApexTriggerDocument(path, name))
        case "object" if isObjectsChild && name.value.endsWith("__mdt") =>
          Some(CustomMetadataDocument(path, sObjectName))
        case "object" if isObjectsChild && name.value.endsWith("__b") =>
          Some(BigObjectDocument(path, sObjectName))
        case "object" if isObjectsChild && name.value.endsWith("__e") =>
          Some(PlatformEventDocument(path, sObjectName))
        case "object" if isObjectsChild => Some(SObjectDocument(path, sObjectName))
        case "component"                => Some(ComponentDocument(path, name))
        case "flow"                     => Some(FlowDocument(path, name))
        case "labels"                   => Some(LabelsDocument(path, name))
        case "page"                     => Some(PageDocument(path, name))
        case _                          => None
      }
    } else if (parts.length == 3 && parts(2) == "xml") {
      lazy val sObjectName = Name(path.parent.basename)
      parts(1) match {
        case "cls-meta"     => Some(ApexClassMetaDocument(path, name))
        case "trigger-meta" => Some(ApexTriggerMetaDocument(path, name))
        case "field-meta" if isFieldsChild =>
          Some(SObjectFieldDocument(path, name))
        case "fieldSet-meta" if isFieldSetChild =>
          Some(SObjectFieldSetDocument(path, name))
        case "sharingReason-meta" if isSharingReasonChild =>
          Some(SObjectSharingReasonDocument(path, name))
        case "object-meta" if isObjectsGrandChild && name.value.endsWith("__mdt") =>
          Some(CustomMetadataDocument(path, sObjectName))
        case "object-meta" if isObjectsGrandChild && name.value.endsWith("__b") =>
          Some(BigObjectDocument(path, sObjectName))
        case "object-meta" if isObjectsGrandChild && name.value.endsWith("__e") =>
          Some(PlatformEventDocument(path, sObjectName))
        case "object-meta" if isObjectsGrandChild => Some(SObjectDocument(path, sObjectName))
        case "flow-meta"                          => Some(FlowDocument(path, name))
        case "labels-meta"                        => Some(LabelsDocument(path, name))
        case _                                    => None
      }
    } else {
      None
    }
  }
}
