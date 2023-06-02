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

import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames._
import com.nawforce.apexlink.types.core.{FieldDeclaration, TypeDeclaration, TypeId}
import com.nawforce.apexlink.types.synthetic.{CustomField, CustomFieldDeclaration}
import com.nawforce.pkgforce.names._

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/* SObject field finding support. This is separated so it can be used by TypeDeclarations that can be used to
 * represent SObjects, the current cases being PlatformTypeDeclaration for standard objects and SObjectDeclarations
 * used for Custom SObjects and Standard SObjects when they are loaded into modules. It provides the handling needed
 * to support reflection of the fields that is unique to SObjects.
 * FUTURE: Remove this as part of refactor work on TypeDeclarations
 */
trait SObjectFieldFinder {
  this: TypeDeclaration =>

  val fields: ArraySeq[FieldDeclaration]
  private var relationshipFields: mutable.Map[Name, FieldDeclaration] = _

  def validate(withRelationshipCollection: Boolean): Unit

  def collectRelationshipFields(dependencyHolders: Set[TypeId]): Unit = {
    // Find SObject relationship fields that reference this one via dependency analysis
    val incomingObjects = this +: dependencyHolders.toSeq
      .flatMap(typeId => TypeResolver(typeId.typeName, typeId.module).toOption)
      .collect { case sobject: SObjectFieldFinder => sobject }
    val incomingObjectAndField: Seq[(TypeDeclaration, CustomField)] =
      incomingObjects.flatMap(sobject => {
        sobject.fields
          .collect { case field: CustomField => field }
          .filter(field =>
            field.relationshipName.nonEmpty &&
              field.typeName == typeName &&
              field.name.value
                .endsWith("__r")
          )
          .map(field => (sobject.asInstanceOf[TypeDeclaration], field))
      })

    relationshipFields = this.moduleDeclaration
      .flatMap(_.nextModule)
      .flatMap(nextModule => {
        TypeResolver(typeName, nextModule).toOption
          .collect { case sobject: SObjectFieldFinder => sobject }
          .flatMap(sobject => Option(sobject.relationshipFields))
      })
      .getOrElse(mutable.Map())

    incomingObjectAndField.foreach(incoming => {
      val relationshipName = Name(incoming._2.relationshipName.get + "__r")
      val ns               = this.moduleDeclaration.flatMap(_.namespace)
      val targetFieldName =
        EncodedName(relationshipName).defaultNamespace(ns).fullName
      relationshipFields.put(
        targetFieldName,
        CustomFieldDeclaration(targetFieldName, TypeNames.recordSetOf(incoming._1.typeName), None)
      )
    })
  }

  def findSObjectField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    findFieldSObject(name, staticContext, findField(name))
      .orElse {
        Option(relationshipFields).flatMap(fields => {
          findFieldSObject(name, staticContext, fields.get(name))
        })
      }
  }

  private def findFieldSObject(
    name: Name,
    staticContext: Option[Boolean],
    sObjectField: Option[FieldDeclaration]
  ): Option[FieldDeclaration] = {
    // Handle the synthetic static SObjectField or abort
    if (sObjectField.isEmpty) {
      if (name == Names.SObjectField && staticContext.contains(true))
        Some(CustomFieldDeclaration(Names.SObjectField, TypeNames.sObjectFields$(typeName), None))
      else
        None
    } else {
      val field = sObjectField.get
      if (staticContext.contains(field.isStatic)) {
        // Found a matching field
        sObjectField
      } else if (staticContext.contains(true)) {
        // Create an SObjectField version of this field
        val shareTypeName = if (typeName.isShare) Some(typeName) else None
        Some(field.getSObjectStaticField(shareTypeName, moduleDeclaration))
      } else {
        None
      }
    }
  }
}
