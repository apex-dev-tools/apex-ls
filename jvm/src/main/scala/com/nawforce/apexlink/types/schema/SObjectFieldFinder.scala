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
import com.nawforce.apexlink.org.OPM.Module
import com.nawforce.apexlink.types.core.FieldDeclaration
import com.nawforce.apexlink.types.schema.SObjectFieldFinder.relationshipExtension
import com.nawforce.apexlink.types.synthetic.{CustomField, CustomFieldDeclaration}
import com.nawforce.pkgforce.names._

import scala.collection.immutable.ArraySeq

/* SObject field finding support. This is separated so it can be used by TypeDeclarations that can be used to
 * represent SObjects, the current cases being PlatformTypeDeclaration for standard objects and SObjectDeclarations
 * used for Custom SObjects and Standard SObjects when they are loaded into modules. It provides the handling needed
 * to support reflection of the fields that is unique to SObjects.
 * FUTURE: Remove this as part of refactor work on TypeDeclarations
 */
trait SObjectFieldFinder {
  this: SObjectLikeDeclaration =>

  val typeName: TypeName
  val fields: ArraySeq[FieldDeclaration]

  def findSObjectField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    findFieldSObject(name, staticContext, findField(name))
      .orElse {
        findFieldSObject(name, staticContext, getRelationshipField(name))
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

  private def getRelationshipField(name: Name): Option[FieldDeclaration] = {
    val encodedName = EncodedName(name).defaultNamespace(module.namespace)
    if (encodedName.ext.contains(relationshipExtension)) {
      getRelationshipField(this, encodedName)
        .orElse(
          getTypeDependencyHolders.toIterable.view
            .flatMap(typeId => {
              getSObjectFieldFinder(typeId.module, typeId.typeName)
                .flatMap(sobject => getRelationshipField(sobject, encodedName))
            })
            .headOption
        )
        .orElse(
          this.moduleDeclaration
            .flatMap(_.nextModule)
            .flatMap(nextModule => {
              getSObjectFieldFinder(nextModule, typeName)
                .flatMap(_.getRelationshipField(name))
            })
        )
    } else {
      None
    }
  }

  private def getSObjectFieldFinder(
    module: Module,
    typeName: TypeName
  ): Option[SObjectFieldFinder] = {
    TypeResolver(typeName, module).toOption
      .collect { case sobject: SObjectFieldFinder => sobject }
  }

  private def getRelationshipField(
    sobject: SObjectFieldFinder,
    encodedName: EncodedName
  ): Option[FieldDeclaration] = {
    sobject.fields
      .collect { case field: CustomField if field.relationshipName.nonEmpty => field }
      .find(field => {
        EncodedName(
          Name(field.relationshipName.get),
          Some(relationshipExtension),
          EncodedName(field.name).namespace
        ) == encodedName && field.typeName == typeName
      })
      .map(_ =>
        CustomFieldDeclaration(encodedName.fullName, TypeNames.recordSetOf(sobject.typeName), None)
      )
  }
}

object SObjectFieldFinder {
  val relationshipExtension: Name = Name("r")
}
