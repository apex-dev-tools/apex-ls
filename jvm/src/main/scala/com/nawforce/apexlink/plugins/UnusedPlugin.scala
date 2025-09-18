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
package com.nawforce.apexlink.plugins

import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.plugins.UnusedPlugin._
import com.nawforce.apexlink.types.apex.{ApexFieldLike, ApexMethodLike, FullDeclaration}
import com.nawforce.apexlink.types.core.{
  Dependent,
  DependentType,
  MethodDeclaration,
  TypeDeclaration
}
import com.nawforce.pkgforce.diagnostics.{Diagnostic, DiagnosticCategory, Issue, UNUSED_CATEGORY}
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, ENUM_NATURE, FIELD_NATURE, PROPERTY_NATURE}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Provides plugin for generating unused warnings on a single type
  * @param td type being handled by this plugin
  */
class UnusedPlugin(td: DependentType) extends Plugin(td) {

  override def onClassValidated(td: ClassDeclaration): Seq[DependentType] = reportUnused(td)

  override def onEnumValidated(td: EnumDeclaration): Seq[DependentType] = reportUnused(td)

  override def onInterfaceValidated(td: InterfaceDeclaration): Seq[DependentType] = reportUnused(td)

  private def reportUnused(td: FullDeclaration): Seq[DependentType] = {
    // Ignore if suppressed, or inner type (handled by unusedIssues)
    if (td.modifiers.exists(suppressModifiers.contains) || td.outerTypeName.isDefined) {
      Seq.empty
    } else {
      // Only update if we don't have errors, to reduce noise
      val existingIssues = td.paths.flatMap(td.module.pkg.org.issues.issuesForFileInternal)
      val hasErrors =
        existingIssues.exists(issue => DiagnosticCategory.isErrorType(issue.diagnostic.category))
      if (hasErrors) {
        td.module.pkg.org.issues.replaceUnusedIssues(td.paths.head, Seq())
      } else {
        // This is a bit messy, we need to preserve unused locals are they are pre-computed
        // via onBlockValidate. They need to be handled that way for local suppression to work.
        val localUnused =
          existingIssues.filter(_.diagnostic.message.startsWith("Unused local variable"))
        td.module.pkg.org.issues.replaceUnusedIssues(td.paths.head, td.unusedIssues ++ localUnused)
      }

      // Return all our dependents so they are re-validated for unused as well
      val dependents = mutable.Set[Dependent]()
      td.collectDependencies(dependents)
      dependents
        .collect { case td: TypeDeclaration => td }
        .map(_.outermostTypeDeclaration)
        .collect { case dt: DependentType => dt }
        .toSeq
    }
  }

  override def onBlockValidated(
    block: Block,
    isStatic: Boolean,
    context: BlockVerifyContext
  ): Unit = {
    if (context.modifiers(suppressModifiers.contains))
      return

    context.declaredVars
      .filter(localVar =>
        !context.referencedVars.contains(localVar._1) && localVar._2.definition.nonEmpty
      )
      .foreach(localVar => {
        val definition = localVar._2.definition.get
        context.log(
          new Issue(
            definition.location.path,
            Diagnostic(
              UNUSED_CATEGORY,
              definition.location.location,
              s"Unused local variable '${localVar._1}'"
            )
          )
        )
      })
  }

  private implicit class DeclarationOps(td: FullDeclaration) {

    /** Generates unused issues for a type, see doc/Unused.md for details.
      *
      * @return the issues
      */
    def unusedIssues: ArraySeq[Issue] = {

      // Hack: Unused calculation requires a methodMap as it establishes shadow relationships
      td.methodMap

      // Ignore page controllers, although we parse VF we don't establish use relationships yet
      if (td.isPageController)
        return ArraySeq.empty

      // Get body declaration issues, we exclude initializers as they are really part of the type
      val issues =
        td.nestedTypes.flatMap(ad => {
          if (ad.modifiers.exists(suppressModifiers.contains)) ArraySeq.empty else ad.unusedIssues
        }) ++
          td.unusedFields ++
          td.unusedMethods

      // Bail early if we found nothing of interest, hopefully the common case
      val childCount = td.nestedTypes.length + td.localFields.length + td.localMethods.length
      if (issues.isEmpty && childCount > 0)
        return ArraySeq.empty

      // Check if need to promote the used to the type level to reduce noise in output
      if (canPromoteUnusedToType(issues.length)) {
        val onlyTestReferenced = td.hasHolders || (issues.nonEmpty && issues
          .forall(_.diagnostic.message.contains(onlyTestCodeReferenceText)))

        // Classes should likely be @isTest, but you can't use that on enum/interfaces
        val suffix = new StringBuilder()
        if (onlyTestReferenced) {
          suffix.append(", only referenced by test code")
          td.nature match {
            case CLASS_NATURE =>
              suffix.append(", consider using @isTest or @SuppressWarnings('Unused') if needed")
            case _ => suffix.append(", consider using @SuppressWarnings('Unused') if needed")
          }
        }

        ArraySeq(
          new Issue(
            td.location.path,
            Diagnostic(
              UNUSED_CATEGORY,
              td.idLocation,
              s"Unused ${td.nature.value} '${td.typeName}'$suffix"
            )
          )
        )
      } else {
        issues
      }
    }

    private def canPromoteUnusedToType(issueCount: Int): Boolean = {
      // If the type has holders itself then we need to report on each body declaration
      val hasHolders = if (td.inTest) td.hasHolders else td.hasNonTestHolders
      if (hasHolders)
        return false

      // Don't promote for global as these are implicitly used
      if (td.visibility.getOrElse(PRIVATE_MODIFIER) == GLOBAL_MODIFIER)
        return false

      // Exclude reporting on empty outers, that is just a bit harsh
      val childCount = td.nestedTypes.length + td.localFields.length +
        (if (td.nature == ENUM_NATURE) 0 else td.localMethods.length)
      if (td.outerTypeName.isEmpty && childCount == 0)
        return false

      // Finally if we got issues for each child say yes
      childCount == issueCount
    }

    def unusedFields: ArraySeq[Issue] = {
      td.localFields
        .filterNot(_.isUsed(td.inTest))
        .map(field => {
          val nature = field.nature match {
            case FIELD_NATURE    => "field"
            case PROPERTY_NATURE => "property"
            case _               => "field or property"
          }
          val suffix = if (field.hasHolders) s", $onlyTestCodeReferenceText" else ""
          new Issue(
            field.location.path,
            Diagnostic(UNUSED_CATEGORY, field.idLocation, s"Unused $nature '${field.name}'$suffix")
          )
        })
    }

    def unusedMethods: ArraySeq[Issue] = {
      td.localMethods
        .flatMap {
          case am: ApexMethodLike if !am.isUsed(td.module, td.inTest) => Some(am)
          case _                                                      => None
        }
        .map(method => {
          val suffix = if (method.hasHolders) s", $onlyTestCodeReferenceText" else ""
          new Issue(
            method.location.path,
            Diagnostic(
              UNUSED_CATEGORY,
              method.idLocation,
              s"Unused ${method.visibility.getOrElse(PRIVATE_MODIFIER).name} method '${method.signature}'$suffix"
            )
          )
        })
    }

  }

  private implicit class FieldOps(field: ApexFieldLike) {
    def isUsed(inTest: Boolean): Boolean = {
      if (inTest)
        field.hasHolders || field.modifiers.exists(excludedTestFieldModifiers)
      else
        field.hasNonTestHolders || field.modifiers.exists(excludedFieldModifiers)
    }
  }

  private implicit class MethodOps(method: ApexMethodLike) {

    /** Is the method in use, NOTE: requires a MethodMap is constructed for shadow support first! */
    def isUsed(module: OPM.Module, inTest: Boolean): Boolean = {
      method.isSynthetic ||
      (if (inTest)
         method.hasHolders || method.modifiers.exists(excludedTestMethodModifiers.contains)
       else
         method.hasNonTestHolders || method.modifiers.exists(excludedMethodModifiers.contains)) ||
      method.shadows.exists({
        case am: ApexMethodLike   => am.isUsed(module, inTest)
        case _: MethodDeclaration => true
        case _                    => false
      }) ||
      method.parameters.exists(parameter => module.isGhostedType(parameter.typeName))
    }
  }
}

object UnusedPlugin {
  val onlyTestCodeReferenceText =
    "only referenced by test code, remove or make private @TestVisible"

  val suppressModifiers: Set[Modifier] =
    Set(SUPPRESS_WARNINGS_ANNOTATION_PMD, SUPPRESS_WARNINGS_ANNOTATION_UNUSED)

  val excludedMethodModifiers: Set[Modifier] =
    Set(
      TEST_VISIBLE_ANNOTATION,
      GLOBAL_MODIFIER,
      AURA_ENABLED_ANNOTATION,
      SUPPRESS_WARNINGS_ANNOTATION_PMD,
      SUPPRESS_WARNINGS_ANNOTATION_UNUSED
    )
  val excludedTestMethodModifiers: Set[Modifier] =
    Set(
      ISTEST_ANNOTATION,
      TEST_SETUP_ANNOTATION,
      TEST_METHOD_MODIFIER,
      SUPPRESS_WARNINGS_ANNOTATION_PMD,
      SUPPRESS_WARNINGS_ANNOTATION_UNUSED
    )
  val excludedFieldModifiers: Set[Modifier] =
    Set(
      TEST_VISIBLE_ANNOTATION,
      GLOBAL_MODIFIER,
      AURA_ENABLED_ANNOTATION,
      SUPPRESS_WARNINGS_ANNOTATION_PMD,
      SUPPRESS_WARNINGS_ANNOTATION_UNUSED
    )
  val excludedTestFieldModifiers: Set[Modifier] =
    Set(SUPPRESS_WARNINGS_ANNOTATION_PMD, SUPPRESS_WARNINGS_ANNOTATION_UNUSED)
}
