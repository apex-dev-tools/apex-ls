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
package com.nawforce.pkgforce.modifiers

import com.nawforce.pkgforce.diagnostics.{LogEntryContext, ModifierLogger}
import com.nawforce.pkgforce.modifiers.ApexModifiers.{
  allowableModifiers,
  asModifiers,
  toModifiers,
  visibilityModifiers
}
import com.nawforce.runtime.parsers.CodeParser
import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import io.github.apexdevtools.apexparser.ApexParser.ModifierContext

import scala.collection.compat.immutable.ArraySeq

sealed abstract class MethodOwnerNature(final val name: String) {
  override def toString: String = name
}

case object FINAL_METHOD_NATURE           extends MethodOwnerNature("final class")
case object VIRTUAL_METHOD_NATURE         extends MethodOwnerNature("virtual class")
case object ABSTRACT_METHOD_NATURE        extends MethodOwnerNature("abstract class")
case object GLOBAL_ABSTRACT_METHOD_NATURE extends MethodOwnerNature("global abstract class")
case object INTERFACE_METHOD_NATURE       extends MethodOwnerNature("interface")
case object ENUM_METHOD_NATURE            extends MethodOwnerNature("enum")

object MethodModifiers {

  private val MethodModifiers: Set[Modifier] = visibilityModifiers.toSet ++ Set(
    ABSTRACT_MODIFIER,
    OVERRIDE_MODIFIER,
    STATIC_MODIFIER,
    TEST_METHOD_MODIFIER,
    WEBSERVICE_MODIFIER,
    VIRTUAL_MODIFIER
  )

  private val MethodAnnotations: Set[Modifier] = Set(
    AURA_ENABLED_ANNOTATION,
    DEPRECATED_ANNOTATION,
    FUTURE_ANNOTATION,
    INVOCABLE_METHOD_ANNOTATION,
    ISTEST_ANNOTATION,
    TEST_VISIBLE_ANNOTATION,
    NAMESPACE_ACCESSIBLE_ANNOTATION,
    READ_ONLY_ANNOTATION,
    SUPPRESS_WARNINGS_ANNOTATION_PMD,
    SUPPRESS_WARNINGS_ANNOTATION_UNUSED,
    TEST_SETUP_ANNOTATION,
    HTTP_DELETE_ANNOTATION,
    HTTP_GET_ANNOTATION,
    HTTP_PATCH_ANNOTATION,
    HTTP_POST_ANNOTATION,
    HTTP_PUT_ANNOTATION,
    REMOTE_ACTION_ANNOTATION
  )

  private val MethodModifiersAndAnnotations: Set[Modifier] = MethodAnnotations ++ MethodModifiers

  def classMethodModifiers(
    parser: CodeParser,
    modifierContexts: ArraySeq[ModifierContext],
    context: ParserRuleContext,
    ownerNature: MethodOwnerNature,
    isOuter: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(parser, modifierContexts)
    classMethodModifiers(logger, mods, LogEntryContext(parser, context), ownerNature, isOuter)
  }

  def classMethodModifiers(
    logger: ModifierLogger,
    modifiers: ArraySeq[(Modifier, LogEntryContext, String)],
    context: LogEntryContext,
    ownerNature: MethodOwnerNature,
    isOuter: Boolean
  ): ModifierResults = {

    val normalModifiers = ApexModifiers.deduplicateVisibility(
      asModifiers(
        allowableModifiers(modifiers, MethodModifiersAndAnnotations, "methods", logger),
        logger,
        context
      ),
      "methods",
      logger,
      context
    )

    val explicitVisibility = normalModifiers
      .intersect(visibilityModifiers)
      .headOption

    val extendedModifiers =
      (if (explicitVisibility.isEmpty) {
         ArraySeq(
           if (normalModifiers.contains(WEBSERVICE_MODIFIER))
             GLOBAL_MODIFIER
           else PRIVATE_MODIFIER
         )
       } else ArraySeq()) ++ normalModifiers

    val visibility = extendedModifiers
      .intersect(visibilityModifiers)
      .head

    val results = {
      if (visibility != GLOBAL_MODIFIER && extendedModifiers.contains(WEBSERVICE_MODIFIER)) {
        logger.logError(context, s"webservice methods must be global")
        GLOBAL_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else if (!isOuter && extendedModifiers.contains(WEBSERVICE_MODIFIER)) {
        logger.logError(context, s"webservice methods can only be declared on outer classes")
        GLOBAL_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else if (
        extendedModifiers
          .contains(VIRTUAL_MODIFIER) && extendedModifiers.contains(ABSTRACT_MODIFIER)
      ) {
        logger.logError(context, s"abstract methods are virtual methods")
        extendedModifiers.filterNot(_ == VIRTUAL_MODIFIER)
      } else if (
        extendedModifiers.contains(
          ABSTRACT_MODIFIER
        ) && !(ownerNature == ABSTRACT_METHOD_NATURE || ownerNature == GLOBAL_ABSTRACT_METHOD_NATURE)
      ) {
        logger.logError(context, s"abstract methods can only be declared on abstract classes")
        extendedModifiers
      } else if (
        ownerNature == GLOBAL_ABSTRACT_METHOD_NATURE && visibility != GLOBAL_MODIFIER &&
        extendedModifiers.contains(ABSTRACT_MODIFIER)
      ) {
        logger.logError(context, s"abstract methods must be global in global abstract classes")
        GLOBAL_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else {
        extendedModifiers
      }
    }
    ModifierResults(results, logger.issues).intern
  }

  def interfaceMethodModifiers(
    parser: CodeParser,
    modifierContexts: ArraySeq[ModifierContext],
    context: ParserRuleContext
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(parser, modifierContexts)
    interfaceMethodModifiers(logger, mods, LogEntryContext(parser, context))
  }

  def interfaceMethodModifiers(
    logger: ModifierLogger,
    modifiers: ArraySeq[(Modifier, LogEntryContext, String)],
    context: LogEntryContext
  ): ModifierResults = {

    val allowedModifiers = allowableModifiers(modifiers, Set.empty, "interface methods", logger)

    val mods = ApexModifiers.deduplicateVisibility(
      asModifiers(allowedModifiers, logger, context),
      "methods",
      logger,
      context
    )

    ModifierResults(mods ++ ArraySeq(VIRTUAL_MODIFIER, PUBLIC_MODIFIER), logger.issues).intern
  }
}
