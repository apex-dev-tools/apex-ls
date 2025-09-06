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

sealed abstract class MethodOwnerInfo
case object InterfaceOwnerInfo extends MethodOwnerInfo
case object EnumOwnerInfo      extends MethodOwnerInfo
case class ClassOwnerInfo(modifiers: ArraySeq[Modifier], isExtending: Boolean)
    extends MethodOwnerInfo

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

  private val MethodModifiersAndAnnotations = MethodAnnotations ++ MethodModifiers

  private val globalAbstractModifiers  = ArraySeq(GLOBAL_MODIFIER, ABSTRACT_MODIFIER)
  private val virtualAbstractModifiers = ArraySeq(VIRTUAL_MODIFIER, ABSTRACT_MODIFIER)

  def classMethodModifiers(
    parser: CodeParser,
    modifierContexts: ArraySeq[ModifierContext],
    context: ParserRuleContext,
    ownerInfo: ClassOwnerInfo,
    isOuter: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(parser, modifierContexts)
    classMethodModifiers(logger, mods, LogEntryContext(parser, context), ownerInfo, isOuter)
  }

  def classMethodModifiers(
    logger: ModifierLogger,
    modifiers: ArraySeq[(Modifier, LogEntryContext, String)],
    context: LogEntryContext,
    ownerInfo: ClassOwnerInfo,
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

    var visibility = normalModifiers
      .intersect(visibilityModifiers)
      .headOption

    val extendedModifiers =
      (if (visibility.isEmpty && normalModifiers.contains(WEBSERVICE_MODIFIER)) {
         visibility = Some(GLOBAL_MODIFIER)
         ArraySeq(GLOBAL_MODIFIER)
       } else ArraySeq()) ++ normalModifiers

    val isGlobal = visibility.contains(GLOBAL_MODIFIER)
    val results = {
      if (!isGlobal && extendedModifiers.contains(WEBSERVICE_MODIFIER)) {
        logger.logError(context, "Webservice methods must be global")
        GLOBAL_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else if (!isOuter && extendedModifiers.contains(WEBSERVICE_MODIFIER)) {
        logger.logError(context, "Webservice methods can only be declared on outer classes")
        GLOBAL_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else if (
        extendedModifiers
          .contains(VIRTUAL_MODIFIER) && extendedModifiers.contains(ABSTRACT_MODIFIER)
      ) {
        logger.logError(context, "Abstract methods are virtual methods")
        extendedModifiers.filterNot(_ == VIRTUAL_MODIFIER)
      } else if (
        extendedModifiers
          .contains(ABSTRACT_MODIFIER) && !ownerInfo.modifiers.contains(ABSTRACT_MODIFIER)
      ) {
        logger.logError(context, "Abstract methods can only be declared on abstract classes")
        extendedModifiers
      } else if (
        ownerInfo.modifiers
          .intersect(globalAbstractModifiers)
          .length == globalAbstractModifiers.length &&
        !isGlobal &&
        extendedModifiers.contains(ABSTRACT_MODIFIER)
      ) {
        logger.logError(context, "Abstract methods must be global in global abstract classes")
        GLOBAL_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else if (
        visibility.contains(PROTECTED_MODIFIER) &&
        !extendedModifiers.contains(STATIC_MODIFIER) && // Static error is caught later
        !ownerInfo.isExtending &&
        ownerInfo.modifiers.intersect(virtualAbstractModifiers).isEmpty
      ) {
        logger.logError(
          context,
          "Protected methods can only be used on virtual or abstract classes"
        )
        PUBLIC_MODIFIER +: extendedModifiers.diff(visibilityModifiers)
      } else if (
        (visibility.isEmpty || visibility.contains(PRIVATE_MODIFIER)) &&
        extendedModifiers.intersect(virtualAbstractModifiers).nonEmpty
      ) {
        logger.logWarning(
          context,
          "Private method overrides have inconsistent behaviour, use global, public or protected"
        )
        extendedModifiers
      } else if (
        (extendedModifiers
          .contains(ISTEST_ANNOTATION) || extendedModifiers.contains(TEST_METHOD_MODIFIER)) &&
        !extendedModifiers.contains(STATIC_MODIFIER)
      ) {
        logger.logError(context, "testMethod and @IsTest methods must be static")
        STATIC_MODIFIER +: extendedModifiers
      } else if (
        (extendedModifiers
          .contains(ISTEST_ANNOTATION) || extendedModifiers.contains(TEST_SETUP_ANNOTATION)) &&
        !ownerInfo.modifiers.contains(ISTEST_ANNOTATION)
      ) {
        logger.logError(
          context,
          "Method with @IsTest or @TestSetup annotation must be in a class with @IsTest annotation"
        )
        extendedModifiers
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
