package com.nawforce.pkgforce.modifiers

import com.nawforce.apexparser.ApexParser.{IdContext, ModifierContext}
import com.nawforce.pkgforce.diagnostics.{LogEntryContext, ModifierLogger}
import com.nawforce.pkgforce.modifiers.ApexModifiers.{
  asModifiers,
  allowableModifiers,
  deduplicateVisibility,
  toModifiers,
  visibilityModifiers
}
import com.nawforce.runtime.parsers.CodeParser

import scala.collection.compat.immutable.ArraySeq

object FieldModifiers {
  private val FieldModifiers: Set[Modifier] =
    visibilityModifiers.toSet ++ Set(
      FINAL_MODIFIER,
      STATIC_MODIFIER,
      TRANSIENT_MODIFIER,
      WEBSERVICE_MODIFIER
    )

  private val FieldAnnotations: Set[Modifier] =
    Set(
      AURA_ENABLED_ANNOTATION,
      DEPRECATED_ANNOTATION,
      INVOCABLE_VARIABLE_ANNOTATION,
      TEST_VISIBLE_ANNOTATION,
      SUPPRESS_WARNINGS_ANNOTATION_PMD,
      SUPPRESS_WARNINGS_ANNOTATION_UNUSED
    )

  private val FieldModifiersAndAnnotations: Set[Modifier] = FieldAnnotations ++ FieldModifiers

  private val InnerFieldModifiersAndAnnotations: Set[Modifier] =
    FieldModifiersAndAnnotations - STATIC_MODIFIER

  def fieldModifiers(
    parser: CodeParser,
    modifierContexts: ArraySeq[ModifierContext],
    outer: Boolean,
    idContext: IdContext
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(parser, modifierContexts)
    fieldModifiers(logger, mods, outer, LogEntryContext(parser, idContext))
  }

  def fieldModifiers(
    logger: ModifierLogger,
    modifiers: ArraySeq[(Modifier, LogEntryContext, String)],
    outer: Boolean,
    idContext: LogEntryContext
  ): ModifierResults = {

    val allowedModifiers = allowableModifiers(
      modifiers,
      if (outer) FieldModifiersAndAnnotations else InnerFieldModifiersAndAnnotations,
      if (outer) "fields" else "inner class fields",
      logger
    )

    val mods = deduplicateVisibility(
      asModifiers(allowedModifiers, logger, idContext),
      "fields",
      logger,
      idContext
    )

    val results = {
      if (mods.intersect(visibilityModifiers).isEmpty && mods.contains(WEBSERVICE_MODIFIER)) {
        GLOBAL_MODIFIER +: mods
      } else if (
        !mods.intersect(visibilityModifiers).contains(GLOBAL_MODIFIER) && mods.contains(
          WEBSERVICE_MODIFIER
        )
      ) {
        logger.logError(idContext, s"webservice fields must be global")
        GLOBAL_MODIFIER +: mods.diff(visibilityModifiers)
      } else if (mods.intersect(visibilityModifiers).isEmpty) {
        PRIVATE_MODIFIER +: mods
      } else {
        mods
      }
    }
    ModifierResults(results, logger.issues).intern
  }

}
