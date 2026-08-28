/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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

import com.nawforce.pkgforce.path.Location

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

/** Validation of the parameters written on an annotation.
  *
  * Diagnostics anchor on the parameter at fault rather than on the annotation, the parameter list
  * carries the locations needed for that. An annotation missing from [[AnnotationDefinition]] is
  * left alone, an unknown annotation is never an error.
  */
object AnnotationValidation {

  def validate(
    name: String,
    parameters: Option[ArraySeq[AnnotationParameter]],
    target: AnnotationTarget,
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    /* The separator is a property of Apex's annotation syntax rather than of any one annotation,
     * so it is checked whatever was written. Reporting a comma inside the parameter list of an
     * annotation we do not know claims nothing about whether that annotation exists. */
    parameters.foreach(written => validateSeparators(written, context, logger))

    /* A missing parameter list is validated as an empty one, the platform rejects a bare
     * annotation that requires a parameter just as it rejects the empty parenthesised form. */
    AnnotationDefinition(name).foreach(definition =>
      validateParameters(
        definition,
        parameters.getOrElse(AnnotationParameter.emptyArraySeq),
        target,
        context,
        logger
      )
    )
  }

  private def validateParameters(
    definition: AnnotationDefinition,
    parameters: ArraySeq[AnnotationParameter],
    target: AnnotationTarget,
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    validateRequirement(definition, parameters, context, logger)

    /* Only the combination rules need the values, and most annotations have none. */
    val values =
      if (definition.combinations.isEmpty) None
      else Some(new mutable.LinkedHashMap[String, String]())

    lastWritten(parameters).foreach(parameter =>
      if (!isEmptyParameter(parameter))
        validateParameter(definition, parameter, target, context, logger)
          .foreach { case (property, value) =>
            values.foreach(written => written.update(property.key, value))
          }
    )

    values.foreach(written =>
      validateCombinations(definition, parameters, written.toMap, context, logger)
    )
  }

  /* A required parameter is decided by what was written, before anything is said about whether
   * the values are legal. Writing some other property does not satisfy the rule, the platform
   * reports the missing one alongside the unknown name. The diagnostic has no parameter to anchor
   * on, so it is reported against the annotation. */
  private def validateRequirement(
    definition: AnnotationDefinition,
    parameters: ArraySeq[AnnotationParameter],
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    definition.requires.foreach(requirement =>
      if (!parameters.exists(_.name.exists(name => requirement.keys.contains(name.toLowerCase))))
        logger.logAnnotationError(context, requirement.message)
    )
  }

  /* The platform reads the parameter list as a map, so a name written more than once is validated
   * only where it was written last, and that is the value its combination rules see. A bare value
   * has no name to collapse on, so each one written is validated as it stands. */
  private def lastWritten(
    parameters: ArraySeq[AnnotationParameter]
  ): ArraySeq[AnnotationParameter] = {
    val lastIndex = mutable.HashMap[String, Int]()
    parameters.zipWithIndex.foreach { case (parameter, index) =>
      parameter.name.foreach(name => lastIndex.update(name.toLowerCase, index))
    }
    parameters.zipWithIndex.collect {
      case (parameter, index)
          if parameter.name.forall(name => lastIndex.get(name.toLowerCase).contains(index)) =>
        parameter
    }
  }

  /* Apex separates parameters by whitespace alone. The grammar and the outline parser both record a
   * comma rather than rejecting it, so that one targeted error can be reported here in place of the
   * platform's cascade. One error is enough, the whole list is written the same way. */
  private def validateSeparators(
    parameters: ArraySeq[AnnotationParameter],
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    parameters
      .find(_.precedingSeparator.contains(AnnotationParameterSeparator.Comma))
      .foreach(parameter =>
        logger.logAnnotationError(at(context, parameter.location), "Expecting ')' but was: ','")
      )
  }

  /* A trailing comma leaves the outline parser with a parameter that has nothing written in it. It
   * is reported by its separator alone, there is no name or value to say anything about. */
  private def isEmptyParameter(parameter: AnnotationParameter): Boolean =
    parameter.name.forall(_.isEmpty) && parameter.value.isEmpty

  private def validateParameter(
    definition: AnnotationDefinition,
    parameter: AnnotationParameter,
    target: AnnotationTarget,
    context: LogEntryContext,
    logger: ModifierLogger
  ): Option[(AnnotationProperty, String)] = {
    parameter.name match {
      case None =>
        definition.bareValue match {
          case None =>
            logger.logAnnotationError(
              at(context, parameter.location),
              s"Annotation parameter on ${definition.name} must be written as name=value"
            )
            None
          case Some(property) =>
            validateValue(definition, property, parameter, target, context, logger)
        }
      case Some(name) =>
        definition.property(name) match {
          case None =>
            logger.logAnnotationError(
              at(context, parameter.nameLocation.orElse(parameter.location)),
              s"No such property, $name, defined on this annotation: ${definition.name}"
            )
            None
          case Some(property) =>
            validateValue(definition, property, parameter, target, context, logger)
        }
    }
  }

  private def validateValue(
    definition: AnnotationDefinition,
    property: AnnotationProperty,
    parameter: AnnotationParameter,
    target: AnnotationTarget,
    context: LogEntryContext,
    logger: ModifierLogger
  ): Option[(AnnotationProperty, String)] = {
    val name         = parameter.name.getOrElse(property.name)
    val value        = parameter.value
    val valueContext = at(context, parameter.valueLocation.orElse(parameter.location))

    if (!property.propertyType.accepts(value)) {
      logger.logAnnotationError(
        valueContext,
        s"Invalid value for property $name expected type ${property.propertyType.typeName}"
      )
      return None
    }

    val content = AnnotationValue.content(value)
    if (property.allowedValues.exists(!_.contains(content.toLowerCase))) {
      logger.logAnnotationError(
        valueContext,
        s"Annotation property, $name on ${definition.name}, unknown value: $content"
      )
      return None
    }

    val formatError = property.valueCheck.flatMap(check => check(content))
    formatError.foreach(message => logger.logAnnotationError(valueContext, message))

    if (property.notAllowedOn.contains(target)) {
      logger.logAnnotationError(
        at(context, parameter.nameLocation.orElse(parameter.location)),
        s"Annotation property, $name on ${definition.name}, is not allowed on ${target.pluralName}"
      )
    }

    Option.when(formatError.isEmpty)((property, value))
  }

  /* Only parameters that are themselves valid take part, a value already reported on says nothing
   * about the combination it was written in. */
  private def validateCombinations(
    definition: AnnotationDefinition,
    parameters: ArraySeq[AnnotationParameter],
    values: Map[String, String],
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    definition.combinations
      .filter(combination => combination.isInvalid(values))
      .foreach(combination =>
        logger.logAnnotationError(
          anchorFor(parameters, combination.anchor, context),
          combination.message
        )
      )
  }

  private def anchorFor(
    parameters: ArraySeq[AnnotationParameter],
    anchor: String,
    context: LogEntryContext
  ): LogEntryContext = {
    parameters
      .findLast(_.name.exists(_.equalsIgnoreCase(anchor)))
      .map(parameter => at(context, parameter.location))
      .getOrElse(context)
  }

  private def at(context: LogEntryContext, location: Option[Location]): LogEntryContext =
    location.map(l => new LogEntryContext(l, context.path)).getOrElse(context)
}
