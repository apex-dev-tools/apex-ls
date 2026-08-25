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
    for {
      definition <- AnnotationDefinition(name)
      written    <- parameters
    } validateParameters(definition, written, target, context, logger)
  }

  private def validateParameters(
    definition: AnnotationDefinition,
    parameters: ArraySeq[AnnotationParameter],
    target: AnnotationTarget,
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    if (parameters.isEmpty) {
      definition.emptyParameterMessage.foreach(message => logger.logError(context, message))
      return
    }

    validateSeparators(parameters, context, logger)

    val values = new mutable.LinkedHashMap[String, Seq[String]]()
    parameters
      .filterNot(isEmptyParameter)
      .foreach(parameter =>
        validateParameter(definition, parameter, target, context, logger)
          .foreach { case (property, value) =>
            values.update(property.key, values.getOrElse(property.key, Seq()) :+ value)
          }
      )

    validateCombinations(definition, parameters, values.toMap, context, logger)
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
        logger.logError(at(context, parameter.location), "Expecting ')' but was: ','")
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
            logger.logError(
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
            logger.logError(
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
      logger.logError(
        valueContext,
        s"Invalid value for property $name expected type ${property.propertyType.typeName}"
      )
      return None
    }

    val content =
      if (AnnotationValue.isStringLiteral(value)) AnnotationValue.stringContent(value) else value
    if (property.values.exists(!_.contains(content.toLowerCase))) {
      logger.logError(
        valueContext,
        s"Annotation property, $name on ${definition.name}, unknown value: $content"
      )
      return None
    }

    val formatError = property.valueCheck.flatMap(check => check(value))
    formatError.foreach(message => logger.logError(valueContext, message))

    if (property.targets.exists(!_.contains(target))) {
      logger.logError(
        at(context, parameter.nameLocation.orElse(parameter.location)),
        s"Annotation property, $name on ${definition.name}, is not allowed on ${target.pluralName}"
      )
    }

    Option.when(formatError.isEmpty)((property, value))
  }

  /* Duplicate parameters are legal, so a rule sees every value written for a property. Only
   * parameters that are themselves valid take part, a value already reported on says nothing about
   * the combination it was written in. */
  private def validateCombinations(
    definition: AnnotationDefinition,
    parameters: ArraySeq[AnnotationParameter],
    values: Map[String, Seq[String]],
    context: LogEntryContext,
    logger: ModifierLogger
  ): Unit = {
    definition.combinations
      .filter(combination => combination.isInvalid(values))
      .foreach(combination =>
        logger.logError(anchorFor(parameters, combination.anchor, context), combination.message)
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
