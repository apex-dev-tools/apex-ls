/*
 * Copyright (c) 2023 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.platform

import io.github.apexdevtools.types.base.{
  Annotation => OPAnnotation,
  AnnotationParameterSeparator => OPAnnotationParameterSeparator,
  IdWithLocation => OPId,
  Location => OPLocation,
  Modifier => OPModifier
}
import com.nawforce.pkgforce.modifiers.{LogEntryContext, ModifierLogger, _}
import com.nawforce.pkgforce.path.PathLike

import scala.collection.compat.immutable.ArraySeq
object OutlineParserModifierOps {

  private def toModifiers(
    path: PathLike,
    declarationLocation: OPLocation,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    logger: ModifierLogger
  ): ArraySeq[(Modifier, LogEntryContext, String)] = {

    def normaliseModifierLocation(modifier: OPModifier): OPLocation = {
      modifier.location.getOrElse(declarationLocation)
    }

    def normaliseAnnotationLocation(annotation: OPAnnotation): OPLocation = {
      annotation.location.getOrElse(declarationLocation)
    }

    val annotationContexts = ArraySeq.from(
      annotations.map(annotation =>
        (annotation.name, OPLogEntryContext(path, normaliseAnnotationLocation(annotation)))
      )
    )
    ApexModifiers.validateDuplicateAnnotations(annotationContexts, logger)

    val modifiers = {
      annotations.flatMap(opA =>
        ModifierOps("@" + opA.name.replace(" ", "").toLowerCase, toAnnotationParameters(opA))
          .map(m => (m, OPLogEntryContext(path, normaliseAnnotationLocation(opA)), "Annotation"))
      ) ++
        src.flatMap(opM =>
          ModifierOps(opM.text.replace(" ", "").toLowerCase, None)
            .map(m => (m, OPLogEntryContext(path, normaliseModifierLocation(opM)), "Modifier"))
        )
    }

    ApexModifiers.deduplicateAnnotationModifiers(ArraySeq.from(modifiers))
  }

  private def toAnnotationParameters(
    annotation: OPAnnotation
  ): Option[ArraySeq[AnnotationParameter]] = {
    annotation.parameterList.map(parameters =>
      ArraySeq.from(
        parameters.map(parameter =>
          AnnotationParameter(
            parameter.name,
            parameter.value,
            parameter.precedingSeparator.map(toSeparator),
            parameter.nameLocation.map(OutlineParserLocationOps.toLocation),
            parameter.valueLocation.map(OutlineParserLocationOps.toLocation),
            parameter.location.map(OutlineParserLocationOps.toLocation)
          )
        )
      )
    )
  }

  private def toSeparator(
    separator: OPAnnotationParameterSeparator
  ): AnnotationParameterSeparator = {
    separator match {
      case OPAnnotationParameterSeparator.Comma => AnnotationParameterSeparator.Comma
      case _                                    => AnnotationParameterSeparator.Whitespace
    }
  }

  def fieldModifiers(
    path: PathLike,
    id: OPId,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    outer: Boolean
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(path, id.location, annotations, src, logger)
    FieldModifiers.fieldModifiers(logger, mods, outer, OPLogEntryContext(path, id.location))
  }

  def classModifiers(
    path: PathLike,
    id: OPId,
    declarationLocation: OPLocation,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, declarationLocation, annotations, src, logger)
    ApexModifiers.classModifiers(logger, mods, outer, OPLogEntryContext(path, id.location))
  }

  def interfaceModifiers(
    path: PathLike,
    id: OPId,
    declarationLocation: OPLocation,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, declarationLocation, annotations, src, logger)
    ApexModifiers.interfaceModifiers(logger, mods, outer, OPLogEntryContext(path, id.location))
  }

  def enumModifiers(
    path: PathLike,
    id: OPId,
    declarationLocation: OPLocation,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, declarationLocation, annotations, src, logger)
    ApexModifiers.enumModifiers(logger, mods, outer, OPLogEntryContext(path, id.location))
  }

  def constructorModifiers(
    path: PathLike,
    id: OPId,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier]
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(path, id.location, annotations, src, logger)
    ApexModifiers.constructorModifiers(logger, mods, OPLogEntryContext(path, id.location))
  }

  def parameterModifiers(
    path: PathLike,
    idLocation: OPLocation,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier]
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, idLocation, annotations, src, logger)
    ApexModifiers.parameterModifiers(logger, mods, OPLogEntryContext(path, idLocation))
  }

  def classMethodModifiers(
    path: PathLike,
    id: OPId,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    ownerInfo: ClassOwnerInfo,
    isOuter: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, id.location, annotations, src, logger)

    MethodModifiers.classMethodModifiers(
      logger,
      mods,
      OPLogEntryContext(path, id.location),
      ownerInfo,
      isOuter
    )
  }

  def interfaceMethodModifiers(
    path: PathLike,
    id: OPId,
    annotations: Array[OPAnnotation],
    src: Array[OPModifier],
    ownerInfo: InterfaceOwnerInfo
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(path, id.location, annotations, src, logger)
    MethodModifiers.interfaceMethodModifiers(
      logger,
      mods,
      OPLogEntryContext(path, id.location),
      ownerInfo
    )
  }

  def initializerBlockModifiers(isStatic: Boolean): ModifierResults =
    ApexModifiers.initializerBlockModifiers(isStatic)

  def enumConstantModifiers(): ModifierResults = ApexModifiers.enumConstantModifiers()

}

object OPLogEntryContext {
  def apply(path: PathLike, location: OPLocation): LogEntryContext = {
    new LogEntryContext(OutlineParserLocationOps.toLocation(location), path)
  }
}
