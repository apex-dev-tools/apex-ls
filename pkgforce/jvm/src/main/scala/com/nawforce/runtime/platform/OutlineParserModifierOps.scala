package com.nawforce.runtime.platform

import com.financialforce.oparser.{
  Annotation => OPAnnotation,
  Id => OPId,
  Location => OPLocation,
  Modifier => OPModifier
}
import com.nawforce.pkgforce.diagnostics.{LogEntryContext, ModifierLogger}
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.OutlineParserLocationOps.extendLocation

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object OutlineParserModifierOps {

  private def toModifiers(
    path: PathLike,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier]
  ): ArraySeq[(Modifier, LogEntryContext, String)] = {

    val modifiers = {
      annotations.flatMap(
        opA =>
          ModifierOps(
            "@" + opA.qName.toString.replace(" ", "").toLowerCase,
            opA.parameters.getOrElse("")
          )
            .map(
              m =>
                (
                  m,
                  OPLogEntryContext(path, extendLocation(opA.qName.location, startLineOffset = -2)),
                  "Annotation"
                )
            )
      ) ++
        src.flatMap(
          opM =>
            ModifierOps(opM.text.replace(" ", "").toLowerCase, "")
              .map(
                m =>
                  (
                    m,
                    OPLogEntryContext(path, extendLocation(opM.location, startLineOffset = -1)),
                    "Annotation"
                  )
              )
        )
    }

    ArraySeq.from(modifiers)
  }

  def fieldModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier],
    outer: Boolean
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)
    FieldModifiers.fieldModifiers(logger, mods, outer, OPLogEntryContext(path, id.id.location))
  }

  def classModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)
    ApexModifiers.classModifiers(logger, mods, outer, OPLogEntryContext(path, id.id.location))
  }

  def interfaceModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)
    ApexModifiers.interfaceModifiers(logger, mods, outer, OPLogEntryContext(path, id.id.location))
  }

  def enumModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)
    ApexModifiers.enumModifiers(logger, mods, outer, OPLogEntryContext(path, id.id.location))
  }

  def constructorModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier]
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)
    ApexModifiers.constructorModifiers(logger, mods, OPLogEntryContext(path, id.id.location))
  }

  def parameterModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier]
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods = toModifiers(
      path,
      ArraySeq.unsafeWrapArray(annotations.toArray),
      ArraySeq.unsafeWrapArray(src.toArray)
    )
    ApexModifiers.parameterModifiers(logger, mods, OPLogEntryContext(path, id.id.location))
  }

  def classMethodModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier],
    ownerNature: MethodOwnerNature,
    isOuter: Boolean
  ): ModifierResults = {

    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)

    MethodModifiers.classMethodModifiers(
      logger,
      mods,
      OPLogEntryContext(path, id.id.location),
      ownerNature,
      isOuter
    )
  }

  def interfaceMethodModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArraySeq[OPAnnotation],
    src: ArraySeq[OPModifier]
  ): ModifierResults = {
    val logger = new ModifierLogger()
    val mods   = toModifiers(path, annotations, src)
    MethodModifiers.interfaceMethodModifiers(
      logger,
      mods,
      OPLogEntryContext(path, id.id.location),
      isOuter = false
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
