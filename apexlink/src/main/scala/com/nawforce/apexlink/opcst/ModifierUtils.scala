package com.nawforce.apexlink.opcst

// TODO This need to be worked back into pkgforce
import com.financialforce.oparser.{Annotation => OPAnnotation, Id => OPId, Modifier => OPModifier}
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.modifiers.{
  ABSTRACT_METHOD_NATURE,
  ABSTRACT_MODIFIER,
  AURA_ENABLED_ANNOTATION,
  ApexModifiers,
  DEPRECATED_ANNOTATION,
  FINAL_MODIFIER,
  FUTURE_ANNOTATION,
  GLOBAL_MODIFIER,
  HTTP_DELETE_ANNOTATION,
  HTTP_GET_ANNOTATION,
  HTTP_PATCH_ANNOTATION,
  HTTP_POST_ANNOTATION,
  HTTP_PUT_ANNOTATION,
  INHERITED_SHARING_MODIFIER,
  INVOCABLE_METHOD_ANNOTATION,
  INVOCABLE_VARIABLE_ANNOTATION,
  ISTEST_ANNOTATION,
  JSON_ACCESS_ANNOTATION,
  MethodOwnerNature,
  Modifier,
  ModifierOps,
  ModifierResults,
  NAMESPACE_ACCESSIBLE_ANNOTATION,
  OVERRIDE_MODIFIER,
  PRIVATE_MODIFIER,
  PUBLIC_MODIFIER,
  READ_ONLY_ANNOTATION,
  REMOTE_ACTION_ANNOTATION,
  REST_RESOURCE_ANNOTATION,
  STATIC_MODIFIER,
  SUPPRESS_WARNINGS_ANNOTATION_PMD,
  SUPPRESS_WARNINGS_ANNOTATION_UNUSED,
  TEST_METHOD_MODIFIER,
  TEST_SETUP_ANNOTATION,
  TEST_VISIBLE_ANNOTATION,
  TRANSIENT_MODIFIER,
  VIRTUAL_MODIFIER,
  WEBSERVICE_MODIFIER,
  WITHOUT_SHARING_MODIFIER,
  WITH_SHARING_MODIFIER
}
import com.nawforce.pkgforce.path.PathLike

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

private[opcst] object ModifierUtils {

  private val TypeAnnotations: Set[Modifier] =
    Set(
      DEPRECATED_ANNOTATION,
      TEST_VISIBLE_ANNOTATION,
      SUPPRESS_WARNINGS_ANNOTATION_PMD,
      SUPPRESS_WARNINGS_ANNOTATION_UNUSED,
      NAMESPACE_ACCESSIBLE_ANNOTATION
    )

  private val sharingModifiers =
    Seq(WITH_SHARING_MODIFIER, WITHOUT_SHARING_MODIFIER, INHERITED_SHARING_MODIFIER)

  private val TypeModifiers: Set[Modifier] =
    Set(GLOBAL_MODIFIER, PUBLIC_MODIFIER, PRIVATE_MODIFIER)

  private val TypeModifiersAndAnnotations: Set[Modifier] =
    TypeAnnotations ++ TypeModifiers

  private val ClassAnnotations: Set[Modifier] =
    TypeAnnotations ++ Set(ISTEST_ANNOTATION, REST_RESOURCE_ANNOTATION, JSON_ACCESS_ANNOTATION)

  private val ClassModifiers: Set[Modifier] =
    TypeModifiers ++ Set(ABSTRACT_MODIFIER, VIRTUAL_MODIFIER) ++ sharingModifiers.toSet

  private val ClassModifiersAndAnnotations: Set[Modifier] =
    ClassAnnotations ++ ClassModifiers

  private val FieldModifiers: Set[Modifier] =
    ApexModifiers.visibilityModifiers.toSet[Modifier] ++ Set(
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

  private val legalConstructorModifiersAndAnnotations: Set[Modifier] =
    ApexModifiers.visibilityModifiers.toSet ++ TypeAnnotations

  private val MethodModifiers: Set[Modifier] =
    ApexModifiers.visibilityModifiers.toSet[Modifier] ++ Set(
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

  private val InterfaceModifiers: Set[Modifier] =
    Set(GLOBAL_MODIFIER, PUBLIC_MODIFIER, PRIVATE_MODIFIER, VIRTUAL_MODIFIER)

  private val InterfaceModifiersAndAnnotations: Set[Modifier] =
    TypeAnnotations ++ InterfaceModifiers

  private val legalParameterModifiersAndAnnotations: Set[Modifier] = Set(FINAL_MODIFIER)

  private val staticModifier: ArraySeq[Modifier] = ArraySeq(STATIC_MODIFIER)

  private def deduplicateVisibility(
    modifiers: ArraySeq[Modifier],
    pluralName: String,
    logger: CodeOutlineParserLogger,
    id: OPId
  ): ArraySeq[Modifier] = {
    if (modifiers.intersect(ApexModifiers.visibilityModifiers).size > 1) {
      if (logger.isEmpty)
        logger.logWarning(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"Only one visibility modifier from 'global', 'public' & 'private' should be used on $pluralName"
        )
      PUBLIC_MODIFIER +: modifiers.diff(ApexModifiers.visibilityModifiers)
    } else {
      modifiers
    }
  }

  private def deduplicateSharing(
    modifiers: ArraySeq[Modifier],
    pluralName: String,
    logger: CodeOutlineParserLogger,
    id: OPId
  ): ArraySeq[Modifier] = {
    if (modifiers.intersect(sharingModifiers).size > 1) {
      if (logger.isEmpty)
        logger.logWarning(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"Only one sharing modifier from 'with sharing', 'without sharing' & 'inherited sharing' should be used on $pluralName"
        )
      WITHOUT_SHARING_MODIFIER +: modifiers.diff(sharingModifiers)
    } else {
      modifiers
    }
  }

  private def deduplicate(
    modifiers: ArraySeq[Modifier],
    pluralName: String,
    logger: CodeOutlineParserLogger,
    id: OPId
  ): ArraySeq[Modifier] = {
    deduplicateVisibility(
      deduplicateSharing(modifiers, pluralName, logger, id),
      pluralName,
      logger,
      id
    )
  }

  private def asModifiers(
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    allow: Set[Modifier],
    pluralName: String,
    logger: CodeOutlineParserLogger,
    id: OPId
  ): ArraySeq[Modifier] = {

    val modifiers = toModifiers(annotations, src, allow, pluralName, logger)
    if (modifiers.size == src.size + annotations.size) {
      val duplicates = modifiers.duplicates(identity)
      if (duplicates.nonEmpty) {
        logger.logError(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"Modifier '${duplicates.head._1.toString}' is used more than once"
        )
      }
    }
    modifiers.distinct
  }

  private def toModifiers(
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    allow: Set[Modifier],
    pluralName: String,
    logger: CodeOutlineParserLogger
  ): ArraySeq[Modifier] = {

    def filterAnnotation(annotation: OPAnnotation, modifier: Modifier): Option[Modifier] = {
      if (allow.contains(modifier)) Some(modifier)
      else {
        logger.logError(
          LocationUtils.extendLocation(annotation.qName.location, startLineOffset = -2),
          s"Annotation '${annotation.toString()}' is not supported on $pluralName"
        )
        None
      }
    }

    def filterModifier(opModifier: OPModifier, modifier: Modifier): Option[Modifier] = {
      if (allow.contains(modifier)) Some(modifier)
      else {
        logger.logError(
          LocationUtils.extendLocation(opModifier.location, startLineOffset = -1),
          s"Modifier '${opModifier.toString()}' is not supported on $pluralName"
        )
        None
      }
    }

    val modifiers = {
      annotations.flatMap(
        opA =>
          ModifierOps(
            "@" + opA.qName.toString.replace(" ", "").toLowerCase,
            opA.parameters.getOrElse("")
          )
            .flatMap(a => filterAnnotation(opA, a))
      ) ++
        src.flatMap(
          opM =>
            ModifierOps(opM.text.replace(" ", "").toLowerCase, "")
              .flatMap(m => filterModifier(opM, m))
        )
    }

    ArraySeq.from(modifiers)
  }

  def classModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicate(
      asModifiers(annotations, src, ClassModifiersAndAnnotations, "classes", logger, id),
      "classes",
      logger,
      id
    )

    val results =
      if (logger.isEmpty) {
        if (outer && !mods.contains(ISTEST_ANNOTATION) && mods.contains(PRIVATE_MODIFIER)) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Private modifier is not allowed on outer classes"
          )
          mods.filterNot(_ == PRIVATE_MODIFIER)
        } else if (
          outer && !mods.contains(ISTEST_ANNOTATION) && !(mods.contains(GLOBAL_MODIFIER) || mods
            .contains(PUBLIC_MODIFIER))
        ) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Outer classes must be declared either 'global' or 'public'"
          )
          PUBLIC_MODIFIER +: mods
        } else if (mods.contains(ABSTRACT_MODIFIER) && mods.contains(VIRTUAL_MODIFIER)) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Abstract classes are virtual classes"
          )
          mods.filterNot(_ == VIRTUAL_MODIFIER)
        } else if (!outer && mods.contains(ISTEST_ANNOTATION)) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"isTest can only be used on outer classes"
          )
          mods.filterNot(_ == ISTEST_ANNOTATION)
        } else {
          mods
        }
      } else {
        mods
      }

    ModifierResults(results, logger.issues).intern
  }

  def fieldModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicateVisibility(
      asModifiers(
        annotations,
        src,
        if (outer) FieldModifiersAndAnnotations else InnerFieldModifiersAndAnnotations,
        if (outer) "fields" else "inner class fields",
        logger,
        id
      ),
      "fields",
      logger,
      id
    )

    val results = {
      if (
        mods.intersect(ApexModifiers.visibilityModifiers).isEmpty && mods.contains(
          WEBSERVICE_MODIFIER
        )
      ) {
        GLOBAL_MODIFIER +: mods
      } else if (
        !mods.intersect(ApexModifiers.visibilityModifiers).contains(GLOBAL_MODIFIER) && mods
          .contains(WEBSERVICE_MODIFIER)
      ) {
        logger.logError(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"webservice fields must be global"
        )
        GLOBAL_MODIFIER +: mods.diff(ApexModifiers.visibilityModifiers)
      } else if (mods.intersect(ApexModifiers.visibilityModifiers).isEmpty) {
        PRIVATE_MODIFIER +: mods
      } else {
        mods
      }
    }
    ModifierResults(results, logger.issues).intern
  }

  def constructorModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier]
  ): ModifierResults = {

    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicateVisibility(
      asModifiers(
        annotations,
        src,
        legalConstructorModifiersAndAnnotations,
        "constructors",
        logger,
        id
      ),
      "constructors",
      logger,
      id
    )

    val results = {
      if (mods.intersect(ApexModifiers.visibilityModifiers).isEmpty) {
        PRIVATE_MODIFIER +: mods
      } else {
        mods
      }
    }
    ModifierResults(results, logger.issues).intern
  }

  def classMethodModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    ownerNature: MethodOwnerNature,
    isOuter: Boolean
  ): ModifierResults = {

    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicateVisibility(
      asModifiers(annotations, src, MethodModifiersAndAnnotations, "methods", logger, id),
      "methods",
      logger,
      id
    )

    val results = {
      if (
        mods.intersect(ApexModifiers.visibilityModifiers).isEmpty && mods.contains(
          WEBSERVICE_MODIFIER
        )
      ) {
        GLOBAL_MODIFIER +: mods
      } else if (
        !mods.intersect(ApexModifiers.visibilityModifiers).contains(GLOBAL_MODIFIER) && mods
          .contains(WEBSERVICE_MODIFIER)
      ) {
        logger.logError(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"webservice methods must be global"
        )
        GLOBAL_MODIFIER +: mods.diff(ApexModifiers.visibilityModifiers)
      } else if (isOuter && mods.contains(WEBSERVICE_MODIFIER)) {
        logger.logError(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"webservice methods can only be declared on outer classes"
        )
        GLOBAL_MODIFIER +: mods.diff(ApexModifiers.visibilityModifiers)
      } else if (mods.contains(VIRTUAL_MODIFIER) && mods.contains(ABSTRACT_MODIFIER)) {
        logger.logError(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"abstract methods are virtual methods"
        )
        mods.filterNot(_ == VIRTUAL_MODIFIER)
      } else if (ownerNature != ABSTRACT_METHOD_NATURE && mods.contains(ABSTRACT_MODIFIER)) {
        logger.logError(
          LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
          s"abstract methods can only be declared on abstract classes"
        )
        mods
      } else {
        mods
      }
    }
    ModifierResults(results, logger.issues).intern
  }

  def interfaceModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicate(
      asModifiers(annotations, src, InterfaceModifiersAndAnnotations, "interfaces", logger, id),
      "interfaces",
      logger,
      id
    )

    val results = {
      if (logger.isEmpty) {
        if (outer && mods.contains(PRIVATE_MODIFIER)) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Private modifier is not allowed on outer interfaces"
          )
          mods.filterNot(_ == PRIVATE_MODIFIER)
        } else if (outer && !(mods.contains(GLOBAL_MODIFIER) || mods.contains(PUBLIC_MODIFIER))) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Outer interfaces must be declared either 'global' or 'public'"
          )
          PUBLIC_MODIFIER +: mods
        } else {
          mods
        }
      } else {
        mods
      }
    }
    ModifierResults(results, logger.issues).intern
  }

  def interfaceMethodModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier]
  ): ModifierResults = {
    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicate(
      asModifiers(annotations, src, Set.empty, "interface methods", logger, id),
      "methods",
      logger,
      id
    )

    ModifierResults((mods ++ ArraySeq(VIRTUAL_MODIFIER, PUBLIC_MODIFIER)), logger.issues).intern
  }

  def enumModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier],
    outer: Boolean
  ): ModifierResults = {

    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicate(
      asModifiers(annotations, src, TypeModifiersAndAnnotations, "enums", logger, id),
      "enums",
      logger,
      id
    )

    val results = {
      if (logger.isEmpty) {
        if (outer && mods.contains(PRIVATE_MODIFIER)) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Private modifier is not allowed on outer enums"
          )
          mods.filterNot(_ == PRIVATE_MODIFIER)
        } else if (outer && !(mods.contains(GLOBAL_MODIFIER) || mods.contains(PUBLIC_MODIFIER))) {
          logger.logError(
            LocationUtils.extendLocation(id.id.location, startLineOffset = -1),
            s"Outer enums must be declared either 'global' or 'public'"
          )
          PUBLIC_MODIFIER +: mods
        } else {
          mods
        }
      } else {
        mods
      }
    }
    ModifierResults(results, logger.issues).intern
  }

  def parameterModifiers(
    path: PathLike,
    id: OPId,
    annotations: ArrayBuffer[OPAnnotation],
    src: ArrayBuffer[OPModifier]
  ): ModifierResults = {
    val logger = new CodeOutlineParserLogger(path)
    val mods = deduplicateVisibility(
      asModifiers(
        annotations,
        src,
        legalParameterModifiersAndAnnotations,
        "parameters",
        logger,
        id
      ),
      "parameters",
      logger,
      id
    )

    ModifierResults(mods, ArraySeq.empty).intern
  }

  def enumConstantModifiers(): ModifierResults =
    ModifierResults(ArraySeq(PUBLIC_MODIFIER, STATIC_MODIFIER), ArraySeq())

  def initializerBlockModifiers(isStatic: Boolean): ModifierResults = {
    val mods = if (isStatic) staticModifier else ArraySeq.empty

    ModifierResults(mods, ArraySeq.empty).intern
  }
}
