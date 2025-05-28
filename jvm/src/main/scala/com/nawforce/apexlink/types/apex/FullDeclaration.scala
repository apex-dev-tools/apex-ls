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

package com.nawforce.apexlink.types.apex

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.finding.TypeResolver.TypeCache
import com.nawforce.apexlink.finding.{RelativeTypeContext, TypeResolver}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.org.{OPM, OrgInfo, Referenceable}
import com.nawforce.apexlink.types.core._
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.{Name, Names, TypeIdentifier, TypeName}
import com.nawforce.pkgforce.parsers.{ApexNode, CLASS_NATURE, INTERFACE_NATURE, Nature}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.parsers.{CodeParser, Source, SourceData}
import io.github.apexdevtools.apexparser.ApexParser.TypeDeclarationContext
import upickle.default.writeBinary

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Represents a complete Apex type declaration including its source code and all inner components.
  *
  * This abstract class serves as the base for all Apex type declarations (classes, interfaces, and enums).
  * It handles parsing, validation, dependency tracking, and caching of Apex types.
  *
  * @param source The source code containing this declaration
  * @param module The module this declaration belongs to
  * @param typeContext The context for resolving type references
  * @param typeName The fully qualified name of this type
  * @param outerTypeName The name of the enclosing type if this is a nested type
  * @param id The identifier information for this declaration
  * @param _modifiers The modifiers applied to this declaration
  * @param _inTest Whether this declaration is in a test context
  * @param superClass The parent class if any
  * @param interfaces The interfaces implemented by this type
  * @param bodyDeclarations The declarations contained within this type (methods, fields, etc.)
  */
abstract class FullDeclaration(
  val source: Source,
  val module: OPM.Module,
  val typeContext: RelativeTypeContext,
  override val typeName: TypeName,
  override val outerTypeName: Option[TypeName],
  val id: Id,
  _modifiers: ModifierResults,
  _inTest: Boolean,
  val superClass: Option[TypeName],
  val interfaces: ArraySeq[TypeName],
  val bodyDeclarations: ArraySeq[ClassBodyDeclaration]
) extends ClassBodyDeclaration(_modifiers)
    with ApexClassDeclaration
    with ApexFullDeclaration {

  val sourceHash: Int = source.hash
  private val contentHash: Int =
    ParsedCache.classMetaHash(
      source.path.parent.join(s"${typeName.name.toString}.cls-meta.xml"),
      sourceHash
    )

  override def paths: ArraySeq[PathLike] = ArraySeq(source.path)

  override val moduleDeclaration: Option[OPM.Module] = Some(module)
  override val name: Name                            = typeName.name
  override val idLocation: Location                  = id.location.location
  override val nature: Nature
  override val inTest: Boolean = _inTest

  // Track if this has been flushed to cache yet
  private var flushedToCache = false

  // For ApexNode compatibility
  override val children: ArraySeq[ApexNode] = bodyDeclarations

  override def nestedTypes: ArraySeq[FullDeclaration] =
    bodyDeclarations.flatMap {
      case x: FullDeclaration => Some(x)
      case _                  => None
    }

  override lazy val blocks: ArraySeq[ApexInitializerBlock] = {
    bodyDeclarations.flatMap {
      case x: ApexInitializerBlock => Some(x)
      case _                       => None
    }
  }

  lazy val localFields: ArraySeq[ApexFieldLike] = {
    bodyDeclarations.flatMap {
      case x: ApexFieldDeclaration    => Some(x)
      case x: ApexPropertyDeclaration => Some(x)
      case _                          => None
    }
  }

  override lazy val localConstructors: ArraySeq[ApexConstructorDeclaration] = {
    bodyDeclarations.flatMap {
      case x: ApexConstructorDeclaration => Some(x)
      case _                             => None
    }
  }

  lazy val localMethods: ArraySeq[ApexMethodDeclaration] = {
    bodyDeclarations.flatMap({
      case m: ApexMethodDeclaration => Some(m)
      case _                        => None
    })
  }

  override def flush(pc: ParsedCache, context: PackageContext): Unit = {
    if (!flushedToCache) {
      val diagnostics = module.pkg.org.issueManager.getDiagnostics(location.path).toArray
      pc.upsert(context, name.value, contentHash, writeBinary(ApexSummary(summary, diagnostics)))
      flushedToCache = true
    }
  }

  override protected def validate(): Unit = {
    LoggerOps.debugTime(s"Validated ${location.path}") {
      // Validate inside a parsing context as LazyBlock may call parser
      CST.sourceContext.withValue(Some(source)) {
        val context = new TypeVerifyContext(None, this, None, enablePlugins = true)
        modifierIssues.foreach(context.log)
        verify(context)
        propagateOuterDependencies(new TypeCache())

        // Re-validation may update diagnostics which now need flushing
        flushedToCache = false
      }
    }
  }

  /* Reset local caches ready for re-validation */
  override def preReValidate(): Unit = {
    super.preReValidate()
    typeContext.reset()
    resetMethodMapIfInvalid()
    resetConstructorMapIfInvalid()
    bodyDeclarations.collect({ case p: PreReValidatable => p.preReValidate() })
    nestedTypes.foreach(_.preReValidate())
  }

  protected def verify(context: TypeVerifyContext): Unit = {
    // Check for name/path mismatch on outer types
    val pathBasename = location.path.basename
    if (outerTypeName.isEmpty && !pathBasename.matches(s"(?i)${id.name}.x?cls")) {
      context.logError(
        id.location,
        s"Type name '${id.name}' does not match file name '$pathBasename'"
      )
    }

    // check custom exception is named and extended correctly
    val isNamedCorrectly = id.name.endsWith(Names.Exception)
    val isExtendedCorrectly =
      superClassDeclaration.map(_.typeName).contains(TypeNames.Exception) || superClassDeclaration
        .exists(_.isCustomException)

    (isCustomException, isNamedCorrectly, isExtendedCorrectly) match {
      case (true, true, false) =>
        context.logError(
          id.location,
          s"Exception class '${id.name}' must extend another Exception class"
        )
      case (true, false, true) =>
        context.logError(
          id.location,
          s"Class '${id.name}' extending an Exception must have a name ending in Exception"
        )
      case _ =>
    }

    // Check super class is good
    superClass.foreach(superClass => {
      if (superClassDeclaration.isEmpty)
        context.missingType(id.location, superClass)
    })
    superClassDeclaration.foreach(superClassDeclaration => {
      Referenceable.addReferencingLocation(superClassDeclaration, id.location, this)
      context.addDependency(superClassDeclaration)
      if (superClassDeclaration.nature != CLASS_NATURE) {
        OrgInfo.logError(id.location, s"Parent type '${superClass.get.asDotName}' must be a class")
      } else if (
        !inTest &&
        superClassDeclaration.visibility.getOrElse(PRIVATE_MODIFIER) == PRIVATE_MODIFIER &&
        superClassDeclaration.outermostTypeDeclaration != outermostTypeDeclaration
      ) {
        // Private is OK with Outer extends Inner, Inner extends Inner or Test classes
        OrgInfo.logError(
          id.location,
          s"Parent class '${superClass.get.asDotName}' is private, it must be public or global"
        )
      } else if (
        superClassDeclaration.modifiers
          .intersect(Seq(VIRTUAL_MODIFIER, ABSTRACT_MODIFIER))
          .isEmpty
      ) {
        OrgInfo.logError(
          id.location,
          s"Parent class '${superClass.get.asDotName}' must be declared virtual or abstract"
        )
      }
    })

    // Check for duplicate nested types
    val duplicateNestedType =
      (this +: nestedTypes).groupBy(_.name).collect { case (_, Seq(_, y, _*)) => y }
    duplicateNestedType.foreach(td =>
      OrgInfo.logError(td.location, s"Duplicate type name '${td.name.toString}'")
    )

    // Check interfaces are visible
    interfaces.foreach(interface => {
      val td = context.getTypeAndAddDependency(interface, context.thisType).toOption
      if (td.isEmpty) {
        if (!context.module.isGulped && !context.module.isGhostedType(interface))
          context.missingType(id.location, interface)
      } else if (td.get.nature != INTERFACE_NATURE) {
        OrgInfo.logError(id.location, s"Type '${interface.toString}' must be an interface")
      } else {
        Referenceable.addReferencingLocation(td.get, id.location, this)
      }
    })

    // Detail check each body declaration
    bodyDeclarations.foreach(bd => bd.validate(new BodyDeclarationVerifyContext(context, bd, None)))

    nestedTypes
      .filter(t => t.nestedTypes.nonEmpty)
      .foreach(_.nestedTypes.foreach {
        case fd: FullDeclaration =>
          OrgInfo
            .logError(fd.id.location, s"${fd.id.name}: Inner types of Inner types are not valid.")
        case _ =>
      })

    // Log dependencies logged against this context
    setDepends(context.dependencies)

    // Force creating of method/constructor maps in case it is not already done.
    // Either way we need to pass on any diagnostics found
    methodMap.errors.foreach(OrgInfo.log)
    constructorMap.errors.foreach(OrgInfo.log)
  }

  override def gatherDependencies(
    dependsOn: mutable.Set[TypeId],
    apexOnly: Boolean,
    outerTypesOnly: Boolean,
    typeCache: TypeCache
  ): Unit = {
    val dependents = mutable.Set[Dependent]()
    collectDependencies(dependents)
    DependentType.dependentsToTypeIds(module, dependents, apexOnly, outerTypesOnly, dependsOn)
  }

  override def collectDependencies(dependsOn: mutable.Set[Dependent]): Unit = {
    super.collectDependencies(dependsOn)
    bodyDeclarations.foreach(_.collectDependencies(dependsOn))
  }

  /** Locate an ApexDeclaration for the passed typeName that was extracted from location. */
  override def findDeclarationFromSourceReference(
    searchTerm: String,
    location: Location
  ): Option[ApexDeclaration] = {

    /** Find the outer or inner class that contains the passed cursor position */
    def findEnclosingClass(line: Int, offset: Int): Option[FullDeclaration] = {
      nestedTypes
        .collect { case nested: FullDeclaration => nested }
        .find(_.location.location.contains(line, offset))
        .orElse({
          if (this.location.location.contains(line, offset))
            Some(this)
          else
            None
        })
    }

    TypeName(searchTerm).toOption match {
      case Some(typeName: TypeName) =>
        findEnclosingClass(location.startLine, location.startPosition).flatMap(td => {
          TypeResolver(typeName, td).toOption.collect { case td: ApexDeclaration => td }
        })
      case _ => None
    }
  }

  /** Returns a map of validation results for the body declaration at the specified source location.
    * <p>
    * This method locates the innermost body declaration (such as a method, field, or nested type)
    * that contains the given line and offset. It then validates that declaration, collecting
    * validation results for each relevant source location within it.
    * <p>
    * If no matching body declaration is found, or if an error occurs during validation,
    * an empty map is returned.
    *
    * @param line   The 1-based line number within the source file.
    * @param offset The 0-based character offset within the line.
    * @return       A map from source locations to their corresponding validation results.
    */
  override def getValidationMap(line: Int, offset: Int): Map[Location, ValidationResult] = {
    try {
      getBodyDeclarationFromLocation(line, offset)
        .map(typeAndBody => {
          // Validate the body declaration for the side effect of being able to collect a map of expression results
          val typeContext = new TypeVerifyContext(None, typeAndBody._1, None, enablePlugins = false)
          val resultMap   = mutable.Map[Location, ValidationResult]()
          val context =
            new BodyDeclarationVerifyContext(typeContext, typeAndBody._2, Some(resultMap))
          context.disableIssueReporting() {
            typeAndBody._2.validate(context)
          }
          resultMap.toMap
        })
        .getOrElse(Map.empty)
    } catch {
      case ex: Throwable =>
        val at = ex.getStackTrace.headOption.getOrElse("Unknown")
        LoggerOps.debug(s"Body validation failure: ${ex.toString} $at")
        Map.empty
    }
  }

  /** Finds the body declaration (such as a method, field, or nested type) at the specified line and offset.
    * Recursively searches nested types and returns the innermost matching (FullDeclaration, ClassBodyDeclaration) pair.
    *
    * @param line   The line number to search (1-based).
    * @param offset The character offset within the line (0-based).
    * @return       An Option containing the (FullDeclaration, ClassBodyDeclaration) if found, otherwise None.
    */
  def getBodyDeclarationFromLocation(
    line: Int,
    offset: Int
  ): Option[(FullDeclaration, ClassBodyDeclaration)] = {
    nestedTypes.view
      .collect { case nested: FullDeclaration => nested }
      .flatMap(td => td.getBodyDeclarationFromLocation(line, offset))
      .headOption
      .orElse({
        bodyDeclarations.find(_.location.location.contains(line, offset)).map((this, _))
      })
  }

  // Override to avoid super class access (use local fields & methods) & provide location information
  override def summary: TypeSummary = {
    TypeSummary(
      sourceHash,
      location.location,
      id.location.location,
      name.toString,
      typeName,
      nature.value,
      modifiers,
      inTest,
      superClass,
      interfaces,
      blocks.map(_.summary),
      localFields.map(_.summary).sortBy(_.name),
      localConstructors.map(_.summary).sortBy(_.parameters.length),
      localMethods.map(_.summary).sortBy(_.name),
      nestedTypes.map(_.summary).sortBy(_.name),
      dependencySummary()
    )
  }
}

final case class ThisType(module: OPM.Module, typeName: TypeName, inTest: Boolean) {
  def typeId: TypeId = TypeId(module, typeName)

  def typeIdentifier: TypeIdentifier = typeId.asTypeIdentifier

  def asInner(name: String): ThisType = {
    ThisType(module, TypeName(Names(name), Nil, Some(typeName)), inTest)
  }
}

object FullDeclaration {

  def create(
    module: OPM.Module,
    doc: ClassDocument,
    data: SourceData,
    forceConstruct: Boolean
  ): Option[FullDeclaration] = {
    val parser = CodeParser(doc.path, data)
    val result = parser.parseClass()
    val issues = result.issues
    issues.foreach(OrgInfo.log)
    if (issues.isEmpty || forceConstruct) {
      try {
        CompilationUnit.construct(parser, module, doc.name, result.value).map(_.typeDeclaration)
      } catch {
        case ex: Throwable =>
          module.log(doc.path, "CST construction failed", ex)
          None
      }
    } else {
      None
    }
  }

  def construct(
    parser: CodeParser,
    module: OPM.Module,
    name: Name,
    typeDeclaration: TypeDeclarationContext
  ): Option[FullDeclaration] = {

    val modifiers = ArraySeq.unsafeWrapArray(CodeParser.toScala(typeDeclaration.modifier()).toArray)
    val thisType  = TypeName(name).withNamespace(module.namespace)

    val cst: Option[FullDeclaration] = CodeParser
      .toScala(typeDeclaration.classDeclaration())
      .map(cd => {
        val classModifiers = ApexModifiers.classModifiers(parser, modifiers, outer = true, cd.id())
        ClassDeclaration.construct(
          parser,
          ThisType(module, thisType, classModifiers.modifiers.contains(ISTEST_ANNOTATION)),
          None,
          classModifiers,
          cd
        )
      })
      .orElse(
        CodeParser
          .toScala(typeDeclaration.interfaceDeclaration())
          .map(id =>
            InterfaceDeclaration.construct(
              parser,
              ThisType(module, thisType, inTest = false),
              None,
              ApexModifiers.interfaceModifiers(parser, modifiers, outer = true, id.id()),
              id
            )
          )
      )
      .orElse(
        CodeParser
          .toScala(typeDeclaration.enumDeclaration())
          .map(ed =>
            EnumDeclaration.construct(
              parser,
              ThisType(module, thisType, inTest = false),
              None,
              ApexModifiers.enumModifiers(parser, modifiers, outer = true, ed.id()),
              ed
            )
          )
      )

    cst.map(_.withContext(typeDeclaration))
  }
}
