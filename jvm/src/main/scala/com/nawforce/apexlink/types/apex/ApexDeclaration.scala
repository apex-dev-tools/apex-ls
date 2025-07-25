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
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.finding.TypeResolver.TypeCache
import com.nawforce.apexlink.memory.SkinnyWeakSet
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.org.{OPM, OrgInfo, Referenceable}
import com.nawforce.apexlink.types.core._
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, Nature}
import com.nawforce.pkgforce.path.{IdLocatable, Locatable, Location, PathLocation}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait PreReValidatable {

  /** Called before validate() when a type is about to be re-validated to allow for cached state
    * cleaning.
    */
  def preReValidate(): Unit = {}
}

/** Apex block core features, be they full or summary style */
trait ApexBlockLike extends BlockDeclaration with Locatable {
  val thisTypeId: TypeId
  override def thisTypeIdOpt: Option[TypeId] = Some(thisTypeId)
  def location: PathLocation
  def summary: BlockSummary = BlockSummary(location.location, isStatic, dependencySummary())
}

/** Unifying trait for ApexConstructorLike and CustomConstructorDeclaration. Both need to appear to
  * be visible from a type but have little in common beyond allowing for creation of a summary.
  */
trait ApexVisibleConstructorLike extends ConstructorDeclaration {
  def summary: ConstructorSummary
}

/** Apex defined constructor core features, be they full or summary style */
trait ApexConstructorLike extends ApexVisibleConstructorLike with Referenceable {
  val thisTypeId: TypeId
  override def thisTypeIdOpt: Option[TypeId] = Some(thisTypeId)

  def summary: ConstructorSummary = {
    ConstructorSummary(
      location.location,
      idLocation,
      modifiers,
      parameters.map(_.serialise),
      dependencySummary()
    )
  }
}

/** Unifying trait for ApexMethodLike and CustomMethodDeclaration. Both need to appear to be visible
  * from a type but have little in common beyond allowing for constructions of a summary.
  */
trait ApexVisibleMethodLike extends MethodDeclaration {
  def summary: MethodSummary
}

/** Apex defined method core features, be they full or summary style */
trait ApexMethodLike extends ApexVisibleMethodLike with Referenceable {
  override val thisTypeId: TypeId
  override def thisTypeIdOpt: Option[TypeId] = Some(thisTypeId)

  // Synthetic methods are generated locally & so can be excluded from issue reporting
  def isSynthetic: Boolean = false

  // Populated by type MethodMap construction
  private var _shadows: SkinnyWeakSet[MethodDeclaration]    = new SkinnyWeakSet()
  private var _shadowedBy: SkinnyWeakSet[MethodDeclaration] = new SkinnyWeakSet()

  def shadows: Set[MethodDeclaration]    = _shadows.toSet
  def shadowedBy: Set[MethodDeclaration] = _shadowedBy.toSet

  def resetShadows(): Unit = {
    _shadows = new SkinnyWeakSet()
    _shadowedBy = new SkinnyWeakSet()
  }

  def addShadow(method: MethodDeclaration): Unit = {
    if (method ne this) {
      _shadows.add(method)

      method match {
        case am: ApexMethodLike => am._shadowedBy.add(this)
        case _                  =>
      }
    }
  }

  /** Collects all related `Referenceable` instances, including the current method and
    * all parent and child methods in the shadow hierarchy.
    *
    * @return A set of `Referenceable` instances related to the current method.
    */
  override def collectRelatedReferencable(): Set[ApexMethodLike] = {
    def getShadowMethods(method: ApexMethodLike): Set[ApexMethodLike] = {
      (method.shadows ++ method.shadowedBy).collect { case am: ApexMethodLike => am }
    }

    // To use shadows & shadowedBy we need method maps setup for dependency holders, typically
    // this will have already been done but when cache loading method maps are not generally
    // needed so better to be sure
    this.thisTypeId
      .toTypeDeclaration[ApexClassDeclaration]
      .foreach(td => {
        val holders = mutable.Set[ApexClassDeclaration]()
        val targets = mutable.Queue[ApexClassDeclaration](td)
        while (targets.nonEmpty) {
          val target = targets.dequeue()
          holders.add(target)
          target.getTypeDependencyHolders.toIterable
            .flatMap(_.toTypeDeclaration[ApexClassDeclaration])
            .filterNot(holders.contains)
            .foreach(targets.enqueue)
        }
        holders.foreach(_.methodMap)
      })

    // Traverse both shadows and shadowedBy recursively to find all related methods, this
    // is arguably over traversing in that it finds all whereas simply finding base & super
    // implementations would be sufficient for most use cases
    val visitedMethods = mutable.Set[ApexMethodLike](this)
    val queue          = mutable.Queue[ApexMethodLike]()
    queue.enqueueAll(getShadowMethods(this))
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      visitedMethods.add(current)
      queue.enqueueAll(
        getShadowMethods(current)
          .diff(visitedMethods)
      )
    }
    visitedMethods.toSet
  }

  def summary: MethodSummary = {
    MethodSummary(
      location.location,
      idLocation,
      name.toString,
      modifiers,
      typeName,
      parameters.map(_.serialise),
      hasBlock,
      dependencySummary()
    )
  }
}

/** Apex defined fields core features, be they full or summary style */
trait ApexFieldLike extends FieldDeclaration with Referenceable {
  val thisTypeId: TypeId
  override def thisTypeIdOpt: Option[TypeId] = Some(thisTypeId)
  val nature: Nature
  val idTarget: Option[TypeName] = None

  def isReadOnly: Boolean = modifiers.contains(FINAL_MODIFIER)

  def summary: FieldSummary = {
    FieldSummary(
      location.location,
      idLocation,
      name.toString,
      nature,
      modifiers,
      typeName,
      readAccess,
      writeAccess,
      dependencySummary()
    )
  }
}

/** Apex defined types core features, be they full or summary style */
trait ApexDeclaration extends DependentType with IdLocatable {

  val module: OPM.Module
  val isEntryPoint: Boolean

  /** Calculate a hash for the source code of the declaration */
  def sourceHash: Int

  /** Create a summary for the of the declaration */
  def summary: TypeSummary

  override def nestedTypes: ArraySeq[ApexDeclaration]

  /** Get referenceable element at the specified line and offset.
    * @param line   The 1-based line number within the source file.
    * @param offset The 0-based character offset within the line.
    * @return       A map from source locations to their corresponding validation results.
    */
  def findReferenceableFromLocation(line: Int, offset: Int): Option[Referenceable] = None
}

/** Apex defined type for parsed (aka Full) classes, interfaces, enums & triggers */
trait ApexFullDeclaration extends ApexDeclaration {

  /** Get validation map for the specified line and offset.
    * @param line   The 1-based line number within the source file.
    * @param offset The 0-based character offset within the line.
    * @return       A map from locations to their corresponding validation results.
    */
  def getValidationMap(line: Int, offset: Int): Map[Location, ValidationResult]

  /** Locate an ApexDeclaration for the passed typeName that was extracted from location..
    * @param searchTerm The name of the type to search for.
    * @param location The location of the type in the source code.
    * @return       An option containing the source reference, if found.
    */
  def findDeclarationFromSourceReference(
    searchTerm: String,
    location: Location
  ): Option[TypeDeclaration with Locatable]
}

/** Apex defined trigger of either full or summary type */
trait ApexTriggerDeclaration extends ApexDeclaration {
  override val isEntryPoint: Boolean = true
}

/** Apex defined classes, interfaces, enum of either full or summary type */
trait ApexClassDeclaration extends ApexDeclaration with DependencyHolder with Referenceable {
  val localFields: ArraySeq[ApexFieldLike]
  val localMethods: ArraySeq[ApexMethodLike]
  val localConstructors: ArraySeq[ApexConstructorLike]

  override lazy val thisTypeId: TypeId       = typeId
  override def thisTypeIdOpt: Option[TypeId] = Some(typeId)

  override def nestedTypes: ArraySeq[ApexClassDeclaration]

  override def isCustomException: Boolean =
    name.endsWith(Names.Exception) || superClassDeclaration
      .map(_.typeName)
      .contains(TypeNames.Exception) || superClassDeclaration.exists(_.isCustomException)

  /** Override to resolve conflict, TypeDeclaration & DependencyHolder both default false */
  override val inTest: Boolean = false

  /** Override to handle request to flush the type to passed cache if dirty */
  def flush(pc: ParsedCache, context: PackageContext): Unit

  /** Reset local caches ready for revalidation */
  override def preReValidate(): Unit = {
    super.preReValidate()
    _methodMap = None
    _constructorMap = None
    _superClassDeclaration = None
    _interfaceDeclarations = ArraySeq.empty
  }

  private var _superClassDeclaration: Option[TypeDeclaration] = None

  override def superClassDeclaration: Option[TypeDeclaration] = {
    // Don't cache empty as maybe currently missing
    if (_superClassDeclaration.isEmpty) {
      _superClassDeclaration = superClass.flatMap(sc => TypeResolver(sc, this).toOption)
    }
    _superClassDeclaration
  }

  private var _interfaceDeclarations: ArraySeq[TypeDeclaration] = ArraySeq.empty

  override def interfaceDeclarations: ArraySeq[TypeDeclaration] = {
    // Don't cache empty as maybe currently missing
    if (_interfaceDeclarations.size != interfaces.size) {
      _interfaceDeclarations = interfaces.flatMap(i => TypeResolver(i, this).toOption)
    }
    _interfaceDeclarations
  }

  override lazy val isComplete: Boolean = {
    (superClassDeclaration.nonEmpty && superClassDeclaration.get.isComplete) || superClass.isEmpty
  }

  override lazy val fields: ArraySeq[FieldDeclaration] = {
    ArraySeq.unsafeWrapArray(
      localFields
        .groupBy(f => f.name)
        .collect {
          case (_, single) if single.length == 1 => single.head
          case (_, duplicates) =>
            duplicates.tail.foreach {
              case af: ApexFieldLike =>
                OrgInfo.logError(af.idPathLocation, s"Duplicate field/property: '${af.name}'")
              case _ => assert(false)
            }
            duplicates.head
        }
        .toArray
    )
  }

  private lazy val staticMethods: ArraySeq[MethodDeclaration] = {
    localMethods.filter(_.isStatic) ++
      (superClassDeclaration match {
        case Some(td: ApexClassDeclaration) =>
          td.staticMethods
        case _ =>
          MethodDeclaration.emptyMethodDeclarations
      })
  }

  lazy val isPageController: Boolean = {
    getTypeDependencyHolders.toIterable.exists(tid =>
      tid.typeName == TypeNames.Page || tid.typeName == TypeNames.Component
    )
  }

  private lazy val hasExternalMembers: Boolean = {
    localMethods.exists(_.isExternallyVisible) || localFields.exists(_.isExternallyVisible)
  }

  private lazy val isAsync: Boolean = {
    !isAbstract && Seq(
      TypeName(Seq(Names.Batchable, Names.Database)),
      TypeName(Seq(Names.Schedulable, Names.System)),
      TypeName(Seq(Names.Queueable, Names.System)),
      TypeName(Seq(Names.Finalizer, Names.System))
    ).exists(implements(_, ignoreGenerics = true))
  }

  override lazy val isEntryPoint: Boolean = {
    outerTypeName.isEmpty && isExternallyVisible || hasExternalMembers || isPageController || isAsync
  }

  def methodMap: MethodMap = {
    if (_methodMap.isEmpty)
      _methodMap = Some(createMethodMap)
    _methodMap.get
  }

  def constructorMap: ConstructorMap = {
    if (_constructorMap.isEmpty) {
      _constructorMap = Some(createConstructorMap)
    }
    _constructorMap.get
  }

  private var _methodMap: Option[MethodMap]           = None
  private var _constructorMap: Option[ConstructorMap] = None

  private def createMethodMap: MethodMap = {
    val errorLocation = Some(idPathLocation)
    superClassDeclaration match {
      case Some(at: ApexClassDeclaration) =>
        MethodMap(this, errorLocation, at.methodMap, localMethods, interfaceDeclarations)
      case Some(td: TypeDeclaration) =>
        MethodMap(this, errorLocation, MethodMap(td), localMethods, interfaceDeclarations)
      case _ =>
        MethodMap(this, errorLocation, MethodMap.empty(), localMethods, interfaceDeclarations)
    }
  }

  private def createConstructorMap: ConstructorMap = {
    val errorLocation = Some(idPathLocation)
    superClassDeclaration match {
      case Some(at: ApexClassDeclaration) =>
        ConstructorMap(this, errorLocation, localConstructors, at.constructorMap)
      case Some(td: TypeDeclaration) =>
        ConstructorMap(this, errorLocation, localConstructors, ConstructorMap(td))
      case _ =>
        ConstructorMap(this, errorLocation, localConstructors, ConstructorMap.empty)
    }
  }

  override def methods: ArraySeq[MethodDeclaration] = {
    methodMap.allMethods
  }

  override def constructors: ArraySeq[ConstructorDeclaration] = {
    constructorMap.allConstructors
  }

  override def findMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    verifyContext: VerifyContext
  ): Either[String, MethodDeclaration] = {
    methodMap.findMethod(name, params, staticContext, verifyContext)
  }

  override def findConstructor(
    params: ArraySeq[TypeName],
    verifyContext: VerifyContext
  ): Either[String, ConstructorDeclaration] = {
    nature match {
      case CLASS_NATURE => constructorMap.findConstructorByParams(params, verifyContext)
      case _            => Left(s"Type cannot be constructed: $typeName")
    }

  }

  def bombScore(total: Int): (Int, Int, Double) = {
    val magicScale = 1.7306 // Places score 0-100

    val typeCache    = new TypeCache()
    val dependencies = mutable.Set[TypeId]()
    gatherDependencies(dependencies, apexOnly = true, outerTypesOnly = true, typeCache)
    dependencies.remove(typeId)
    val uses   = dependencies.size
    val usedBy = getTypeDependencyHolders.size
    val score = magicScale * Math.log(1 + (uses * 2000).toDouble / total) * Math.log(
      1 + (usedBy * 2000).toDouble / total
    )
    val roundScore =
      BigDecimal(score.toString).setScale(2, BigDecimal.RoundingMode.HALF_UP).doubleValue
    (uses, usedBy, roundScore)
  }

  override def findReferenceableFromLocation(line: Int, offset: Int): Option[Referenceable] = {
    if (idLocation.contains(line, offset))
      Some(this)
    else
      localFields
        .find(_.idLocation.contains(line, offset))
        .orElse(
          localMethods
            .find(_.idLocation.contains(line, offset))
        )
        .orElse(
          localConstructors
            .find(_.idLocation.contains(line, offset))
        )
        .orElse(
          nestedTypes.view
            .flatMap(td => td.findReferenceableFromLocation(line, offset))
            .headOption
        )
        .collect { case ref: Referenceable => ref }
  }

  override def toString: String = {
    var typeStr = modifiers.map(_.name).mkString(" ") + " " + nature.value + " " + name
    if (superClass.exists(_ != TypeNames.InternalObject)) {
      typeStr += " extends " + superClass.get.name
    }
    if (interfaces.nonEmpty) {
      typeStr += " implements " + interfaces.map(i => i.name).mkString(", ")
    }
    typeStr
  }
}
