/*
 Copyright (c) 2017 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.cst.AssignableSupport.{AssignableOptions, isAssignable}
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.names.{TypeNames, XNames}
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexDeclaration, ApexMethodLike}
import com.nawforce.apexlink.types.core.MethodDeclaration.{
  emptyMethodDeclarations,
  emptyMethodDeclarationsSet
}
import com.nawforce.apexlink.types.core.{MethodDeclaration, ParameterDeclaration, TypeDeclaration}
import com.nawforce.apexlink.types.platform.{GenericPlatformMethod, PlatformMethod}
import com.nawforce.apexlink.types.synthetic.CustomMethodDeclaration
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.modifiers.{
  ABSTRACT_MODIFIER,
  AURA_ENABLED_ANNOTATION,
  GLOBAL_MODIFIER,
  Modifier,
  PRIVATE_MODIFIER,
  PROTECTED_MODIFIER,
  PUBLIC_MODIFIER
}
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, INTERFACE_NATURE}
import com.nawforce.pkgforce.path.{Location, PathLocation}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

sealed abstract class MethodCallError(final val value: String)

case object NO_MATCH_ERROR extends MethodCallError("No matching method found")

case object AMBIGUOUS_ERROR extends MethodCallError("Ambiguous method call")

final case class MethodMap(
  typeName: Option[TypeName],
  td: Option[ApexClassDeclaration],
  methodsByName: Map[(Name, Int), Array[MethodDeclaration]],
  testVisiblePrivateMethods: Set[MethodDeclaration],
  errors: List[Issue]
) {
  val deepHash: Int = td.map(_.deepHash).getOrElse(0)

  /** Return all available methods */
  def allMethods: ArraySeq[MethodDeclaration] = {
    val buffer = new mutable.ArrayBuffer[MethodDeclaration]()
    methodsByName.values.foreach(methods => buffer.addAll(methods))
    ArraySeq.unsafeWrapArray(buffer.toArray)
  }

  /** Find a method, without concern for the calling context so must be an exact match. */
  def findMethod(name: Name, params: ArraySeq[TypeName]): Option[MethodDeclaration] = {
    methodsByName
      .getOrElse((name, params.length), Array())
      .find(method => method.parameters.map(_.typeName) == params)
  }

  /** Find a method using full rules for disambiguation */
  def findMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    context: VerifyContext
  ): Either[String, MethodDeclaration] = {
    findMethodCall(name, params, staticContext, context) match {
      case Right(method) => Right(method)
      case Left(err) =>
        val callee = typeName.map(_.toString).getOrElse("Unknown")
        val paramsMessage =
          if (params.nonEmpty)
            s"arguments '${params.map(_.toString).mkString(", ")}'"
          else
            "no arguments"
        val suggestion = suggestMethod(name, params, staticContext).getOrElse("")
        Left(s"${err.value} for '$name' on '$callee' taking $paramsMessage$suggestion")
    }
  }

  private def suggestMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean]
  ): Option[String] = {
    val matched = methodsByName.get((name, params.length)).flatMap(_.headOption)
    if (matched.isEmpty) {
      methodsByName
        .find(_._1._1 == name)
        .map(sameName => s", did you mean to call '${sameName._2.head.toString()}'?")
    } else if (staticContext.nonEmpty && matched.exists(_.isStatic != staticContext.get)) {
      Some(
        s", are you trying to call the instance method '${matched.get.toString()}' from a static context?"
      )
    } else {
      Some(s", wrong argument types for calling '${matched.get.toString()}'")
    }
  }

  private def findMethodCall(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    context: VerifyContext
  ): Either[MethodCallError, MethodDeclaration] = {

    val matches = methodsByName.getOrElse((name, params.length), Array())

    // Filter for right static context
    val staticContextMatches = staticContext match {
      case None    => matches
      case Some(x) => matches.filter(m => m.isStatic == x)
    }

    val testMatches =
      if (!context.thisType.inTest && testVisiblePrivateMethods.nonEmpty) {
        // If not in test code, filter out inherited private test visible methods that we allowed in
        staticContextMatches.filterNot(testVisiblePrivateMethods.contains)
      } else {
        staticContextMatches
      }

    // Try for an exact match first
    val exactMatches =
      testMatches.filter(_.hasCompatibleParameters(params, allowPlatformGenericEquivalence = true))
    if (exactMatches.length == 1)
      return Right(exactMatches.head)
    else if (exactMatches.length > 1)
      return Left(AMBIGUOUS_ERROR)

    // If no exact we need to search for possible in two stages, either using strict assignment or lax if
    // we are still short of a possible match
    var found =
      mostSpecificMatch(strict = true, testMatches, params, context)
        .getOrElse(
          mostSpecificMatch(strict = false, testMatches, params, context)
            .getOrElse(Left(NO_MATCH_ERROR))
        )

    // If not found locally search super & outer for statics
    if (found == Left(NO_MATCH_ERROR) && !staticContext.contains(false)) {
      found = findStaticMethodOn(td.flatMap(_.superClassDeclaration), name, params, context)
      if (found == Left(NO_MATCH_ERROR)) {
        found = findStaticMethodOn(td.flatMap(_.outerTypeDeclaration), name, params, context)
      }
    }

    found
  }

  /** Helper for finding static methods on related type declarations */
  private def findStaticMethodOn(
    td: Option[TypeDeclaration],
    name: Name,
    params: ArraySeq[TypeName],
    context: VerifyContext
  ): Either[MethodCallError, MethodDeclaration] = {
    td match {
      case Some(td: ApexClassDeclaration) =>
        td.methodMap.findMethodCall(name, params, Some(true), context)
      case _ =>
        Left(NO_MATCH_ERROR)
    }
  }

  /** Find the most specific method call match. This uses a most-specific selection model similar to
    * that outlined in JLS6 15.12.2.5.
    */
  private def mostSpecificMatch(
    strict: Boolean,
    matches: Array[MethodDeclaration],
    params: ArraySeq[TypeName],
    context: VerifyContext
  ): Option[Either[MethodCallError, MethodDeclaration]] = {

    val assignable = matches.filter(m => {
      val argZip = m.parameters.map(_.typeName).zip(params)
      argZip.forall(argPair =>
        isAssignable(
          argPair._1,
          argPair._2,
          context,
          AssignableOptions(strict, narrowSObjects = true)
        )
      )
    })

    if (assignable.isEmpty)
      None
    else if (assignable.length == 1)
      Some(Right(assignable.head))
    else if (params.contains(TypeNames.Any)) {
      // We might get multiple matches when input contains an any, wrap one to fake the return an Any
      // in case the assignable methods have different return types
      Some(Right(new AnyReturnMethodDeclaration(assignable.head)))
    } else {
      Some(
        assignable
          .find(method =>
            assignable.forall(m =>
              m == method || method
                .hasMoreSpecificParams(m.parameters, params, context)
                .contains(true)
            )
          )
          .map(Right(_))
          .getOrElse(Left(AMBIGUOUS_ERROR))
      )
    }
  }
}

/* Method wrapper that enforces an Any return type on the method */
class AnyReturnMethodDeclaration(method: MethodDeclaration) extends MethodDeclaration {
  override val name: Name                                 = method.name
  override val modifiers: ArraySeq[Modifier]              = method.modifiers
  override val parameters: ArraySeq[ParameterDeclaration] = method.parameters

  override def typeName: TypeName = TypeName.Any
  override def hasBlock: Boolean  = method.hasBlock
}

object MethodMap {
  type WorkingMap = mutable.HashMap[(Name, Int), List[MethodDeclaration]]

  private val DISALLOWED_TYPES_FOR_AURAENABLED = List(TypeNames.Set$)

  private val specialOverrideMethodSignatures = Set[String](
    "system.boolean equals(object)",
    "system.integer hashcode()",
    "system.string tostring(),"
  )

  private val batchOverrideMethodSignature =
    "database.querylocator start(database.batchablecontext)"

  def empty(): MethodMap = {
    new MethodMap(None, None, Map(), Set(), Nil)
  }

  private def toMap(workingMap: WorkingMap): Map[(Name, Int), Array[MethodDeclaration]] = {
    workingMap.map(kv => (kv._1, kv._2.toArray)).toMap
  }

  /** Construct for an arbitrary type declaration, for Apex classes used the other apply function
    * which builds up a proper MethodMap. This is just for other type declarations with non-complex
    * needs.
    */
  def apply(td: TypeDeclaration): MethodMap = {
    val workingMap = new WorkingMap()
    td.methods.foreach(method => {
      val key = (method.name, method.parameters.length)
      workingMap.put(key, method :: workingMap.getOrElse(key, Nil))
    })
    new MethodMap(Some(td.typeName), None, toMap(workingMap), Set(), Nil)
  }

  def apply(
    td: TypeDeclaration,
    location: Option[PathLocation],
    superClassMap: MethodMap,
    newMethods: ArraySeq[MethodDeclaration],
    interfaces: ArraySeq[TypeDeclaration]
  ): MethodMap = {

    // Create a starting working map from super class map, just removing statics initially
    var workingMap         = new WorkingMap()
    val testVisiblePrivate = mutable.Set[MethodDeclaration]()
    superClassMap.methodsByName.foreach(superMethodGroup => {
      val superMethods = superMethodGroup._2.filterNot(_.isStatic)
      workingMap.put(superMethodGroup._1, superMethods.toList)
    })
    workingMap = workingMap.filterNot(_._2.isEmpty)

    // Now build the rest of the map with the new methods
    val errors                         = mutable.Buffer[Issue]()
    val (staticLocals, instanceLocals) = newMethods.partition(_.isStatic)

    // Add instance methods first with validation checks
    instanceLocals.foreach {
      case am: ApexMethodLike => am.resetShadows()
      case _                  =>
    }
    instanceLocals.foreach(method =>
      applyInstanceMethod(workingMap, method, td.inTest, td.isComplete, errors)
    )

    // Now strip out none test visible/abstract inherited privates excluding when a super class is in the same file as
    // td, in that case the private methods are visible. Yeah, this is very odd behaviour, but might be related to how
    // Java compiles inner classes as outers.
    val sameFileSuperclassPrivateMethods = findSameFileSuperclassPrivateMethods(td)
    workingMap.foreach(keyAndMethodGroup => {
      val methods = keyAndMethodGroup._2.filterNot(method => {
        method.visibility == PRIVATE_MODIFIER && !method.isAbstract &&
        !(method.isTestVisible || isApexLocalMethod(td, method) || sameFileSuperclassPrivateMethods
          .contains(method))
      })
      workingMap.put(keyAndMethodGroup._1, methods)
    })
    workingMap = workingMap.filterNot(_._2.isEmpty)

    // For interfaces make sure we have all methods
    if (td.nature == INTERFACE_NATURE) {
      interfaces.foreach(interface => mergeInterface(workingMap, interface))
    }

    // Add local statics, de-duped
    val ignorableStatics = mutable.Set[MethodDeclaration]()
    staticLocals
      .duplicates(_.nameAndParameterTypes.toLowerCase)
      .foreach(duplicates => {
        duplicates._2.foreach(duplicate => {
          ignorableStatics.add(duplicate)
          setMethodError(
            duplicate,
            s"Method '${duplicate.name}' is a duplicate of an existing method",
            errors
          )
        })
      })

    // Add errors for any static methods that shadow instance methods
    staticLocals
      .flatMap(st =>
        instanceLocals
          .find(f => f.nameAndParameterTypes.toLowerCase == st.nameAndParameterTypes.toLowerCase)
          .map((st, _))
      )
      .foreach(methodAndStaticMethod => {
        setMethodError(
          methodAndStaticMethod._1,
          s"static method '${methodAndStaticMethod._1.name}' is a duplicate of an existing instance method",
          errors
        )
        setMethodError(
          methodAndStaticMethod._2,
          s"method '${methodAndStaticMethod._2.name}' is a duplicate of an existing static method",
          errors
        )
      })
    // Add errors for any static methods with protected modifier
    staticLocals
      .filter(s => s.modifiers.contains(PROTECTED_MODIFIER))
      .foreach(m => {
        setMethodError(m, s"protected method '${m.name}' cannot be static", errors)
      })
    staticLocals
      .filterNot(ignorableStatics.contains)
      .foreach(method => applyStaticMethod(workingMap, method))

    val ad = td match {
      case ad: ApexClassDeclaration => Some(ad)
      case _: TypeDeclaration       => None
    }

    // Validate any interface used in classes
    ad.filter(_.nature == CLASS_NATURE)
      .foreach(ad => {
        workingMap.put(
          (Names.Clone, 0),
          List(
            CustomMethodDeclaration(
              Location.empty,
              Names.Clone,
              td.typeName,
              CustomMethodDeclaration.emptyParameters
            )
          )
        )
        if (!ad.isAbstract)
          checkInterfaces(ad, location, td.isAbstract, workingMap, errors)
      })

    // Valida auraEnabled methods to disallow Sets
    checkAuraEnabledMethods(location, errors, workingMap)

    // Finally, construct the actual MethodMap
    val testVisiblePrivateSet =
      if (testVisiblePrivate.isEmpty) emptyMethodDeclarationsSet else testVisiblePrivate.toSet
    new MethodMap(Some(td.typeName), ad, toMap(workingMap), testVisiblePrivateSet, errors.toList)
  }

  private def checkAuraEnabledMethods(
    location: Option[PathLocation],
    errors: mutable.Buffer[Issue],
    workingMap: WorkingMap
  ): Unit = {

    def hasDisallowedTypes(typeName: TypeName): Boolean = {
      DISALLOWED_TYPES_FOR_AURAENABLED.exists(disAllowed => {
        disAllowed.name == typeName.name && disAllowed.outer == typeName.outer
      }) || typeName.params.exists(hasDisallowedTypes)
    }

    val auraEnabledMethods = workingMap.values.flatten.collect {
      case m: ApexMethodDeclaration if m.modifiers.contains(AURA_ENABLED_ANNOTATION) => m
    }

    auraEnabledMethods
      .filter(_.parameters.map(_.typeName).exists(hasDisallowedTypes))
      .foreach(m =>
        errors.append(
          new Issue(
            location.get.path,
            Diagnostic(
              ERROR_CATEGORY,
              location.get.location,
              s"AuraEnabled methods do not support parameter type of ${m.parameters.map(_.typeName).filter(hasDisallowedTypes).mkString(", ")}"
            )
          )
        )
      )

    auraEnabledMethods
      .filter(m => hasDisallowedTypes(m.typeName))
      .foreach(m => {
        errors.append(
          new Issue(
            location.get.path,
            Diagnostic(
              ERROR_CATEGORY,
              location.get.location,
              s"AuraEnabled methods do not support return type of ${m.typeName.toString}"
            )
          )
        )
      })
  }

  private def isApexLocalMethod(td: TypeDeclaration, method: MethodDeclaration): Boolean = {
    (td, method) match {
      case (td: ApexClassDeclaration, method: ApexMethodLike) =>
        td.typeId == method.thisTypeId
      case _ =>
        false
    }
  }

  private def findSameFileSuperclassPrivateMethods(
    td: TypeDeclaration
  ): ArraySeq[MethodDeclaration] = {
    if (td.superClass.nonEmpty) {
      val superClass = td.superClassDeclaration
      if (superClass.nonEmpty && superClass.get.paths == td.paths) {
        return superClass.get.methods.filter(method =>
          method.visibility == PRIVATE_MODIFIER && !method.isStatic
        )
      }
    }
    emptyMethodDeclarations
  }

  /** Update working map with interface methods.
    *
    * If interface methods are not in the map then we add, if they are then the 'shadow' relationship is created
    * linking from the interface method to the previously discovered impl method. This processes interfaces
    * recursively so we handle interfaces implemented by interfaces.
    *
    * @param workingMap map to add to
    * @param interface interface to process
    */
  private def mergeInterface(workingMap: WorkingMap, interface: TypeDeclaration): Unit = {
    // This should not be needed, but we can't type interfaces here due to platform types
    if (interface.nature != INTERFACE_NATURE)
      return

    // We merge top-down here to make sure shadows is always set up in correct direction, doing it bottom
    // up can result in an inverted shadowing relationship when both interfaces contains the same method
    interface.methods
      .filterNot(_.isStatic)
      .foreach(method => {
        val key     = (method.name, method.parameters.length)
        val methods = workingMap.getOrElse(key, Nil)

        val matched = methods.find(mapMethod => areSameMethodsIgnoringReturn(mapMethod, method))
        if (matched.isEmpty) {
          workingMap.put(key, method :: methods)
        } else {
          matched.get match {
            case am: ApexMethodLike => am.addShadow(method)
            case _                  => ()
          }
        }
      })

    if (interface.isInstanceOf[ApexClassDeclaration] && interface.nature == INTERFACE_NATURE) {
      interface.interfaceDeclarations.foreach(interface => mergeInterface(workingMap, interface))
    }
  }

  private def checkInterfaces(
    from: ApexClassDeclaration,
    location: Option[PathLocation],
    isAbstract: Boolean,
    workingMap: WorkingMap,
    errors: mutable.Buffer[Issue]
  ): Unit = {
    val allInterfaces = mutable.Set[TypeDeclaration]()
    from.collectInterfaces(allInterfaces)
    allInterfaces
      .foreach(interface =>
        checkInterface(from, location, isAbstract, workingMap, interface, errors)
      )
  }

  private def checkInterface(
    from: ApexClassDeclaration,
    location: Option[PathLocation],
    isAbstract: Boolean,
    workingMap: WorkingMap,
    interface: TypeDeclaration,
    errors: mutable.Buffer[Issue]
  ): Unit = {
    interface.methods
      .filterNot(m =>
        m.isStatic || specialOverrideMethodSignatures.contains(m.signature.toLowerCase())
      )
      .foreach(method => {
        val key     = (method.name, method.parameters.length)
        val methods = workingMap.getOrElse(key, Nil)
        val matched = methods.find(mapMethod => isInterfaceMethod(from, method, mapMethod))

        matched match {
          case Some(matched: ApexMethodLike) =>
            if (!isPublicOrGlobal(matched)) {
              errors.append(
                new Issue(
                  matched.location.path,
                  Diagnostic(
                    MISSING_CATEGORY,
                    matched.idLocation,
                    s"Method '${matched.signature}' from interface '${interface.typeName}' must be public or global"
                  )
                )
              )
            }
            matched.addShadow(method)
          case Some(_) => ()
          case None =>
            val module = from.moduleDeclaration.get
            if (isAbstract) {
              workingMap.put(
                key,
                method :: methods
                  .filterNot(_.hasSameSignature(method, allowPlatformGenericEquivalence = true))
              )
            } else if (!module.isGulped && !hasGhostedMethods(module, methods)) {
              location.foreach(l =>
                errors.append(
                  new Issue(
                    l.path,
                    Diagnostic(
                      MISSING_CATEGORY,
                      l.location,
                      s"Non-abstract class must implement method '${method.signature}' from interface '${interface.typeName}'"
                    )
                  )
                )
              )
            }
        }
      })
  }

  private def applyStaticMethod(workingMap: WorkingMap, method: MethodDeclaration): Unit = {
    val key     = (method.name, method.parameters.length)
    val methods = workingMap.getOrElse(key, Nil)
    val matched = methods.find(mapMethod => areSameMethodsIgnoringReturn(mapMethod, method))
    if (matched.isEmpty)
      workingMap.put(key, method :: methods)
    else if (matched.get.isStatic)
      workingMap.put(key, method :: methods.filterNot(_ eq matched.get))
  }

  /** Add an instance method into the working map. The is littered with very horrible conditional
    * logic based on what we have been able to work out from testing.
    */
  private def applyInstanceMethod(
    workingMap: WorkingMap,
    method: MethodDeclaration,
    isTest: Boolean,
    isComplete: Boolean,
    errors: mutable.Buffer[Issue]
  ): Unit = {
    val errorCount = errors.length
    val key        = (method.name, method.parameters.length)
    val methods    = workingMap.getOrElse(key, Nil)
    val matched    = methods.find(mapMethod => areSameMethodsIgnoringReturn(mapMethod, method))

    if (matched.nonEmpty) {
      val matchedMethod = matched.get
      lazy val isSpecial = {
        val matchedSignature = matchedMethod.signature.toLowerCase()
        specialOverrideMethodSignatures.contains(matchedSignature) ||
        (matchedSignature == batchOverrideMethodSignature &&
          method.typeName.outer.contains(
            TypeNames.System
          ) && method.typeName.name == XNames.Iterable)
      }

      lazy val isPlatformMethod =
        matchedMethod.isInstanceOf[PlatformMethod] || matchedMethod
          .isInstanceOf[GenericPlatformMethod]

      lazy val isInterfaceMethod =
        !matchedMethod.hasBlock && !matchedMethod.modifiers.contains(ABSTRACT_MODIFIER)

      lazy val reallyPrivateMethod =
        matchedMethod.visibility == PRIVATE_MODIFIER && !areInSameApexFile(method, matchedMethod)

      lazy val areBothPrivate =
        matchedMethod.visibility == PRIVATE_MODIFIER && method.visibility == PRIVATE_MODIFIER

      lazy val hasNonPrivateSameVisibilityModifier =
        !areBothPrivate && matchedMethod.visibility == method.visibility
      lazy val hasNonPrivateModifierInSameFile =
        areInSameApexFile(method, matchedMethod) && !areBothPrivate

      if (areInSameApexClass(matchedMethod, method)) {
        matchedMethod match {
          case matchedMethod: ApexMethodLike =>
            if (method.hasSameParameters(matchedMethod, allowPlatformGenericEquivalence = false))
              setMethodError(
                method,
                s"Method '${method.name}' is a duplicate of an existing method at ${matchedMethod.idLocation
                    .displayPosition()}",
                errors
              )
            else
              setMethodError(
                method,
                s"Method '${method.name}' can not use same platform generic interface as existing method at ${matchedMethod.idLocation
                    .displayPosition()}",
                errors
              )
          case _ => ()
        }
      } else if (
        !areSameReturnType(
          matchedMethod.typeName,
          method.typeName
        ) && !reallyPrivateMethod && !isSpecial
      ) {
        setMethodError(
          method,
          s"Method '${method.name}' has wrong return type to override, should be '${matched.get.typeName}'",
          errors
        )
      } else if (!matchedMethod.isVirtualOrAbstract && !reallyPrivateMethod) {
        setMethodError(
          method,
          s"Method '${method.name}' can not override non-virtual method",
          errors
        )
      } else if (
        !method.isVirtualOrOverride && !reallyPrivateMethod && !matchedMethod.isAbstract &&
        !isInterfaceMethod && !isSpecial && !isTest && !isPlatformMethod
      ) {
        setMethodError(method, s"Method '${method.name}' must use override keyword", errors)
      } else if (
        method.visibility.methodOrder < matchedMethod.visibility.methodOrder && !isSpecial
      ) {
        setMethodError(
          method,
          s"Method '${method.name}' can not reduce visibility in override",
          errors
        )
      } else if (
        method.isOverride && matchedMethod.isVirtualOrAbstract && matchedMethod.visibility == PRIVATE_MODIFIER
      ) {
        // Some escapes from this being bad, don't ask why, know one knows :-(
        if (
          !areInSameApexFile(
            method,
            matchedMethod
          ) && !(method.inTest && matchedMethod.isTestVisible)
        )
          setMethodError(
            method,
            s"Method '${method.name}' can not override a private method",
            errors
          )
      } else if (
        !method.isOverride &&
        matchedMethod.isAbstract &&
        (hasNonPrivateModifierInSameFile || hasNonPrivateSameVisibilityModifier)
      ) {
        setMethodError(
          method,
          s"Method '${method.name}' must use the 'override' keyword when implementing an abstract method",
          errors
        )
      }
    } else if (method.isOverride && isComplete) {
      setMethodError(
        method,
        s"Method '${method.name}' does not override a virtual or abstract method",
        errors
      )
    }

    // Shadow if all looks OK
    if (errors.length == errorCount) {
      method match {
        case am: ApexMethodLike => matched.foreach(am.addShadow)
        case _                  => ()
      }
    }

    // Update workingMap with new methods, regardless of if we error on it as probably was meant to be
    matched match {
      case None          => workingMap.put(key, method :: methods)
      case Some(matched) => workingMap.put(key, method :: methods.filterNot(_ eq matched))
    }
  }

  private def areSameReturnType(matchedTypeName: TypeName, methodTypeName: TypeName): Boolean = {
    (matchedTypeName == methodTypeName) ||
    (matchedTypeName.isAnyIterator && methodTypeName.isIterator)
  }

  private def setMethodError(
    method: MethodDeclaration,
    error: String,
    errors: mutable.Buffer[Issue],
    isWarning: Boolean = false
  ): Unit = {
    method match {
      case am: ApexMethodLike if isWarning =>
        errors.append(
          new Issue(am.location.path, Diagnostic(WARNING_CATEGORY, am.idLocation, error))
        )
      case am: ApexMethodLike =>
        errors.append(new Issue(am.location.path, Diagnostic(ERROR_CATEGORY, am.idLocation, error)))
      case _ => ()
    }
  }

  /** Determine if two Apex defined methods are declared in the same Apex file. */
  private def areInSameApexFile(m1: MethodDeclaration, m2: MethodDeclaration): Boolean = {
    (m1, m2) match {
      case (am1: ApexMethodLike, am2: ApexMethodLike) => am1.location.path == am2.location.path
      case _                                          => false
    }
  }

  /** Determine if two methods are declared in the same Apex class. The implementation is a bit
    * awkward due to how apex defined and platform methods diff in representation.
    */
  private def areInSameApexClass(m1: MethodDeclaration, m2: MethodDeclaration): Boolean = {
    (m1, m2) match {
      case (am1: ApexMethodLike, am2: ApexMethodLike) => am1.thisTypeId == am2.thisTypeId
      case (pm1: PlatformMethod, pm2: PlatformMethod) => pm1.typeDeclaration eq pm2.typeDeclaration
      case _                                          => false
    }
  }

  /** Determine if two methods are considered the same without looking at the return type. For
    * 'equals' we consider them the same if they both have a single parameter even if that parameter
    * differs. This is because defining equals in a class will hide the Object equals method if the
    * arguments don't match.
    */
  private def areSameMethodsIgnoringReturn(
    method: MethodDeclaration,
    other: MethodDeclaration
  ): Boolean = {
    if (
      method.name == other.name &&
      method.hasSameParameters(other, allowPlatformGenericEquivalence = true)
    )
      true
    else if (isEqualsLike(method) && isEqualsLike(other))
      true
    else
      false
  }

  /** Check if the implMethod fulfills the contract of the interfaceMethod. As usual there are
    * plenty of oddities to handle to determine this.
    */
  private def isInterfaceMethod(
    from: ApexClassDeclaration,
    interfaceMethod: MethodDeclaration,
    implMethod: MethodDeclaration
  ): Boolean = {
    if (
      implMethod.name == interfaceMethod.name &&
      canAssign(interfaceMethod.typeName, implMethod.typeName, from) &&
      interfaceMethod.fulfillsInterfaceMethodParams(from, implMethod)
    ) {
      true
    } else if (isEqualsLike(interfaceMethod) && isEqualsLike(implMethod))
      true
    else if (isDatabaseBatchableStart(interfaceMethod) && isDatabaseBatchableIterable(implMethod))
      true
    else
      false
  }

  /** A helper to invoke isAssignable which need a VerifyContext */
  private def canAssign(toType: TypeName, fromType: TypeName, from: TypeDeclaration): Boolean = {
    if (toType == fromType)
      true
    else {
      from match {
        case ad: ApexDeclaration =>
          val context = new TypeVerifyContext(None, ad, None, enablePlugins = false)
          isAssignable(toType, fromType, context)
        case _ =>
          false
      }
    }
  }

  private def isEqualsLike(method: MethodDeclaration): Boolean = {
    method.name == XNames.Equals &&
    method.typeName == TypeNames.Boolean &&
    !method.isStatic &&
    method.parameters.length == 1
  }

  private def isDatabaseBatchableStart(method: MethodDeclaration): Boolean = {
    method.name == XNames.Start &&
    method.typeName == TypeNames.QueryLocator &&
    !method.isStatic &&
    method.parameters.length == 1 && method.parameters.head.typeName == TypeNames.BatchableContext
  }

  private def isDatabaseBatchableIterable(method: MethodDeclaration): Boolean = {
    method.name == XNames.Start &&
    (method.typeName.isIterable || method.typeName.isList) &&
    !method.isStatic &&
    method.parameters.length == 1 && method.parameters.head.typeName == TypeNames.BatchableContext
  }

  private def isPublicOrGlobal(method: MethodDeclaration): Boolean = {
    method.visibility == PUBLIC_MODIFIER || method.visibility == GLOBAL_MODIFIER
  }

  private def hasGhostedMethods(module: OPM.Module, methods: List[MethodDeclaration]): Boolean = {
    methods.exists(method =>
      module.isGhostedType(method.typeName) ||
        methods
          .exists(method => method.parameters.map(_.typeName).exists(module.isGhostedType))
    )
  }

}
