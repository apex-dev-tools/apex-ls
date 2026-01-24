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
import com.nawforce.apexlink.types.core.MethodDeclaration.emptyMethodDeclarations
import com.nawforce.apexlink.types.core.{
  AnyReturnMethodDeclaration,
  MethodDeclaration,
  TypeDeclaration
}
import com.nawforce.apexlink.types.platform.PlatformMethod
import com.nawforce.apexlink.types.synthetic.CustomMethodDeclaration
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, INTERFACE_NATURE}
import com.nawforce.pkgforce.path.{Location, PathLocation}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

sealed abstract class MethodCallError(final val value: String)

case object NO_MATCH_ERROR extends MethodCallError("No matching method found")

case object AMBIGUOUS_ERROR extends MethodCallError("Ambiguous method call")

final case class MethodMap private (
  typeName: Option[TypeName],
  td: Option[ApexClassDeclaration],
  methodsByName: Map[(Name, Int), Array[MethodDeclaration]],
  errors: List[Issue]
) {

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

    // Try for an exact match first
    val exactMatches =
      staticContextMatches.filter(
        _.hasCompatibleParameters(params, allowPlatformGenericEquivalence = true)
      )
    if (exactMatches.length == 1)
      return Right(exactMatches.head)
    else if (exactMatches.length > 1)
      return Left(AMBIGUOUS_ERROR)

    // If no exact we need to search for possible in two stages, either using strict assignment or lax if
    // we are still short of a possible match
    var found =
      mostSpecificMatch(strict = true, staticContextMatches, params, context)
        .getOrElse(
          mostSpecificMatch(strict = false, staticContextMatches, params, context)
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

object MethodMap {
  type WorkingMap = mutable.HashMap[(Name, Int), List[MethodDeclaration]]

  private val DISALLOWED_TYPES_FOR_AURAENABLED = List(TypeNames.Set$)

  def empty(): MethodMap = {
    new MethodMap(None, None, Map(), Nil)
  }

  private def toMap(workingMap: WorkingMap): Map[(Name, Int), Array[MethodDeclaration]] = {
    workingMap.map(kv => (kv._1, kv._2.toArray)).toMap
  }

  /** Construct for an arbitrary type declaration. This is just for simple type
    * declarations with non-complex needs.
    */
  def apply(td: TypeDeclaration): MethodMap = {
    val workingMap = new WorkingMap()
    td.methods.foreach(method => {
      val key = (method.name, method.parameters.length)
      workingMap.put(key, method :: workingMap.getOrElse(key, Nil))
    })
    new MethodMap(Some(td.typeName), None, toMap(workingMap), Nil)
  }

  /** Construct for a complex type declaration from a super class map.
    */
  def apply(
    td: TypeDeclaration,
    location: Option[PathLocation],
    superClassMap: MethodMap,
    newMethods: ArraySeq[MethodDeclaration],
    interfaces: ArraySeq[TypeDeclaration]
  ): MethodMap = {
    val errors                         = mutable.Buffer[Issue]()
    var workingMap                     = createStartingWorkingMap(superClassMap)
    val (staticLocals, instanceLocals) = newMethods.partition(_.isStatic)

    // Add instance methods first with validation checks and reset of shadowing
    instanceLocals.foreach(method => {
      method match {
        case am: ApexMethodLike => am.resetShadows()
        case _                  =>
      }
      val errorCount = errors.length
      applyInstanceMethod(workingMap, td, method, errors).foreach(overriddenMethod => {
        if (errors.length == errorCount) {
          method match {
            case am: ApexMethodLike => am.addShadow(overriddenMethod)
            case _                  => ()
          }
        }
      })

    })

    // Now strip out none test visible/abstract inherited privates excluding when a super class is in the same file as
    // td, in that case the private methods are visible. Yeah, this is very odd behaviour, but might be related to how
    // Java compiles inner classes as outers.
    val sameFileSuperclassPrivateMethods = findSameFileSuperclassPrivateMethods(td)
    workingMap.foreach(keyAndMethodGroup => {
      val methods = keyAndMethodGroup._2.filterNot(method => {
        method.visibility.getOrElse(PRIVATE_MODIFIER) == PRIVATE_MODIFIER && !method.isAbstract &&
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
    // Add errors for any static methods on inner classes
    if (td.outerTypeName.isDefined && td.nature == CLASS_NATURE) {
      staticLocals.foreach(m => {
        setMethodError(m, s"Static methods are not allowed in inner classes", errors)
      })
    }
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
    new MethodMap(Some(td.typeName), ad, toMap(workingMap), errors.toList)
  }

  /** Create a starting working map from super class map, just removing statics initially
    */
  private def createStartingWorkingMap(parentMap: MethodMap): WorkingMap = {
    var workingMap = new WorkingMap()
    parentMap.methodsByName.foreach(superMethodGroup => {
      val superMethods = superMethodGroup._2.filterNot(_.isStatic)
      workingMap.put(superMethodGroup._1, superMethods.toList)
    })
    workingMap = workingMap.filterNot(_._2.isEmpty)
    workingMap
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
          method.visibility.getOrElse(PRIVATE_MODIFIER) == PRIVATE_MODIFIER && !method.isStatic
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
    // up can result in an inverted shadowing relationship when both interfaces contain the same method
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
      .filterNot(m => m.isStatic || isObjectMethod(m).contains(true))
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

  /** Add an instance method into the working map.
    * @return method the passed one is overriding if any
    */
  private def applyInstanceMethod(
    workingMap: WorkingMap,
    td: TypeDeclaration,
    method: MethodDeclaration,
    errors: mutable.Buffer[Issue]
  ): Option[MethodDeclaration] = {
    val key     = (method.name, method.parameters.length)
    val methods = workingMap.getOrElse(key, Nil)
    val matched = methods.find(mapMethod => areSameMethodsIgnoringReturn(mapMethod, method))

    // Not overriding
    if (matched.isEmpty) {
      if (method.isOverride && td.isComplete) {
        setMethodError(
          method,
          s"Method '${method.name}' does not override a virtual or abstract method",
          errors
        )
      }
      workingMap.put(key, method :: methods)
      return None
    }

    // Error & ignore duplicates
    val matchedMethod = matched.get
    if (isDuplicateMethod(matchedMethod, method, errors))
      return None

    // We have an attempted override, update map with it as probably was meant to be
    workingMap.put(key, method :: methods.filterNot(_ eq matchedMethod))

    // Is standard object method override
    isObjectMethod(method) match {
      case Some(true) => return Some(matchedMethod)
      case Some(false) =>
        if (isObjectMethod(matchedMethod).contains(true))
          return None // Different return means we are hiding, not shadowing
      case None if isEqualsLike(method) =>
        return None // Replacing 'equals' with single arg func with non-Object param
      case None => ()
    }

    // Batch allows an Iterable<T> start method instead of usual QueryLocator return
    if (
      isBatchableStartWithListOrIterable(method) &&
      isBatchableStartWithQueryLocator(matchedMethod) &&
      td.implements(TypeName(Seq(Names.Batchable, Names.Database)), ignoreGenerics = true)
    ) {
      return Some(matchedMethod)
    }

    // Return types should match, but allow for Any handling
    if (!haveCompatibleReturnTypes(matchedMethod.typeName, method.typeName)) {
      setMethodError(
        method,
        s"Method '${method.name}' has wrong return type to override, should be '${matched.get.typeName}'",
        errors
      )
      return Some(matchedMethod)
    }

    // See https://github.com/nawforce/override-bench for validation logic
    val isBasePrivate = matchedMethod.visibility.getOrElse(PRIVATE_MODIFIER) == PRIVATE_MODIFIER
    val baseModifier =
      matchedMethod.modifiers.intersect(Seq(ABSTRACT_MODIFIER, VIRTUAL_MODIFIER)).headOption
    val superOverrideModifier =
      method.modifiers.contains(OVERRIDE_MODIFIER)
    if (!isBasePrivate) {
      if (baseModifier.isEmpty) {
        setMethodError(
          method,
          s"Method '${method.name}' can not override non-virtual/non-abstract method",
          errors
        )
        return Some(matchedMethod)
      } else if (!superOverrideModifier && !method.isAbstract) {
        setMethodError(method, s"Method '${method.name}' must use the 'override' keyword", errors)
        return Some(matchedMethod)
      }
    } else if (
      !superOverrideModifier &&
      method.visibility
        .getOrElse(PRIVATE_MODIFIER) != PRIVATE_MODIFIER && areInSameApexFile(method, matchedMethod)
    ) {
      setMethodError(method, s"Method '${method.name}' must use the 'override' keyword", errors)
      return Some(matchedMethod)
    }

    if (isVisibilityReduced(matchedMethod.visibility, method.visibility)) {
      setMethodError(
        method,
        s"Method '${method.name}' can not reduce visibility in override",
        errors
      )
      return Some(matchedMethod)
    }

    if (
      isBasePrivate &&
      baseModifier.nonEmpty &&
      superOverrideModifier &&
      !areInSameApexFile(matchedMethod, method) &&
      !(method.inTest && matchedMethod.isTestVisible)
    ) {
      setMethodError(method, s"Method '${method.name}' can not override a private method", errors)
      return Some(matchedMethod)
    }

    if (matchedMethod.visibility.contains(PRIVATE_MODIFIER)) {
      if (baseModifier.contains(ABSTRACT_MODIFIER))
        setMethodError(
          method,
          s"Overriding a private abstract method can cause a GACK, change to protected, public or global",
          errors
        )
      else
        setMethodError(
          method,
          s"Overriding a private method may not work, change to protected, public or global",
          errors
        )
    }

    Some(matchedMethod)
  }

  private def isVisibilityReduced(
    baseVisibility: Option[Modifier],
    superVisibility: Option[Modifier]
  ): Boolean = {
    baseVisibility.getOrElse(PRIVATE_MODIFIER).methodOrder > superVisibility
      .getOrElse(PRIVATE_MODIFIER)
      .methodOrder
  }

  private def isDuplicateMethod(
    existingMethod: MethodDeclaration,
    newMethod: MethodDeclaration,
    errors: mutable.Buffer[Issue]
  ): Boolean = {
    val sameClass = (existingMethod, newMethod) match {
      case (am1: ApexMethodLike, am2: ApexMethodLike) => am1.thisTypeId == am2.thisTypeId
      case (pm1: PlatformMethod, pm2: PlatformMethod) => pm1.typeDeclaration eq pm2.typeDeclaration
      case _                                          => false
    }
    if (!sameClass)
      return false

    existingMethod match {
      case matchedMethod: ApexMethodLike =>
        if (newMethod.hasSameParameters(matchedMethod, allowPlatformGenericEquivalence = false))
          setMethodError(
            newMethod,
            s"Method '${newMethod.name}' is a duplicate of an existing method at ${matchedMethod.idLocation
                .displayPosition()}",
            errors
          )
        else
          setMethodError(
            newMethod,
            s"Method '${newMethod.name}' can not use same platform generic interface as existing method at ${matchedMethod.idLocation
                .displayPosition()}",
            errors
          )
      case _ => ()
    }
    true
  }

  /** Is this an Object style method.
    * @return None if not, Some(true) is exact, Some(false) if only name & parameters match
    */
  private def isObjectMethod(method: MethodDeclaration): Option[Boolean] = {
    method.name match {
      case XNames.Equals
          if method.parameters.length == 1
            && method.parameters.head.typeName == TypeNames.InternalObject =>
        Some(method.typeName == TypeNames.Boolean)
      case XNames.Hashcode if method.parameters.isEmpty =>
        Some(method.typeName == TypeNames.Integer)
      case XNames.Tostring if method.parameters.isEmpty =>
        Some(method.typeName == TypeNames.String)
      case _ => None
    }
  }

  private def haveCompatibleReturnTypes(
    baseTypeName: TypeName,
    overrideTypeName: TypeName
  ): Boolean = {
    (baseTypeName == overrideTypeName) ||
    (baseTypeName.isAnyIterator && overrideTypeName.isIterator)
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

  /** Determine if two methods are considered the same without looking at the return type. For
    * 'equals' we consider them the same if they both have a single parameter even if that parameter
    * differs. This is because defining equals in a class will hide the Object equals method even when
    * the arguments don't match.
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
    else if (
      isBatchableStartWithQueryLocator(interfaceMethod) && isBatchableStartWithListOrIterable(
        implMethod
      )
    )
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

  private def isBatchableStartWithQueryLocator(method: MethodDeclaration): Boolean = {
    method.name == XNames.Start &&
    method.typeName == TypeNames.QueryLocator &&
    !method.isStatic &&
    method.parameters.length == 1 && method.parameters.head.typeName == TypeNames.BatchableContext
  }

  private def isBatchableStartWithListOrIterable(method: MethodDeclaration): Boolean = {
    method.name == XNames.Start &&
    (method.typeName.isIterable || method.typeName.isList) &&
    !method.isStatic &&
    method.parameters.length == 1 && method.parameters.head.typeName == TypeNames.BatchableContext
  }

  private def isPublicOrGlobal(method: MethodDeclaration): Boolean = {
    method.visibility.contains(PUBLIC_MODIFIER) || method.visibility.contains(GLOBAL_MODIFIER)
  }

  private def hasGhostedMethods(module: OPM.Module, methods: List[MethodDeclaration]): Boolean = {
    methods.exists(method =>
      module.isGhostedType(method.typeName) ||
        methods
          .exists(method => method.parameters.map(_.typeName).exists(module.isGhostedType))
    )
  }

}
