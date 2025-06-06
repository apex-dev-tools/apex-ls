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

package com.nawforce.apexlink.types.core

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.cst.AssignableSupport.{AssignableOptions, isAssignable}
import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.diagnostics.IssueOps
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.finding.TypeResolver.TypeResponse
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.names.{TypeNames, XNames}
import com.nawforce.apexlink.org.{OPM, OrgInfo}
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, PreReValidatable}
import com.nawforce.apexlink.types.other.Component
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.apexlink.types.synthetic.{
  CustomField,
  CustomFieldDeclaration,
  LocatableCustomFieldDeclaration
}
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, INTERFACE_NATURE, Nature}
import com.nawforce.pkgforce.path.{PathLike, UnsafeLocatable}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait BlockDeclaration extends DependencyHolder {
  val isStatic: Boolean
}

object BlockDeclaration {
  val emptyBlockDeclarations: ArraySeq[BlockDeclaration] = ArraySeq()
}

trait FieldDeclaration extends DependencyHolder with UnsafeLocatable with Dependent {
  val name: Name
  val modifiers: ArraySeq[Modifier]
  val typeName: TypeName
  val idTarget: Option[TypeName]
  val readAccess: Modifier
  val writeAccess: Modifier

  def visibility: Option[Modifier] =
    modifiers.find(m => ApexModifiers.visibilityModifiers.contains(m))

  def isExternallyVisible: Boolean =
    modifiers.exists(FieldDeclaration.externalFieldModifiers.contains)

  def isStatic: Boolean = modifiers.contains(STATIC_MODIFIER)

  override def toString: String = {
    (if (modifiers.nonEmpty)
       modifiers
         .map(_.toString)
         .mkString(" ") + " "
     else "") + typeName.toString + " " + name.toString
  }

  // Create an SObjectField version of this field
  def getSObjectStaticField(
    shareTypeName: Option[TypeName],
    module: Option[OPM.Module]
  ): CustomField = {
    def preloadSObject(typeName: TypeName): TypeResponse = {
      module.map(m => TypeResolver(typeName, m)).getOrElse(PlatformTypes.get(typeName, None))
    }

    // Some messy special cases
    if (typeName == TypeNames.IdType && idTarget.nonEmpty) {
      // Id field that carries a target SObjectType returns 'fields'
      preloadSObject(idTarget.get)
      CustomFieldDeclaration(name, TypeNames.sObjectFields$(idTarget.get), None, asStatic = true)
    } else if (CustomField.isSObjectPrimitive(typeName)) {
      // Primitives (including other Id types)
      if (shareTypeName.nonEmpty && name == Names.RowCause)
        CustomFieldDeclaration(
          name,
          TypeNames.sObjectFieldRowCause$(shareTypeName.get),
          None,
          asStatic = true
        )
      else {
        Option(location)
          .map(l =>
            LocatableCustomFieldDeclaration(l, name, TypeNames.SObjectField, None, asStatic = true)
          )
          .getOrElse(CustomFieldDeclaration(name, TypeNames.SObjectField, None, asStatic = true))
      }
    } else if (name.value.endsWith("__r") && typeName.isRecordSet) {
      // Relationship field that needs unwrapping
      val targetSObject = typeName.params.head
      preloadSObject(targetSObject)
      CustomFieldDeclaration(name, TypeNames.sObjectFields$(targetSObject), None, asStatic = true)
    } else {
      // Otherwise must be a SObject, but if Platform it might need loading
      preloadSObject(typeName)
      CustomFieldDeclaration(name, TypeNames.sObjectFields$(typeName), None, asStatic = true)
    }
  }
}

object FieldDeclaration {
  val emptyFieldDeclarations: ArraySeq[FieldDeclaration] = ArraySeq()
  private val externalFieldModifiers: Set[Modifier] =
    Set(GLOBAL_MODIFIER, INVOCABLE_VARIABLE_ANNOTATION)
}

trait ParameterDeclaration {
  val name: Name
  def typeName: TypeName

  def serialise: ParameterSummary = {
    ParameterSummary(name.toString, typeName)
  }

  def summary: ParameterSummary = serialise

  override def toString: String = {
    typeName.toString + " " + name.toString
  }
}

trait Parameters {
  val parameters: ArraySeq[ParameterDeclaration]

  /** Test if this params are compatible with those passed. Ideally this would just be a comparison
    * of type names but there is a quirk in how platform generic interfaces are handled.
    */
  def hasCompatibleParameters(
    params: ArraySeq[TypeName],
    allowPlatformGenericEquivalence: Boolean
  ): Boolean = {
    if (parameters.length == params.length) {
      parameters
        .zip(params)
        .forall(paramPair => {
          paramPair._1.typeName == paramPair._2 ||
          (allowPlatformGenericEquivalence &&
            paramPair._1.typeName.params.nonEmpty && areSameGenericTypes(
              paramPair._1.typeName,
              paramPair._2
            ))
        })
    } else {
      false
    }
  }

  /** Determine if this params is a more specific version of the passed params. For this to be true
    * all the parameters of this parameters must be assignable to the corresponding parameter of the
    * other method. However, when dealing with RecordSets (SOQL results) we also prioritise degrees
    * of specificness and use those to select as well.
    */
  def hasMoreSpecificParams(
    otherParams: ArraySeq[ParameterDeclaration],
    params: ArraySeq[TypeName],
    context: VerifyContext
  ): Option[Boolean] = {
    if (parameters.length != otherParams.length || parameters.length != params.length)
      return None

    val zip = params.lazyZip(otherParams).lazyZip(parameters).toList
    Some(zip.forall(tuple => {
      if (tuple._1.isRecordSet) {
        val sObjectType = tuple._1.params.head
        val otherScore  = scoreRecordSetAssignability(tuple._2.typeName, sObjectType)
        val thisScore   = scoreRecordSetAssignability(tuple._3.typeName, sObjectType)
        thisScore.nonEmpty && (otherScore.isEmpty || thisScore.get < otherScore.get)
      } else {
        isAssignable(tuple._2.typeName, tuple._3.typeName, context)
      }
    }))
  }

  /** Determine if parameter type names are considered the same. During method and constructor calls
    * some platform generics are considered equivalent regardless of the type parameters used. Yeah,
    * it's a mess of a language.
    */
  private def areSameGenericTypes(param: TypeName, other: TypeName): Boolean = {
    param.equalsIgnoreParamTypes(other) &&
    ( // Ignore generic type params on these
      (param.outer.contains(TypeNames.System) && param.name == XNames.Iterable) ||
        (param.outer.contains(TypeNames.System) && param.name == XNames.Iterator) ||
        (param.outer.contains(TypeNames.Database) && param.name == Names.Batchable)
    )
  }

  /** Create a score for toType reflecting its priority (low is high) when matching against a
    * RecordSet of sObjectType. The ordering here was empirically derived, having all of these
    * available as possible matches does not create an ambiguity error, although the single record
    * conversion may fail at runtime.
    */
  private def scoreRecordSetAssignability(toType: TypeName, sObjectType: TypeName): Option[Int] = {
    if (toType == TypeNames.listOf(sObjectType))
      Some(0)
    else if (toType.isSObjectList)
      Some(1)
    else if (toType == sObjectType)
      Some(2)
    else if (toType == TypeNames.SObject)
      Some(3)
    else if (toType.isObjectList)
      Some(4)
    else if (toType == TypeNames.InternalObject)
      Some(5)
    else None
  }
}

trait ConstructorDeclaration extends DependencyHolder with Dependent with Parameters {
  val modifiers: ArraySeq[Modifier]

  def visibility: Modifier =
    modifiers.find(m => ApexModifiers.visibilityModifiers.contains(m)).getOrElse(PRIVATE_MODIFIER)
  def isTestVisible: Boolean = modifiers.contains(TEST_VISIBLE_ANNOTATION)

  /** Test if the passed constructor has params compatible with this method. Ideally this would just
    * be a comparison of type names but there is a quirk in how platform generic interfaces are
    * handled.
    */
  def hasSameParameters(
    other: ConstructorDeclaration,
    allowPlatformGenericEquivalence: Boolean
  ): Boolean = {
    hasCompatibleParameters(other.parameters.map(_.typeName), allowPlatformGenericEquivalence)
  }

  override def toString: String =
    modifiers.map(_.toString).mkString(" ") + " constructor(" + parameters
      .map(p => s"${p.typeName.name} ${p.name}")
      .mkString(", ") + ")"
}

object ConstructorDeclaration {
  val emptyConstructorDeclarations: ArraySeq[ConstructorDeclaration] = ArraySeq()
}

trait MethodDeclaration extends DependencyHolder with Dependent with Parameters {
  val name: Name
  val modifiers: ArraySeq[Modifier]

  def typeName: TypeName

  def hasBlock: Boolean

  def signature: String = s"$typeName $nameAndParameterTypes"

  def nameAndParameterTypes: String = s"$name($parameterTypes)"
  def parameterTypes: String        = parameters.map(_.typeName).mkString(", ")

  def isStatic: Boolean      = modifiers.contains(STATIC_MODIFIER)
  def isAbstract: Boolean    = modifiers.contains(ABSTRACT_MODIFIER)
  def isVirtual: Boolean     = modifiers.contains(VIRTUAL_MODIFIER)
  def isOverride: Boolean    = modifiers.contains(OVERRIDE_MODIFIER)
  def isTestVisible: Boolean = modifiers.contains(TEST_VISIBLE_ANNOTATION)

  def visibility: Option[Modifier] =
    modifiers.find(m => ApexModifiers.visibilityModifiers.contains(m))

  def isExternallyVisible: Boolean =
    modifiers.exists(MethodDeclaration.externalMethodModifiers.contains)

  override def toString: String = {
    val modifierStr = if (modifiers.nonEmpty) modifiers.map(_.toString).mkString(" ") + " " else ""
    modifierStr +
      typeName.toString + " " + name.toString + "(" +
      parameters.map(_.toString).mkString(", ") + ")"
  }

  def hasSameSignature(
    other: MethodDeclaration,
    allowPlatformGenericEquivalence: Boolean
  ): Boolean = {
    name == other.name &&
    typeName == other.typeName &&
    hasSameParameters(other, allowPlatformGenericEquivalence)
  }

  /** Test if the passed method has params compatible with this method. Ideally this would just be a
    * comparison of type names but there is a quirk in how platform generic interfaces are handled.
    */
  def hasSameParameters(
    other: MethodDeclaration,
    allowPlatformGenericEquivalence: Boolean
  ): Boolean = {
    hasCompatibleParameters(other.parameters.map(_.typeName), allowPlatformGenericEquivalence)
  }

  /** Test if this method matches the provided params when fulfilling an interface method. The basic
    * approach here is that each param of the interface method must be assignable to the implMethod
    * so that only widening of the types is supported.
    */
  def fulfillsInterfaceMethodParams(
    from: ApexClassDeclaration,
    implMethod: MethodDeclaration
  ): Boolean = {
    if (name != implMethod.name || parameters.length != implMethod.parameters.length)
      return false

    val context    = new TypeVerifyContext(None, from, None, enablePlugins = false)
    val paramTypes = implMethod.parameters.map(_.typeName)
    parameters
      .zip(paramTypes)
      .forall(paramPair => {
        isAssignable(
          paramPair._2,
          paramPair._1.typeName,
          context,
          AssignableOptions(strictConversions = false, narrowSObjects = false)
        )
      })
  }
}

object MethodDeclaration {
  val emptyMethodDeclarations: ArraySeq[MethodDeclaration] = ArraySeq()
  private val externalMethodModifiers: Set[Modifier] =
    Set(
      GLOBAL_MODIFIER,
      WEBSERVICE_MODIFIER,
      AURA_ENABLED_ANNOTATION,
      INVOCABLE_METHOD_ANNOTATION,
      HTTP_DELETE_ANNOTATION,
      HTTP_GET_ANNOTATION,
      HTTP_PATCH_ANNOTATION,
      HTTP_POST_ANNOTATION,
      HTTP_PUT_ANNOTATION
    )
}

/** Method wrapper that enforces an Any return type on the provided method */
class AnyReturnMethodDeclaration(method: MethodDeclaration) extends MethodDeclaration {
  override val name: Name                                 = method.name
  override val modifiers: ArraySeq[Modifier]              = method.modifiers
  override val parameters: ArraySeq[ParameterDeclaration] = method.parameters

  override def typeName: TypeName = TypeName.Any
  override def hasBlock: Boolean  = method.hasBlock
}

trait AbstractTypeDeclaration {
  def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration]

  def findMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    verifyContext: VerifyContext
  ): Either[String, MethodDeclaration]

  def findNestedType(name: Name): Option[AbstractTypeDeclaration]

  def findConstructor(
    params: ArraySeq[TypeName],
    verifyContext: VerifyContext
  ): Either[String, ConstructorDeclaration]

}

trait TypeDeclaration extends AbstractTypeDeclaration with Dependent with PreReValidatable {
  def paths: ArraySeq[PathLike] // Metadata paths that contributed to this type

  def inTest: Boolean = false // Is type defined only for test code

  def isCustomException: Boolean = false

  val moduleDeclaration: Option[
    OPM.Module
  ] // Module that owns this types, None for none-adopted platform types

  val name: Name
  val typeName: TypeName
  val outerTypeName: Option[TypeName]
  val nature: Nature
  val modifiers: ArraySeq[Modifier]

  /** Dead flag, set when discarded from module, useful to aid type cache eviction */
  var dead: Boolean = false

  lazy val namespace: Option[Name] = {
    val outermostType = outerTypeName.getOrElse(typeName).outer
    if (outermostType.forall(_.outer.isEmpty)) outermostType.map(_.name)
    else None
  }

  val superClass: Option[TypeName]
  val interfaces: ArraySeq[TypeName]

  def superClassDeclaration: Option[TypeDeclaration]   = None
  def interfaceDeclarations: ArraySeq[TypeDeclaration] = TypeDeclaration.emptyTypeDeclarations
  def nestedTypes: ArraySeq[TypeDeclaration]

  val blocks: ArraySeq[BlockDeclaration]
  val fields: ArraySeq[FieldDeclaration]

  def methods: ArraySeq[MethodDeclaration]
  def constructors: ArraySeq[ConstructorDeclaration]

  def isComplete: Boolean

  def isExternallyVisible: Boolean =
    modifiers.exists(TypeDeclaration.externalTypeModifiers.contains)
  def visibility: Option[Modifier] = ApexModifiers.visibilityModifiers.find(modifiers.contains)
  def isAbstract: Boolean          = modifiers.contains(ABSTRACT_MODIFIER)
  def isVirtual: Boolean           = modifiers.contains(VIRTUAL_MODIFIER)
  def isExtensible: Boolean =
    nature == INTERFACE_NATURE || (nature == CLASS_NATURE && (isAbstract || isVirtual))

  lazy val isFieldConstructed: Boolean           = isSObject || isApexPagesComponent
  lazy val isSObject: Boolean                    = superClass.contains(TypeNames.SObject)
  private lazy val isApexPagesComponent: Boolean = superClass.contains(TypeNames.ApexPagesComponent)

  def outerTypeDeclaration: Option[TypeDeclaration] =
    outerTypeName.flatMap(typeName => TypeResolver(typeName, this).toOption)

  def outermostTypeDeclaration: TypeDeclaration =
    outerTypeName
      .flatMap(typeName => TypeResolver(typeName, this).toOption)
      .getOrElse(this)

  def safeValidate(): Unit = {
    try {
      validate()
    } catch {
      case ex: Throwable => OrgInfo.logException(ex, paths)
    }
  }

  protected def validate(): Unit

  override def findNestedType(name: Name): Option[TypeDeclaration] = {
    nestedTypes.find(_.name == name)
  }

  override def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    val matches = findField(name)
    staticContext match {
      case Some(x) => matches.find(f => f.isStatic == x)
      case None    => matches
    }
  }

  /* Find a field just from its name. We need to be careful here about recursion between types, e.g. an outer class
   * having a super class which is an inner of the outer class. To handle this we just exclude declarations from
   * being searched a second time. */
  protected def findField(
    name: Name,
    exclude: mutable.HashSet[TypeDeclaration] = new mutable.HashSet()
  ): Option[FieldDeclaration] = {
    exclude.add(this)
    fields
      .find(_.name == name)
      .orElse(superClassDeclaration.filterNot(exclude.contains).flatMap(_.findField(name, exclude)))
      .orElse(
        outerTypeDeclaration
          .filterNot(exclude.contains)
          .flatMap(_.findField(name, exclude).filter(_.isStatic))
      )
  }

  private lazy val methodMap: MethodMap =
    MethodMap(this, None, MethodMap.empty(), methods, ArraySeq())

  private lazy val constructorMap: ConstructorMap =
    ConstructorMap(this, None, constructors, ConstructorMap.empty)

  override def findConstructor(
    params: ArraySeq[TypeName],
    verifyContext: VerifyContext
  ): Either[String, ConstructorDeclaration] = {
    constructorMap.findConstructorByParams(params, verifyContext)
  }

  override def findMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    verifyContext: VerifyContext
  ): Either[String, MethodDeclaration] = {
    val found = methodMap.findMethod(name, params, staticContext, verifyContext)

    // Horrible skulduggery to support SObject.GetSObjectType()
    if (
      found.isLeft && name == Names.GetSObjectType && params.isEmpty && staticContext.contains(true)
    ) {
      findMethod(name, params, Some(false), verifyContext)
    } else {
      found
    }
  }

  def findLocalType(localName: TypeName): Option[TypeDeclaration] = {
    if (moduleDeclaration.isEmpty)
      TypeResolver.platformType(localName.withTail(typeName), this).toOption
    else
      moduleDeclaration.get.getLocalTypeFor(localName, this)
  }

  def validateFieldConstructorArguments(
    input: ExprContext,
    arguments: ArraySeq[Expression],
    context: ExpressionVerifyContext
  ): Unit = {
    assert(isFieldConstructed)

    // FUTURE: Disable this bypass once VF parsing supported
    if (isInstanceOf[Component])
      return

    val validArgs = arguments.flatMap {
      case BinaryExpression(PrimaryExpression(IdPrimary(id)), rhs, "=") =>
        rhs.verify(input, context)
        // Future: check type against field being assigned

        var field: Option[FieldDeclaration] = None

        if (context.module.namespace.nonEmpty) {
          field = findField(context.defaultNamespace(id.name), staticContext = Some(false))
        }

        if (field.isEmpty)
          field = findField(id.name, staticContext = Some(false))

        if (field.isEmpty) {
          if (
            !context.module.isGhostedFieldName(id.name) && !context.module.isGhostedType(typeName)
          )
            context.log(IssueOps.unknownFieldOnSObject(id.location, id.name, typeName))
          None
        } else {
          context.addDependency(field.get)
          Some(id)
        }
      case argument =>
        OrgInfo.logError(
          argument.location,
          s"SObject type '$typeName' construction needs '<field name> = <value>' arguments"
        )
        None
    }

    if (validArgs.length == arguments.length) {
      val duplicates = validArgs.groupBy(_.name).collect { case (_, ArraySeq(_, y, _*)) => y }
      if (duplicates.nonEmpty) {
        OrgInfo.logError(
          duplicates.head.location,
          s"Duplicate assignment to field '${duplicates.head.name}' on SObject type '$typeName'"
        )
      }
    }
  }

  def implements(typeName: TypeName, ignoreGenerics: Boolean): Boolean = {
    val interfaces = interfaceDeclarations
    interfaces.exists(in =>
      (ignoreGenerics && in.typeName.equalsNamesOnly(typeName)) ||
        in.typeName == typeName
    ) ||
    interfaces.exists(_.implements(typeName, ignoreGenerics)) ||
    superClassDeclaration.exists(_.implements(typeName, ignoreGenerics))
  }

  def extendsOrImplements(typeName: TypeName): Boolean = {
    val superclasses = superClassDeclaration
    val interfaces   = interfaceDeclarations
    superclasses.exists(_.typeName == typeName) ||
    interfaces.exists(_.typeName == typeName) ||
    superclasses.exists(_.extendsOrImplements(typeName)) ||
    interfaces.exists(_.extendsOrImplements(typeName))
  }

  def superTypes(): List[TypeName] = {
    lazy val superclasses = superClassDeclaration
    lazy val interfaces   = interfaceDeclarations
    superclasses.map(_.typeName).toList ++
      interfaces.map(_.typeName).toList ++
      superclasses.map(_.superTypes()).getOrElse(Nil) ++
      interfaces.flatMap(_.superTypes())
  }

  /* Collect all interfaces implemented */
  def collectInterfaces(
    found: mutable.Set[TypeDeclaration],
    visited: mutable.Set[TypeDeclaration] = mutable.Set()
  ): Unit = {
    superClassDeclaration
      .filterNot(visited.contains)
      .foreach(td => {
        visited.add(td)
        td.collectInterfaces(found, visited)
      })
    interfaceDeclarations
      .filterNot(visited.contains)
      .foreach(td => {
        visited.add(td)
        if (td.nature == INTERFACE_NATURE) found.add(td)
        td.collectInterfaces(found, visited)
      })
  }

}

object TypeDeclaration {
  val emptyTypeDeclarations: ArraySeq[TypeDeclaration] = ArraySeq()
  private val externalTypeModifiers: Set[Modifier] = Set(GLOBAL_MODIFIER, REST_RESOURCE_ANNOTATION)
}
