/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.common.types

import com.nawforce.common.api._
import com.nawforce.common.cst._
import com.nawforce.common.diagnostics.Issue
import com.nawforce.common.finding.TypeRequest
import com.nawforce.common.metadata.{DependencyHolder, MetadataDeclaration}
import com.nawforce.common.names.{Name, TypeName}
import com.nawforce.common.org.{OrgImpl, PackageImpl}
import com.nawforce.common.types.other.CustomComponent
import com.nawforce.common.types.platform.PlatformTypes
import com.nawforce.runtime.types._

import scala.collection.mutable

sealed abstract class Nature(val value: String)
case object CLASS_NATURE extends Nature("class")
case object INTERFACE_NATURE extends Nature("interface")
case object ENUM_NATURE extends Nature("enum")

object Nature {
  def apply(value: String): Nature = {
    value match {
      case CLASS_NATURE.value => CLASS_NATURE
      case INTERFACE_NATURE.value => INTERFACE_NATURE
      case ENUM_NATURE.value => ENUM_NATURE
    }
  }
}

trait BlockDeclaration extends DependencyHolder {
  val isStatic: Boolean

  def summary: BlockSummary = {
    BlockSummary(isStatic, dependencySummary())
  }
}

trait FieldDeclaration extends DependencyHolder {
  val name: Name
  val modifiers: Seq[Modifier]
  val typeName: TypeName
  val readAccess: Modifier
  val writeAccess: Modifier

  lazy val isStatic: Boolean = modifiers.contains(STATIC_MODIFIER)

  def summary: FieldSummary = {
    summary(None)
  }

  protected def summary(range: Option[RangeLocation]): FieldSummary = {
    FieldSummary(range, name.toString,
      modifiers.map(_.toString).sorted.toList,
      typeName,
      readAccess.toString, writeAccess.toString,
      dependencySummary()
    )
  }
}

trait ParameterDeclaration {
  val name: Name
  val typeName: TypeName

  def summary: ParameterSummary = {
    ParameterSummary(name.toString, typeName)
  }
}

trait ConstructorDeclaration extends DependencyHolder {
  val modifiers: Seq[Modifier]
  val parameters: Seq[ParameterDeclaration]

  def summary: ConstructorSummary = {
    summary(None)
  }

  protected def summary(range: Option[RangeLocation]): ConstructorSummary = {
    ConstructorSummary(range,
      modifiers.map(_.toString).sorted.toList,
      parameters.map(_.summary).sortBy(_.name).toList,
      dependencySummary()
    )
  }
}

trait MethodDeclaration extends DependencyHolder {
  val name: Name
  val modifiers: Seq[Modifier]
  val typeName: TypeName
  val parameters: Seq[ParameterDeclaration]

  lazy val signature: String = s"$typeName $name($parameterTypes)"
  lazy val parameterTypes: String = parameters.map(_.typeName).mkString(", ")

  lazy val isStatic: Boolean = modifiers.contains(STATIC_MODIFIER)
  lazy val isAbstract: Boolean = modifiers.contains(ABSTRACT_MODIFIER)
  lazy val isVirtual: Boolean = modifiers.contains(VIRTUAL_MODIFIER)
  lazy val isVirtualOrOverride: Boolean = isVirtual || modifiers.contains(OVERRIDE_MODIFIER)
  lazy val isGlobalOrPublic: Boolean = modifiers.exists(m => m == GLOBAL_MODIFIER || m == PUBLIC_MODIFIER)

  def hasSameSignature(other: MethodDeclaration): Boolean = {
    name == other.name &&
    typeName == other.typeName &&
    hasSameParameters(other)
  }

  def hasSameParameters(other: MethodDeclaration): Boolean = {
    hasParameters(other.parameters.map(_.typeName))
  }

  def hasParameters(params: Seq[TypeName]): Boolean = {
    if (parameters.size == params.size) {
      parameters.zip(params).forall(z => z._1.typeName == z._2)
    } else {
      false
    }
  }

  def hasSameErasedParameters(pkg: Option[PackageImpl], other: MethodDeclaration): Boolean = {
    hasErasedParameters(pkg, other.parameters.map(_.typeName))
  }

  def hasErasedParameters(pkg: Option[PackageImpl], params: Seq[TypeName]): Boolean = {
    if (parameters.size == params.size) {
      parameters.zip(params).forall(z =>
        (z._1.typeName == z._2) ||
          (z._1.typeName.isStringOrId && z._2.isStringOrId) ||
          (z._2.isSObjectList && z._1.typeName.isList &&
            (TypeRequest(z._1.typeName.params.head, None, pkg, excludeSObjects = false) match {
              case Right(td) => td.isSObject
              case Left(_) => false
            }))
      )
    } else {
      false
    }
  }

  def hasCallErasedParameters(pkg: PackageImpl, params: Seq[TypeName]): Boolean = {
    if (parameters.size == params.size) {
      parameters.zip(params).forall(z =>
        z._1.typeName == z._2 ||
          (z._1.typeName.equalsIgnoreParams(z._2) &&
            (TypeRequest(z._1.typeName, None, Some(pkg), excludeSObjects = false) match {
              case Right(x: PlatformTypeDeclaration) if x.nature == INTERFACE_NATURE =>
                TypeRequest(z._2, None, Some(pkg), excludeSObjects = false) match {
                  case Right(y: PlatformTypeDeclaration) if y.nature == INTERFACE_NATURE => true
                  case _ => false
                }
              case _ => false
            })
            ))
    } else {
      false
    }
  }

  def summary: MethodSummary = {
    summary(None)
  }

  protected def summary(range: Option[RangeLocation]): MethodSummary = {
    MethodSummary(range,
      name.toString, modifiers.map(_.toString).sorted.toList,
      typeName,
      parameters.map(_.summary).toList, dependencySummary()
    )
  }
}

object TypeDeclaration {
  type TID = Int
  private var tidCounter: TID = 0

  def getAndIncrementTID(): TID = {
    tidCounter += 1
    tidCounter
  }
}

trait TypeDeclaration extends MetadataDeclaration {
  val tid: TypeDeclaration.TID = TypeDeclaration.getAndIncrementTID()
  override lazy val internalName: Name = Name(typeName.toString)
  val isSearchable: Boolean = true

  val packageDeclaration: Option[PackageImpl]
  val name: Name
  val typeName: TypeName
  val outerTypeName: Option[TypeName]
  val nature: Nature
  val modifiers: Seq[Modifier]

  lazy val namespace: Option[Name] = {
    val outermostType = outerTypeName.getOrElse(typeName).outer
    assert(outermostType.forall(_.outer.isEmpty))
    outermostType.map(_.name)
  }

  val superClass: Option[TypeName]
  lazy val superClassDeclaration: Option[TypeDeclaration] = None
  val interfaces: Seq[TypeName]
  lazy val interfaceDeclarations: Seq[TypeDeclaration] = Nil
  def nestedTypes: Seq[TypeDeclaration]

  val blocks: Seq[BlockDeclaration]
  val fields: Seq[FieldDeclaration]
  val constructors: Seq[ConstructorDeclaration]
  val methods: Seq[MethodDeclaration]

  def isComplete: Boolean
  val isExternallyVisible: Boolean
  val isAny: Boolean = false
  lazy val isAbstract: Boolean = modifiers.contains(ABSTRACT_MODIFIER)
  lazy val isFieldConstructed: Boolean = isSObject || isApexPagesComponent
  lazy val isSObject: Boolean = superClass.contains(TypeName.SObject)
  lazy val isApexPagesComponent: Boolean = superClass.contains(TypeName.ApexPagesComponent)

  def validate(): Unit

  def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    val matches = fieldsByName.get(name)
    staticContext match {
      case Some(x) => matches.find(f => f.isStatic == x)
      case None => matches
    }
  }

  protected def findFieldSObject(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    val fieldOption = fieldsByName.get(name)
    if (fieldOption.isEmpty) {
      if (name == Name.SObjectField)
        return Some(CustomFieldDeclaration(Name.SObjectField, TypeName.sObjectFields$(typeName)))
      else
        return None
    }

    val field = fieldOption.get
    if (staticContext.contains(field.isStatic)) {
      fieldOption
    } else if (staticContext.contains(true)) {
      if (CustomFieldDeclaration.isSObjectPrimitive(field.typeName)) {
        // TODO: Identify Share
        if (name == Name.RowCause)
          Some(CustomFieldDeclaration(field.name, TypeName.SObjectFieldRowCause$, asStatic = true))
        else
          Some(CustomFieldDeclaration(field.name, TypeName.SObjectField, asStatic = true))
      } else {
        // Make sure SObject is loaded so fields can be found
        PlatformTypes.get(field.typeName, None)
        Some(CustomFieldDeclaration(field.name, TypeName.sObjectFields$(field.typeName), asStatic = true))
      }
    } else {
      None
    }
  }

  private lazy val fieldsByName: mutable.Map[Name, FieldDeclaration] = {
    val outerType = outerTypeName.flatMap(typeName => TypeRequest(typeName, this, excludeSObjects = false).toOption)
    val fieldsByName = mutable.Map(fields.map(f => (f.name, f)) : _*)
    outerType.foreach(td => td.fields.filter(_.isStatic).foreach(f => {
      if (!fieldsByName.contains(f.name))
        fieldsByName.put(f.name, f)
    }))
    fieldsByName
  }

  private lazy val methodMap: MethodMap = MethodMap(this, None, MethodMap.empty(), methods, Seq())

  def findMethod(name: Name, params: Seq[TypeName], staticContext: Option[Boolean],
                 verifyContext: VerifyContext): Seq[MethodDeclaration] = {
    val found = methodMap.findMethod(name, params, staticContext, verifyContext)

    // Horrible skulduggery to support SObject.GetSObjectType()
    if (found.isEmpty && name == Name.GetSObjectType && params.isEmpty && staticContext.contains(true)) {
      findMethod(name, params, Some(false), verifyContext)
    } else {
      found
    }
  }

  def findLocalType(typeName: TypeName): Option[TypeDeclaration] = {
    packageDeclaration.flatMap(pkg => pkg.getLocalTypeFor(typeName, this))
  }

  def validateFieldConstructorArguments(input: ExprContext, arguments: Seq[Expression], context: ExpressionVerifyContext): Unit = {
    assert(isFieldConstructed)

    // FUTURE: Disable this bypass once VF parsing supported
    if (isInstanceOf[CustomComponent])
      return

    val validArgs = arguments.flatMap(argument => {
      argument match {
        case BinaryExpression(PrimaryExpression(IdPrimary(id)), rhs, "=") =>
          rhs.verify(input, context)
          // Future: check type against field being assigned

          var field : Option[FieldDeclaration] = None

          if (context.pkg.namespace.nonEmpty) {
            field = findField(context.defaultNamespace(id.name), staticContext = Some(false))
          }

          if (field.isEmpty)
            field = findField(id.name, staticContext = Some(false))

          if (field.isEmpty) {
            if (isComplete)
              context.log(Issue.unknownFieldOnSObject(id.location, id.name, typeName))
            None
          } else {
            context.addDependency(field.get)
            Some(id)
          }
        case _ =>
          OrgImpl.logError(argument.location, s"SObject type '$typeName' construction needs '<field name> = <value>' arguments")
          None
      }
    })

    if (validArgs.size == arguments.size) {
      val duplicates = validArgs.groupBy(_.name).collect { case (_, Seq(_, y, _*)) => y }
      if (duplicates.nonEmpty) {
        OrgImpl.logError(duplicates.head.location,
          s"Duplicate assignment to field '${duplicates.head.name}' on SObject type '$typeName'")
      }
    }
  }

  def extendsOrImplements(typeName: TypeName): Boolean = {
    superClassDeclaration.exists(_.typeName == typeName) ||
      interfaceDeclarations.exists(_.typeName == typeName) ||
      superClassDeclaration.exists(_.extendsOrImplements(typeName)) ||
      interfaceDeclarations.exists(_.extendsOrImplements(typeName))
  }

  def superTypes(): List[TypeName] = {
    superClassDeclaration.map(_.typeName).toList ++
      interfaceDeclarations.map(_.typeName).toList ++
      superClassDeclaration.map(_.superTypes()).getOrElse(Nil) ++
      interfaceDeclarations.flatMap(_.superTypes())
  }

  // Obtain a summary representation of the type declaration for caching
  def summary: TypeSummary = {
    TypeSummary (
      0,
      None,
      name.toString,
      typeName,
      nature.value, modifiers.map(_.toString).sorted.toList,
      superClass,
      interfaces.toList,
      blocks.map(_.summary).toList,
      fields.map(_.summary).sortBy(_.name).toList,
      constructors.map(_.summary).sortBy(_.parameters.size).toList,
      methods.map(_.summary).sortBy(_.name).toList,
      nestedTypes.map(_.summary).sortBy(_.name).toList,
      dependencySummary(),
      Set.empty
    )
  }
}
