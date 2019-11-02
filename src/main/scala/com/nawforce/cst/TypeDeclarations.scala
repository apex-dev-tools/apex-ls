/*
 [The "BSD licence"]
 Copyright (c) 2017 Kevin Jones
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
package com.nawforce.cst

import java.nio.file.Path

import com.nawforce.names.{Name, TypeName}
import com.nawforce.parsers.ApexParser
import com.nawforce.parsers.ApexParser._
import com.nawforce.types._

import scala.collection.JavaConverters._

final case class CompilationUnit(path: Path, private val _typeDeclaration: ApexTypeDeclaration) extends CST {
  def children(): List[CST] = List(_typeDeclaration)

  def typeDeclaration(): ApexTypeDeclaration = {
    _typeDeclaration
  }
}

object CompilationUnit {
  def construct(pkg: PackageDeclaration, path: Path, compilationUnit: CompilationUnitContext,
                context: ConstructContext): CompilationUnit = {
    CompilationUnit(path,
      ApexTypeDeclaration.construct(pkg, None, compilationUnit.typeDeclaration(), context))
      .withContext(compilationUnit, context)
  }
}

final case class ClassDeclaration(_pkg: PackageDeclaration, _outerTypeName: Option[TypeName], _id: Id,
                                  _modifiers: Seq[Modifier], _extendsType: Option[TypeName], _implementsTypes: Seq[TypeName],
                                  _bodyDeclarations: Seq[ClassBodyDeclaration]) extends
  ApexTypeDeclaration(_pkg, _outerTypeName, _id, _modifiers, _extendsType, _implementsTypes, _bodyDeclarations) {

  override val nature: Nature = CLASS_NATURE

  override def verify(context: TypeVerifyContext): Unit = {
    if (bodyDeclarations.exists(_.isGlobal) && !modifiers.contains(GLOBAL_MODIFIER)) {
      context.logMessage(id.location, "Classes enclosing globals or webservices must also be declared global")
    }
    super.verify(context)
  }

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    if (bodyDeclarations.exists(_.isGlobal) && !modifiers.contains(GLOBAL_MODIFIER)) {
      context.logMessage(id.location, "Classes enclosing globals or webservices must also be declared global")
    }
    super.verify(new TypeVerifyContext(Some(context), this))
  }
}

object ClassDeclaration {
  def construct(pkg: PackageDeclaration, outerTypeName: Option[TypeName], modifiers: Seq[Modifier],
                classDeclaration: ClassDeclarationContext, context: ConstructContext): ClassDeclaration = {

    val thisType = TypeName(Name(classDeclaration.id().getText), Nil,
      outerTypeName.orElse(pkg.namespace.map(TypeName(_))))
    val extendType =
      if (classDeclaration.typeRef() != null)
        Some(TypeRef.construct(classDeclaration.typeRef()))
      else
        None
    val implementsType =
      if (classDeclaration.typeList() != null)
        TypeList.construct(classDeclaration.typeList())
      else
        Seq()

    val classBody = classDeclaration.classBody()
    val classBodyDeclarations: Seq[ClassBodyDeclarationContext] = classBody.classBodyDeclaration().asScala
    val bodyDeclarations: List[ClassBodyDeclaration] =
      if (classBodyDeclarations != null) {
        classBodyDeclarations.toList.flatMap(cbd =>

          if (cbd.block() != null) {
            Seq(InitialiserBlock.construct(if (cbd.STATIC()==null) Seq() else Seq(STATIC_MODIFIER), cbd.block(), context))
          } else if (cbd.memberDeclaration() != null) {
            val modifiers: Seq[ModifierContext] = cbd.modifier().asScala
            ClassBodyDeclaration.construct(pkg, thisType, modifiers.toList, cbd.memberDeclaration(), context)
          } else {
            throw new CSTException()
          }
        )
      } else {
        List()
      }

    ClassDeclaration(pkg, outerTypeName, Id.construct(classDeclaration.id(), context), modifiers, extendType,
      implementsType, bodyDeclarations).withContext(classDeclaration, context)
  }
}

final case class InterfaceDeclaration(_pkg: PackageDeclaration, _outerTypeName: Option[TypeName], _id: Id,
                                      _modifiers: Seq[Modifier], _implementsTypes: Seq[TypeName],
                                      _bodyDeclarations: Seq[ClassBodyDeclaration])
  extends ApexTypeDeclaration(_pkg, _outerTypeName, _id, _modifiers, None, _implementsTypes, _bodyDeclarations) {

  override val nature: Nature = INTERFACE_NATURE

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    super.verify(new TypeVerifyContext(Some(context), this))
  }
}

object InterfaceDeclaration {
  def construct(pkg: PackageDeclaration, outerTypeName: Option[TypeName], modifiers: Seq[Modifier],
                interfaceDeclaration: ApexParser.InterfaceDeclarationContext, context: ConstructContext)
  : InterfaceDeclaration = {
    val thisType = TypeName(Name(interfaceDeclaration.id().getText), Nil,
      outerTypeName.orElse(pkg.namespace.map(TypeName(_))))

    val implementsType =
      if (interfaceDeclaration.typeList() != null)
        TypeList.construct(interfaceDeclaration.typeList())
      else
        Seq()

    val methods: Seq[ApexMethodDeclaration]
        = interfaceDeclaration.interfaceBody().interfaceMethodDeclaration().asScala.map(m =>
            ApexMethodDeclaration.construct(pkg, thisType,
              ApexModifiers.construct(m.modifier().asScala, context), m, context)
    )

    InterfaceDeclaration(pkg, outerTypeName, Id.construct(interfaceDeclaration.id(), context), modifiers,
      implementsType, methods).withContext(interfaceDeclaration, context)
  }
}

final case class EnumDeclaration(_pkg: PackageDeclaration, _outerTypeName: Option[TypeName],_id: Id,
                                 _modifiers: Seq[Modifier], _bodyDeclarations: Seq[ClassBodyDeclaration])
  extends ApexTypeDeclaration(_pkg, _outerTypeName, _id, _modifiers, None, Seq(), _bodyDeclarations) {

  override val nature: Nature = ENUM_NATURE

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    super.verify(new TypeVerifyContext(Some(context), this))
  }
}

object EnumDeclaration {
  def construct(pkg: PackageDeclaration, outerTypeName: Option[TypeName], typeModifiers: Seq[Modifier],
                enumDeclaration: ApexParser.EnumDeclarationContext, context: ConstructContext): EnumDeclaration = {

    // FUTURE: Add standard enum methods

    val id = Id.construct(enumDeclaration.id(), context)
    val thisType = TypeName(id.name, Nil,
      outerTypeName.orElse(pkg.namespace.map(TypeName(_)))
    )
    val constants = Option(enumDeclaration.enumConstants()).map(_.id().asScala).getOrElse(Seq())
    val fields = constants.map(constant => {
      ApexFieldDeclaration(Seq(PUBLIC_MODIFIER, STATIC_MODIFIER), thisType,
        VariableDeclarator(
          thisType,
          Id.construct(constant, context),
          None
        ).withContext(constant, context)
      )
    })

    EnumDeclaration(pkg, outerTypeName,id, typeModifiers, fields).withContext(enumDeclaration, context)
  }
}
