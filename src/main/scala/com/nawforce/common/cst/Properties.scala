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
package com.nawforce.common.cst

import com.nawforce.common.api.{Name, TypeName}
import com.nawforce.common.documents.RangeLocationImpl
import com.nawforce.common.modifiers.{ApexModifiers, Modifier, ModifierResults, PRIVATE_MODIFIER}
import com.nawforce.common.types.apex.ApexFieldLike
import com.nawforce.common.types.core.TypeId
import com.nawforce.runtime.parsers.ApexParser.{PropertyBlockContext, PropertyDeclarationContext}
import com.nawforce.runtime.parsers.CodeParser

final case class ApexPropertyDeclaration(outerTypeId: TypeId, _modifiers: ModifierResults, typeName: TypeName, id: Id,
                                         propertyBlocks: Seq[PropertyBlock])
  extends ClassBodyDeclaration(_modifiers) with ApexFieldLike {

  override val name: Name = id.name
  override val nameRange: RangeLocationImpl = id.location

  val setter: Option[SetterPropertyBlock] =
    propertyBlocks.flatMap {
      case x: SetterPropertyBlock => Some(x)
      case _ => None
    }.headOption

  val getter: Option[GetterPropertyBlock] =
    propertyBlocks.flatMap {
      case x: GetterPropertyBlock => Some(x)
      case _ => None
    }.headOption

  private val visibility: Option[Modifier] =
    _modifiers.modifiers.find(m => ApexModifiers.visibilityModifiers.contains(m))
  override val readAccess: Modifier =
    getter.flatMap(_.modifiers.modifiers.headOption).getOrElse(visibility.getOrElse(PRIVATE_MODIFIER))
  override val writeAccess: Modifier =
    setter.flatMap(_.modifiers.modifiers.headOption).getOrElse(visibility.getOrElse(PRIVATE_MODIFIER))

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    val propType = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (propType.isEmpty)
      context.missingType(id.location, typeName)

    val setters = propertyBlocks.filter(_.isInstanceOf[SetterPropertyBlock])
    setters.foreach(_.verify(context, isStatic))
    val getters = propertyBlocks.filter(_.isInstanceOf[GetterPropertyBlock])
    getters.foreach(_.verify(context, isStatic))

    if (setters.size > 1 || getters.size > 1 || propertyBlocks.isEmpty) {
      context.logError(location, "Properties must have either a single 'get' and/or a single 'set' block")
    }

    if (visibility.nonEmpty && writeAccess.order > visibility.get.order) {
      context.logError(location, "Setter visibility must be same or less than property")
    }

    if (visibility.nonEmpty && readAccess.order > visibility.get.order) {
      context.logError(location, "Getter visibility must be same or less than property")
    }

    setDepends(context.dependencies)
    context.propagateDependencies()
  }
}

object ApexPropertyDeclaration {
  def construct(parser: CodeParser, outerTypeId: TypeId, modifiers: ModifierResults,
                propertyDeclaration: PropertyDeclarationContext) : ApexPropertyDeclaration = {
    val typeName = TypeReference.construct(propertyDeclaration.typeRef())
    ApexPropertyDeclaration(outerTypeId, modifiers, typeName,
      Id.construct(propertyDeclaration.id()),
      CodeParser.toScala(propertyDeclaration.propertyBlock())
        .map(pb => PropertyBlock.construct(parser, pb, typeName)),
    ).withContext(propertyDeclaration)
  }
}

sealed abstract class PropertyBlock extends CST {
  def verify(context: BodyDeclarationVerifyContext, isStatic: Boolean): Unit
}

final case class GetterPropertyBlock(modifiers: ModifierResults, block: Option[Block]) extends PropertyBlock {
  override def verify(context: BodyDeclarationVerifyContext, isStatic: Boolean): Unit = {
    block.foreach(_.verify(new OuterBlockVerifyContext(context, isStatic)))
  }
}

final case class SetterPropertyBlock(modifiers: ModifierResults, typeName: TypeName, block: Option[Block]) extends PropertyBlock {
  override def verify(context: BodyDeclarationVerifyContext, isStatic: Boolean): Unit = {
    val bc = new OuterBlockVerifyContext(context, isStatic)
    bc.addVar(Name("value"), location, typeName)
    block.foreach(_.verify(bc))
  }
}

object PropertyBlock {
  def construct(parser: CodeParser, propertyBlockContext: PropertyBlockContext, typeName: TypeName): PropertyBlock = {
    val modifiers: ModifierResults = ApexModifiers.propertyBlockModifiers(parser,
      CodeParser.toScala(propertyBlockContext.modifier()), propertyBlockContext)
    val cst = {
      val getter = CodeParser.toScala(propertyBlockContext.getter())
      val setter = CodeParser.toScala(propertyBlockContext.setter())

      if (getter.nonEmpty) {
        GetterPropertyBlock(modifiers,
          Block.constructOption(parser, CodeParser.toScala(getter.get.block())))
      } else if (setter.nonEmpty) {
        SetterPropertyBlock(modifiers, typeName,
          Block.constructOption(parser, CodeParser.toScala(setter.get.block())))
      } else {
        throw new CSTException()
      }
    }
    cst.withContext(propertyBlockContext)
  }
}
