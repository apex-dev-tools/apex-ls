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

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.types.apex.{ApexFieldLike, ThisType}
import com.nawforce.apexlink.types.core.{TypeDeclaration, TypeId}
import com.nawforce.apexparser.ApexParser.{PropertyBlockContext, PropertyDeclarationContext}
import com.nawforce.pkgforce.modifiers.{
  ApexModifiers,
  FINAL_MODIFIER,
  Modifier,
  ModifierResults,
  PRIVATE_MODIFIER
}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.parsers.{ApexNode, Nature, PROPERTY_NATURE}
import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.parsers.CodeParser

import scala.collection.immutable.ArraySeq

final case class ApexPropertyDeclaration(
  thisType: ThisType,
  _modifiers: ModifierResults,
  typeName: TypeName,
  id: Id,
  propertyBlocks: Seq[PropertyBlock]
) extends ClassBodyDeclaration(_modifiers)
    with ApexFieldLike {

  override val name: Name                   = id.name
  override val children: ArraySeq[ApexNode] = ArraySeq.empty
  override val nature: Nature               = PROPERTY_NATURE
  override val thisTypeId: TypeId           = thisType.typeId
  override val inTest: Boolean              = thisType.inTest

  // This only returns the location of the type correctly if it is a plain type E.g. String.
  // This Location is WRONG for collections like List, Set and Map.
  // Instead use the location property on TypeName which is populated for these collections
  def typeNameLocation: Location = Location(
    idLocation.startLine,
    idLocation.startPosition - 1 - typeName.name.value.length,
    idLocation.endLine,
    idLocation.startPosition - 1
  )

  override def idLocation: Location = id.location.location

  val setter: Option[SetterPropertyBlock] =
    propertyBlocks.flatMap {
      case x: SetterPropertyBlock => Some(x)
      case _                      => None
    }.headOption

  val getter: Option[GetterPropertyBlock] =
    propertyBlocks.flatMap {
      case x: GetterPropertyBlock => Some(x)
      case _                      => None
    }.headOption

  private val visibility: Option[Modifier] =
    _modifiers.modifiers.find(m => ApexModifiers.visibilityModifiers.contains(m))
  override val readAccess: Modifier =
    getter
      .flatMap(_.modifiers.modifiers.headOption)
      .getOrElse(visibility.getOrElse(PRIVATE_MODIFIER))
  override val writeAccess: Modifier =
    setter
      .flatMap(_.modifiers.modifiers.headOption)
      .getOrElse(visibility.getOrElse(PRIVATE_MODIFIER))

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    val propertyType = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (propertyType.isEmpty)
      context.missingType(id.location, typeName)

    val setters = propertyBlocks.filter(_.isInstanceOf[SetterPropertyBlock])
    propertyType.foreach(td => setters.foreach(_.verify(context, isStatic, td)))
    val getters = propertyBlocks.filter(_.isInstanceOf[GetterPropertyBlock])
    propertyType.foreach(td => getters.foreach(_.verify(context, isStatic, td)))

    if (setters.size > 1 || getters.size > 1 || propertyBlocks.isEmpty) {
      context.logError(
        location,
        "Properties must have either a single 'get' and/or a single 'set' block"
      )
    }

    if (visibility.nonEmpty && writeAccess.order > visibility.get.order) {
      context.logError(location, "Setter visibility must be same or less than property")
    }

    if (visibility.nonEmpty && readAccess.order > visibility.get.order) {
      context.logError(location, "Getter visibility must be same or less than property")
    }

    setDepends(context.dependencies)
  }
}

object ApexPropertyDeclaration {
  def construct(
    parser: CodeParser,
    thisType: ThisType,
    modifiers: ModifierResults,
    propertyDeclaration: PropertyDeclarationContext
  ): ApexPropertyDeclaration = {
    val typeName = TypeReference.construct(propertyDeclaration.typeRef())
    ApexPropertyDeclaration(
      thisType,
      modifiers,
      typeName,
      Id.construct(propertyDeclaration.id()),
      CodeParser
        .toScala(propertyDeclaration.propertyBlock())
        .flatMap(pb => PropertyBlock.construct(parser, pb, typeName))
    ).withContext(propertyDeclaration)
  }
}

sealed abstract class PropertyBlock extends CST {
  def verify(
    context: BodyDeclarationVerifyContext,
    isStatic: Boolean,
    propertyType: TypeDeclaration
  ): Unit
}

final case class GetterPropertyBlock(modifiers: ModifierResults, block: Option[Block])
    extends PropertyBlock {
  override def verify(
    context: BodyDeclarationVerifyContext,
    isStatic: Boolean,
    propertyType: TypeDeclaration
  ): Unit = {
    block.foreach(block => {
      val blockContext = new OuterBlockVerifyContext(context, isStatic, propertyType.typeName)
      block.verify(blockContext)
      blockContext.logControlFlowIssues()
      context.typePlugin.foreach(_.onBlockValidated(block, isStatic, blockContext))
    })
  }
}

final case class SetterPropertyBlock(
  modifiers: ModifierResults,
  typeName: TypeName,
  block: Option[Block]
) extends PropertyBlock {
  override def verify(
    context: BodyDeclarationVerifyContext,
    isStatic: Boolean,
    propertyType: TypeDeclaration
  ): Unit = {
    block.foreach(block => {
      val blockContext = new OuterBlockVerifyContext(context, isStatic)
      blockContext.addVar(
        Name("value"),
        None,
        modifiers.modifiers.contains(FINAL_MODIFIER),
        propertyType
      )
      block.verify(blockContext)
      context.typePlugin.foreach(_.onBlockValidated(block, isStatic, blockContext))
    })
  }
}

object PropertyBlock {
  def construct(
    parser: CodeParser,
    propertyBlockContext: PropertyBlockContext,
    typeName: TypeName
  ): Option[PropertyBlock] = {
    val modifiers: ModifierResults = ApexModifiers.propertyBlockModifiers(
      parser,
      CodeParser.toScala(propertyBlockContext.modifier()),
      propertyBlockContext
    )
    val cst = {
      val getter = CodeParser.toScala(propertyBlockContext.getter())
      val setter = CodeParser.toScala(propertyBlockContext.setter())

      if (getter.nonEmpty) {
        Some(
          GetterPropertyBlock(
            modifiers,
            CodeParser
              .toScala(getter.get.block())
              .map(bc => Block.constructOuterFromANTLR(parser, bc))
          )
        )
      } else if (setter.nonEmpty) {
        Some(
          SetterPropertyBlock(
            modifiers,
            typeName,
            CodeParser
              .toScala(setter.get.block())
              .map(bc => Block.constructOuterFromANTLR(parser, bc))
          )
        )
      } else {
        None
      }
    }
    cst.map(_.withContext(propertyBlockContext))
  }
}
