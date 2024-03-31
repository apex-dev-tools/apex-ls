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

import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames._
import com.nawforce.pkgforce.names.{EncodedName, Name, Names, TypeName}
import com.nawforce.runtime.parsers.CodeParser
import io.github.apexdevtools.apexparser.ApexParser.{
  TypeArgumentsContext,
  TypeListContext,
  TypeNameContext,
  TypeRefContext
}

import scala.collection.immutable.ArraySeq

trait CSTTypeName {
  def typeArguments(): CSTTypeArguments
  def isList: Boolean
  def isSet: Boolean
  def isMap: Boolean
  def getIdText: Option[String]
}

trait CSTTypeReference {
  def arraySubscriptsCount(): Int
  def typeNames(): ArraySeq[CSTTypeName]
}

object CSTTypeReference {
  final val emptyArraySeq = ArraySeq[CSTTypeReference]()
}

trait CSTTypeArguments {
  def typeRefs(): ArraySeq[CSTTypeReference]
}

private[cst] object ANTLRCST {
  private class ANTLRTypeArguments(typeArgumentsContext: Option[TypeArgumentsContext])
      extends CSTTypeArguments {
    override def typeRefs(): ArraySeq[CSTTypeReference] = {
      if (typeArgumentsContext.isEmpty)
        CSTTypeReference.emptyArraySeq
      else
        CodeParser
          .toScala(typeArgumentsContext.get.typeList().typeRef())
          .map(new ANTLRTypeReference(_))
    }
  }

  private class ANTLRTypeName(typeName: TypeNameContext) extends CSTTypeName {
    override def typeArguments(): CSTTypeArguments =
      new ANTLRTypeArguments(CodeParser.toScala(typeName.typeArguments()))
    override def isList: Boolean           = CodeParser.toScala(typeName.LIST()).nonEmpty
    override def isSet: Boolean            = CodeParser.toScala(typeName.SET()).nonEmpty
    override def isMap: Boolean            = CodeParser.toScala(typeName.MAP()).nonEmpty
    override def getIdText: Option[String] = Option(typeName.id()).map(id => CodeParser.getText(id))
  }

  private[cst] class ANTLRTypeReference(typeRef: TypeRefContext) extends CSTTypeReference {
    override def arraySubscriptsCount(): Int =
      CodeParser.getText(typeRef.arraySubscripts()).count(_ == '[')
    override def typeNames(): ArraySeq[CSTTypeName] =
      CodeParser.toScala(typeRef.typeName()).map(new ANTLRTypeName(_))
  }
}

object TypeReference {

  def construct(typeRefs: List[TypeRefContext]): List[TypeName] = {
    typeRefs.map(x => TypeReference.construct(x))
  }

  def construct(typeRef: TypeRefContext): TypeName = {
    construct(CodeParser.toScala(typeRef).map(new ANTLRCST.ANTLRTypeReference(_)))
  }

  def construct(typeRefOpt: Option[CSTTypeReference]): TypeName = {
    typeRefOpt
      .map { typeRef =>
        {
          val arraySubs                    = typeRef.arraySubscriptsCount()
          val names: ArraySeq[CSTTypeName] = typeRef.typeNames()

          // Only decode head as rest can't legally be in EncodedName format
          createTypeName(decodeName(names.head), names.tail).withArraySubscripts(arraySubs)
        }
      }
      .getOrElse(TypeNames.Void)
  }

  private def getName(name: CSTTypeName): Name = {
    if (name.isList) Names.ListName
    else if (name.isSet) Names.SetName
    else if (name.isMap) Names.MapName
    else name.getIdText.map(Names(_)).getOrElse(Names.Empty)
  }

  private def decodeName(name: CSTTypeName): TypeName = {
    val params   = createTypeParams(name.typeArguments())
    val typeName = getName(name)
    val encType  = EncodedName(typeName)
    if (encType.ext.nonEmpty)
      TypeName(encType.fullName, params, Some(TypeNames.Schema)).intern
    else
      TypeName(typeName, params, None).intern
  }

  @scala.annotation.tailrec
  private def createTypeName(outer: TypeName, names: Seq[CSTTypeName]): TypeName = {
    names match {
      case Nil => outer
      case hd +: tl =>
        createTypeName(
          TypeName(getName(hd), createTypeParams(hd.typeArguments()), Some(outer)).intern,
          tl
        )
    }
  }

  private def createTypeParams(typeArguments: CSTTypeArguments): Seq[TypeName] = {
    if (typeArguments.typeRefs().isEmpty)
      TypeName.emptySeq
    else
      typeArguments.typeRefs().map(param => TypeReference.construct(Option(param)))
  }
}

object TypeList {
  def construct(typeList: TypeListContext): ArraySeq[TypeName] = {
    val types = CodeParser.toScala(typeList.typeRef())
    types.map(t => TypeReference.construct(t))
  }
}
