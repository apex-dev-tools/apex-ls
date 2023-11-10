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
import com.nawforce.apexparser.ApexParser.{
  TypeArgumentsContext,
  TypeListContext,
  TypeNameContext,
  TypeRefContext
}
import com.nawforce.pkgforce.names.{EncodedName, Name, Names, TypeName}
import com.nawforce.runtime.parsers.CodeParser

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

  private[cst] class ANTLRTypeName(typeName: TypeNameContext) extends CSTTypeName {
    override def typeArguments(): CSTTypeArguments =
      new ANTLRTypeArguments(CodeParser.toScala(typeName.typeArguments()))
    override def isList: Boolean           = CodeParser.toScala(typeName.LIST()).nonEmpty
    override def isSet: Boolean            = CodeParser.toScala(typeName.SET()).nonEmpty
    override def isMap: Boolean            = CodeParser.toScala(typeName.MAP()).nonEmpty
    override def getIdText: Option[String] = Option(typeName.id()).map(id => CodeParser.getText(id))
    def context: TypeNameContext           = typeName
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

  def construct(typeRef: TypeRefContext, intern: Boolean = true): TypeName = {
    construct(CodeParser.toScala(typeRef).map(new ANTLRCST.ANTLRTypeReference(_)), intern)
  }

  // Interning is disabled for creating collection types. TypeNames that are a type parameter store location information
  // so need to be separate objects to store unique locations.
  def construct(typeRefOpt: Option[CSTTypeReference], intern: Boolean): TypeName = {
    typeRefOpt
      .map { typeRef =>
        {
          val arraySubs                    = typeRef.arraySubscriptsCount()
          val names: ArraySeq[CSTTypeName] = typeRef.typeNames()

          // Only decode head as rest can't legally be in EncodedName format
          val typeName =
            createTypeName(decodeName(names.head, intern), names.tail, intern && arraySubs == 0)

          if (arraySubs > 0) {
            addContext(
              Some(typeName),
              names.collect { case tn: ANTLRCST.ANTLRTypeName =>
                tn.context
              }
            )
          }
          typeName.withArraySubscripts(arraySubs)
        }
      }
      .getOrElse(TypeNames.Void)
  }

  def constructFromTypeList(typeList: TypeListContext): ArraySeq[TypeName] = {
    val types = CodeParser.toScala(typeList.typeRef())
    types.map(t => {
      val typeRefOpt = CodeParser.toScala(t).map(new ANTLRCST.ANTLRTypeReference(_))
      typeRefOpt
        .map { typeRef =>
          val names: ArraySeq[CSTTypeName] = typeRef.typeNames()
          val typeName =
            createTypeName(decodeName(names.head), names.tail, intern = false)
          addContext(
            Some(typeName),
            names.collect { case tn: ANTLRCST.ANTLRTypeName =>
              tn.context
            }
          )
          typeName
        }
        .getOrElse(TypeNames.Void)
    })
  }

  private def getName(name: CSTTypeName): Name = {
    if (name.isList) Names.ListName
    else if (name.isSet) Names.SetName
    else if (name.isMap) Names.MapName
    else name.getIdText.map(Names(_)).getOrElse(Names.Empty)
  }

  private def decodeName(name: CSTTypeName, intern: Boolean = false): TypeName = {
    val params   = createTypeParams(name.typeArguments())
    val typeName = getName(name)
    val encType  = EncodedName(typeName)
    if (encType.ext.nonEmpty) {
      val tn = TypeName(encType.fullName, params, Some(TypeNames.Schema))
      if (intern) tn.intern
      tn
    } else {
      val tn = TypeName(typeName, params, None)
      if (intern) tn.intern
      tn
    }
  }

  @scala.annotation.tailrec
  private def createTypeName(
    outer: TypeName,
    names: Seq[CSTTypeName],
    intern: Boolean = true
  ): TypeName = {
    names match {
      case Nil => outer
      case hd +: tl =>
        val tn = TypeName(getName(hd), createTypeParams(hd.typeArguments()), Some(outer))
        if (intern) { tn.intern }
        createTypeName(tn, tl)
    }
  }

  private def createTypeParams(typeArguments: CSTTypeArguments): Seq[TypeName] = {
    if (typeArguments.typeRefs().isEmpty)
      TypeName.emptySeq
    else
      typeArguments
        .typeRefs()
        .map(param => {
          val paramType = TypeReference.construct(Option(param), intern = false)
          addContext(
            Some(paramType),
            param
              .typeNames()
              .collect { case tn: ANTLRCST.ANTLRTypeName =>
                tn.context
              }
          )

          paramType
        })
  }

  private def addContext(typeName: Option[TypeName], contexts: Seq[TypeNameContext]): Unit = {
    typeName.foreach(tn => {
      contexts match {
        case hd :+ tail =>
          CST.sourceContext.value.get.stampLocation(tn, tail)
          addContext(tn.outer, hd)
        case _ =>
      }
    })
  }
}

object TypeList {
  def construct(typeList: TypeListContext): ArraySeq[TypeName] = {
    TypeReference.constructFromTypeList(typeList)
  }
}
