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
import com.nawforce.pkgforce.path.PathLocation
import com.nawforce.runtime.parsers.CodeParser
import io.github.apexdevtools.apexparser.ApexParser.{
  TypeArgumentsContext,
  TypeListContext,
  TypeNameContext,
  TypeRefContext
}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait CSTTypeName {
  def typeArguments(): CSTTypeArguments
  def isList: Boolean
  def isSet: Boolean
  def isMap: Boolean
  def getIdText: Option[String]

  /** Source location of the identifier of this segment, if the adapter can provide one. Collection
    * keywords (List/Set/Map) and synthetic types have no identifier so return None.
    */
  def idLocation: Option[PathLocation] = None
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
      new ANTLRTypeArguments(Option(typeName.typeArguments()))
    override def isList: Boolean = Option(typeName.LIST()).nonEmpty
    override def isSet: Boolean  = Option(typeName.SET()).nonEmpty
    override def isMap: Boolean  = Option(typeName.MAP()).nonEmpty
    override def getIdText: Option[String] =
      Option(typeName.id()).map(id => Option(id).map(_.getText).getOrElse(""))
    override def idLocation: Option[PathLocation] =
      Option(typeName.id()).flatMap(id => CST.sourceContext.value.map(_.getLocation(id)))
  }

  private[cst] class ANTLRTypeReference(typeRef: TypeRefContext) extends CSTTypeReference {
    override def arraySubscriptsCount(): Int =
      Option(typeRef.arraySubscripts()).map(_.getText).getOrElse("").count(_ == '[')
    override def typeNames(): ArraySeq[CSTTypeName] =
      CodeParser.toScala(typeRef.typeName()).map(new ANTLRTypeName(_))
  }
}

object TypeReference {

  /** Accumulator for the located type occurrences of a written type reference. The backing buffer
    * is only created if something is actually recorded, and a null accumulator disables collection
    * altogether so the common construction path does no extra work.
    */
  private final class Occurrences {
    private var buffer: mutable.ArrayBuffer[SourceTypeOccurrence] = _

    def add(typeName: TypeName, location: PathLocation): Unit = {
      if (buffer == null)
        buffer = new mutable.ArrayBuffer[SourceTypeOccurrence](2)
      buffer.append(SourceTypeOccurrence(typeName, location))
    }

    def result(): ArraySeq[SourceTypeOccurrence] =
      if (buffer == null) SourceTypeOccurrence.empty else ArraySeq.from(buffer)
  }

  def construct(typeRefs: List[TypeRefContext]): List[TypeName] = {
    typeRefs.map(x => TypeReference.construct(x))
  }

  def construct(typeRef: TypeRefContext): TypeName = {
    construct(Option(typeRef).map(new ANTLRCST.ANTLRTypeReference(_)))
  }

  def construct(typeRefOpt: Option[CSTTypeReference]): TypeName = {
    build(typeRefOpt, null)
  }

  /** Construct a type name along with the located occurrences of each explicitly written component
    * of the reference, for use in source level accessibility validation.
    */
  def constructWithOccurrences(
    typeRef: TypeRefContext
  ): (TypeName, ArraySeq[SourceTypeOccurrence]) = {
    constructWithOccurrences(Option(typeRef).map(new ANTLRCST.ANTLRTypeReference(_)))
  }

  def constructWithOccurrences(
    typeRefOpt: Option[CSTTypeReference]
  ): (TypeName, ArraySeq[SourceTypeOccurrence]) = {
    val accum    = new Occurrences
    val typeName = build(typeRefOpt, accum)
    (typeName, accum.result())
  }

  private def build(typeRefOpt: Option[CSTTypeReference], accum: Occurrences): TypeName = {
    typeRefOpt
      .map { typeRef =>
        {
          val arraySubs                    = typeRef.arraySubscriptsCount()
          val names: ArraySeq[CSTTypeName] = typeRef.typeNames()

          // Only decode head as rest can't legally be in EncodedName format
          val typeName = createTypeName(decodeName(names.head, accum), names.tail, accum)
          if (accum != null && names.sizeIs > 1) {
            // Only a qualified name can denote a nested type, so 'String', 'Account' and the like
            // are never recorded; type arguments record themselves through the recursion above.
            // Record against the last written identifier, e.g. the 'Hidden' of 'Outer.Hidden'. The
            // array subscripts are excluded so we validate the component, not the List wrapper.
            names.last.idLocation.foreach(location => accum.add(typeName, location))
          }
          typeName.withArraySubscripts(arraySubs)
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

  private def decodeName(name: CSTTypeName, accum: Occurrences): TypeName = {
    val params   = createTypeParams(name.typeArguments(), accum)
    val typeName = getName(name)
    val encType  = EncodedName(typeName)
    if (encType.ext.nonEmpty)
      TypeName(encType.fullName, params, Some(TypeNames.Schema)).intern
    else
      TypeName(typeName, params, None).intern
  }

  @scala.annotation.tailrec
  private def createTypeName(
    outer: TypeName,
    names: Seq[CSTTypeName],
    accum: Occurrences
  ): TypeName = {
    names match {
      case Nil => outer
      case hd +: tl =>
        createTypeName(
          TypeName(getName(hd), createTypeParams(hd.typeArguments(), accum), Some(outer)).intern,
          tl,
          accum
        )
    }
  }

  private def createTypeParams(
    typeArguments: CSTTypeArguments,
    accum: Occurrences
  ): Seq[TypeName] = {
    if (typeArguments.typeRefs().isEmpty)
      TypeName.emptySeq
    else
      typeArguments.typeRefs().map(param => build(Option(param), accum))
  }
}

object TypeList {
  def construct(typeList: TypeListContext): ArraySeq[TypeName] = {
    val types = CodeParser.toScala(typeList.typeRef())
    types.map(t => TypeReference.construct(t))
  }

  def constructWithOccurrences(
    typeList: TypeListContext
  ): (ArraySeq[TypeName], ArraySeq[SourceTypeOccurrence]) = {
    val results = CodeParser.toScala(typeList.typeRef()).map(TypeReference.constructWithOccurrences)
    (results.map(_._1), SourceTypeOccurrence.concat(results.map(_._2)))
  }
}
