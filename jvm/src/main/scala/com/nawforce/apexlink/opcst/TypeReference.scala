/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved
 */
package com.nawforce.apexlink.opcst

import com.financialforce.types.base.{
  UnresolvedTypeRef,
  TypeNameSegment => OPTypeName,
  TypeRef => OPTypeRef
}
import com.nawforce.apexlink.cst.{
  CSTTypeArguments,
  CSTTypeName,
  CSTTypeReference,
  TypeReference => CSTTypeReferenceAlias
}
import com.nawforce.pkgforce.names.TypeName

import scala.collection.immutable.ArraySeq

private[opcst] object TypeReference {

  def construct(tr: Option[OPTypeRef]): TypeName = {
    CSTTypeReferenceAlias.construct(Some(new OutlineParserTypeReference(tr)))
  }

  def construct(tr: OPTypeRef): TypeName = {
    CSTTypeReferenceAlias.construct(Some(new OutlineParserTypeReference(Some(tr))))
  }

  private class OutlineParserTypeName(typeName: OPTypeName) extends CSTTypeName {
    override def typeArguments(): CSTTypeArguments =
      new OutlineParserTypeArgument(typeName.typeArguments)
    override def isList: Boolean           = typeName.id.lowerCaseName == "list"
    override def isSet: Boolean            = typeName.id.lowerCaseName == "set"
    override def isMap: Boolean            = typeName.id.lowerCaseName == "map"
    override def getIdText: Option[String] = Option(typeName.id.name)
  }

  private class OutlineParserTypeReference(typeReference: Option[OPTypeRef])
      extends CSTTypeReference {
    override def arraySubscriptsCount(): Int = {
      // TODO: is this actually right behaviour?
      typeReference match {
        case Some(utr: UnresolvedTypeRef) => utr.arraySubscripts
        case _                            => 0
      }
    }

    override def typeNames(): ArraySeq[CSTTypeName] = {
      // TODO: is this actually right behaviour?
      typeReference match {
        case Some(utr: UnresolvedTypeRef) =>
          ArraySeq.from(utr.typeNameSegments.map(new OutlineParserTypeName(_)))
        case _ => ArraySeq.empty
      }
    }
  }

  private class OutlineParserTypeArgument(typeArguments: ArraySeq[OPTypeRef])
      extends CSTTypeArguments {
    override def typeRefs(): ArraySeq[CSTTypeReference] =
      ArraySeq.from(typeArguments.map(tr => new OutlineParserTypeReference(Some(tr))))
  }
}

private[opcst] object TypeList {
  def construct(typeList: ArraySeq[OPTypeRef]): ArraySeq[TypeName] = {
    ArraySeq.from(typeList.map(t => TypeReference.construct(Some(t))))
  }
}
