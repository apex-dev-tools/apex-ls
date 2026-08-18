/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved
 */
package com.nawforce.apexlink.opcst

import io.github.apexdevtools.types.base.{
  UnresolvedTypeRef,
  TypeNameSegment => OPTypeName,
  TypeRef => OPTypeRef
}
import com.nawforce.apexlink.cst.{
  CSTTypeArguments,
  CSTTypeName,
  CSTTypeReference,
  SourceTypeOccurrence,
  TypeReference => CSTTypeReferenceAlias
}
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.{PathLike, PathLocation}
import com.nawforce.runtime.platform.OutlineParserLocationOps

import scala.collection.immutable.ArraySeq

private[opcst] object TypeReference {

  def construct(tr: Option[OPTypeRef]): TypeName = {
    CSTTypeReferenceAlias.construct(Some(new OutlineParserTypeReference(tr, None)))
  }

  def construct(tr: OPTypeRef): TypeName = {
    CSTTypeReferenceAlias.construct(Some(new OutlineParserTypeReference(Some(tr), None)))
  }

  /** As construct, but also returning the located occurrences of each written component of the
    * reference so that source level accessibility can be validated.
    */
  def constructWithOccurrences(
    tr: Option[OPTypeRef],
    path: PathLike
  ): (TypeName, ArraySeq[SourceTypeOccurrence]) = {
    CSTTypeReferenceAlias.constructWithOccurrences(
      Some(new OutlineParserTypeReference(tr, Some(path)))
    )
  }

  def constructWithOccurrences(
    tr: OPTypeRef,
    path: PathLike
  ): (TypeName, ArraySeq[SourceTypeOccurrence]) = {
    constructWithOccurrences(Some(tr), path)
  }

  private class OutlineParserTypeName(typeName: OPTypeName, path: Option[PathLike])
      extends CSTTypeName {
    override def typeArguments(): CSTTypeArguments =
      new OutlineParserTypeArgument(typeName.typeArguments, path)
    override def isList: Boolean           = typeName.id.lowerCaseName == "list"
    override def isSet: Boolean            = typeName.id.lowerCaseName == "set"
    override def isMap: Boolean            = typeName.id.lowerCaseName == "map"
    override def getIdText: Option[String] = Option(typeName.id.name)
    override def idLocation: Option[PathLocation] = {
      // Collection keywords are not identifiers, matching how the ANTLR adapter sees them
      if (isList || isSet || isMap) None
      else path.map(PathLocation(_, OutlineParserLocationOps.toLocation(typeName.id.location)))
    }
  }

  private class OutlineParserTypeReference(typeReference: Option[OPTypeRef], path: Option[PathLike])
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
          ArraySeq.from(utr.typeNameSegments.map(new OutlineParserTypeName(_, path)))
        case _ => ArraySeq.empty
      }
    }
  }

  private class OutlineParserTypeArgument(
    typeArguments: ArraySeq[OPTypeRef],
    path: Option[PathLike]
  ) extends CSTTypeArguments {
    override def typeRefs(): ArraySeq[CSTTypeReference] =
      ArraySeq.from(typeArguments.map(tr => new OutlineParserTypeReference(Some(tr), path)))
  }
}

private[opcst] object TypeList {
  def construct(typeList: ArraySeq[OPTypeRef]): ArraySeq[TypeName] = {
    ArraySeq.from(typeList.map(t => TypeReference.construct(Some(t))))
  }

  def constructWithOccurrences(
    typeList: ArraySeq[OPTypeRef],
    path: PathLike
  ): (ArraySeq[TypeName], ArraySeq[SourceTypeOccurrence]) = {
    val results =
      ArraySeq.from(typeList.map(t => TypeReference.constructWithOccurrences(Some(t), path)))
    (results.map(_._1), results.flatMap(_._2))
  }
}
