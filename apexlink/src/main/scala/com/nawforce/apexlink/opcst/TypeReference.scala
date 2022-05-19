package com.nawforce.apexlink.opcst

import com.financialforce.oparser.{
  UnresolvedTypeRef,
  TypeArguments => OPTypeArguments,
  TypeList => OPTypeList,
  TypeNameSegment => OPTypeName,
  TypeRef => OPTypeReference
}
import com.nawforce.pkgforce.names.TypeName

import scala.collection.immutable.ArraySeq
import com.nawforce.apexlink.cst.{
  CSTTypeArguments,
  CSTTypeName,
  CSTTypeReference,
  TypeReference => CSTTypeReferenceAlias
}

private[opcst] object TypeReference {

  def construct(tr: Option[OPTypeReference]): TypeName = {
    CSTTypeReferenceAlias.construct(Some(new OutlineParserTypeReference(tr)))
  }

  def construct(tr: OPTypeReference): TypeName = {
    CSTTypeReferenceAlias.construct(Some(new OutlineParserTypeReference(Some(tr))))
  }

  private class OutlineParserTypeName(typeName: OPTypeName) extends CSTTypeName {
    override def typeArguments(): CSTTypeArguments =
      new OutlineParserTypeArgument(typeName.typeArguments)
    override def isList: Boolean           = typeName.id.id.lowerCaseContents == "list"
    override def isSet: Boolean            = typeName.id.id.lowerCaseContents == "set"
    override def isMap: Boolean            = typeName.id.id.lowerCaseContents == "map"
    override def getIdText: Option[String] = Option(typeName.id.id.contents)
  }

  private class OutlineParserTypeReference(typeReference: Option[OPTypeReference])
      extends CSTTypeReference {
    override def arraySubscriptsCount(): Int = {
      //TODO: is this actually right behaviour?
      typeReference match {
        case Some(utr: UnresolvedTypeRef) => utr.arraySubscripts
        case _                            => 0
      }
    }

    override def typeNames(): ArraySeq[CSTTypeName] = {
      //TODO: is this actually right behaviour?
      typeReference match {
        case Some(utr: UnresolvedTypeRef) =>
          ArraySeq.from(utr.typeNameSegments.map(new OutlineParserTypeName(_)))
        case _ => ArraySeq.empty
      }
    }
  }

  private class OutlineParserTypeArgument(typeArguments: OPTypeArguments) extends CSTTypeArguments {
    override def typeRefs(): ArraySeq[CSTTypeReference] =
      ArraySeq.from(
        typeArguments.typeList.typeRefs.map(tr => new OutlineParserTypeReference(Some(tr)))
      )
  }
}

private[opcst] object TypeList {
  def construct(typeList: OPTypeList): ArraySeq[TypeName] = {
    val types = typeList.typeRefs
    ArraySeq.from(types.map(t => TypeReference.construct(Some(t))))
  }
}
