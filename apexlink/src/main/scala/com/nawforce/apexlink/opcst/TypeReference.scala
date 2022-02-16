package com.nawforce.apexlink.opcst

import com.financialforce.oparser.{
  TypeArguments => OPTypeArguments,
  TypeList => OPTypeList,
  TypeName => OPTypeName,
  TypeRef => OPTypeReference
}
import com.nawforce.pkgforce.names.TypeName

import scala.collection.immutable.ArraySeq
import com.nawforce.apexlink.cst.{
  CSTTypeArguments,
  CSTTypeName,
  CSTTypeReference,
  TypeReference => CSTTypeReference
}

private[opcst] object TypeReference {

  def construct(tr: OPTypeReference): TypeName = {
    CSTTypeReference.construct(Some(new OutlineParserTypeReference(tr)))
  }

  private class OutlineParserTypeName(typeName: OPTypeName) extends CSTTypeName {
    override def typeArguments(): Option[CSTTypeArguments] =
      typeName.typeArguments.map(new OutlineParserTypeArgument(_))
    override def isList: Boolean           = typeName.id.id.lowerCaseContents == "list"
    override def isSet: Boolean            = typeName.id.id.lowerCaseContents == "set"
    override def isMap: Boolean            = typeName.id.id.lowerCaseContents == "map"
    override def getIdText: Option[String] = Option(typeName.id.id.contents)
  }

  private class OutlineParserTypeReference(typeReference: OPTypeReference)
      extends CSTTypeReference {
    override def arraySubscriptsCount(): Int = typeReference.arraySubscripts.length
    override def typeNames(): ArraySeq[CSTTypeName] =
      ArraySeq.from(typeReference.typeNames.map(new OutlineParserTypeName(_)))
  }

  private class OutlineParserTypeArgument(typeArguments: OPTypeArguments) extends CSTTypeArguments {
    override def typeRefs(): ArraySeq[CSTTypeReference] =
      ArraySeq.from(typeArguments.typeList.get.typeRefs.map(new OutlineParserTypeReference(_)))
  }
}

private[opcst] object TypeList {
  def construct(typeList: OPTypeList): ArraySeq[TypeName] = {
    val types = typeList.typeRefs
    ArraySeq.from(types.map(t => TypeReference.construct(t)))
  }
}
