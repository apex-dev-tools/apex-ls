package com.nawforce.apexlink.opcst

import com.financialforce.oparser.{TypeArguments => OPTypeArguments, TypeName => OPTypeName, TypeRef => OPTypeReference, TypeList => OPTypeList}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames._
import com.nawforce.pkgforce.names.{EncodedName, Name, Names, TypeName}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

private [opcst] object TypeReference {

  def construct(tr: OPTypeReference): TypeName ={
    val arraySubs = tr.arraySubscripts.length
    val names = tr.typeNames
    if(tr.typeNames.head.id.id.lowerCaseContents == "void") TypeNames.Void
    else createTypeName(decodeName(names.head), names.tail.toSeq).withArraySubscripts(arraySubs)
  }

  private def decodeName(name: OPTypeName): TypeName = {
    val params = createTypeParams(name.typeArguments)
    val typeName = getName(name)
    val encType = EncodedName(typeName)
    if (encType.ext.nonEmpty)
      TypeName(encType.fullName, params, Some(TypeNames.Schema)).intern
    else
      TypeName(typeName, params, None).intern
  }

  private def getName(name: OPTypeName): Name = {
    if(name.id.id.lowerCaseContents == "list") Names.ListName
    else if(name.id.id.lowerCaseContents == "set") Names.SetName
    else if(name.id.id.lowerCaseContents == "map") Names.MapName
    else Option(name.id).map(id => Names(id.id.contents)).getOrElse(Names.Empty)
  }

  private def createTypeParams(typeArguments: Option[OPTypeArguments]): Seq[TypeName] = {
    typeArguments
      .flatMap(a => a.typeList)
      .map(tl => tl.typeRefs.toSeq)
      .getOrElse(Seq())
      .map(construct)
  }

  @tailrec
  private def createTypeName(outer: TypeName, names: Seq[OPTypeName]): TypeName = {
    names match {
      case Nil => outer
      case hd +: tl =>
        createTypeName(TypeName(getName(hd),
          createTypeParams(hd.typeArguments),
          Some(outer)).intern,
          tl)
    }
  }
}

private [opcst] object TypeList {
  def construct(typeList: OPTypeList): ArraySeq[TypeName] = {
    val types = typeList.typeRefs
    ArraySeq.from(types.map(t => TypeReference.construct(t)))
  }
}
