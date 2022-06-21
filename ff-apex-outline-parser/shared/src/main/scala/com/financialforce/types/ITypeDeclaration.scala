package com.financialforce.types

import com.financialforce.types.StringUtils.asAnnotationAndModifierString

import scala.collection.immutable.ArraySeq
import scala.util.hashing.MurmurHash3

trait Signature {
  var typeRef: TypeRef
  def id: IdWithLocation
  def annotations: Array[Annotation]
  def modifiers: Array[Modifier]

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")
}

trait IBodyDeclaration {
  def id: IdWithLocation
  def bodyLocation: Option[Location]
  def blockLocation: Option[Location]
}

trait IFormalParameter extends Signature with IdWithLocation {
  def location: Location
  def annotations: Array[Annotation]
  def modifiers: Array[Modifier]
  var typeRef: TypeRef
  def name: String
  def id = new LocatableId(name, location)

  override def toString: String = {
    import StringUtils._
    s"${asAnnotationAndModifierString(annotations, modifiers)}${typeRef.getFullName} $name"
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IFormalParameter]

    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) &&
    other.typeRef.getFullName.equalsIgnoreCase(typeRef.getFullName) &&
    other.name == name
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        typeRef.getFullName.toLowerCase,
        name
      )
    )
  }

}

final case class FormalParameterList(formalParameters: ArraySeq[IFormalParameter]) {
  override def toString: String = {
    formalParameters.mkString(", ")
  }
}

object FormalParameterList {
  final val empty = FormalParameterList(ArraySeq())
}

trait IInitializer extends IBodyDeclaration {
  def isStatic: Boolean
}

trait IConstructorDeclaration extends IBodyDeclaration {
  def qname: QualifiedName
  def annotations: Array[Annotation]
  def modifiers: Array[Modifier]
  def formalParameterList: FormalParameterList

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IConstructorDeclaration]
    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) &&
    other.id == id &&
    other.formalParameterList == formalParameterList
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        id,
        formalParameterList
      )
    )
  }

  override def toString: String = {
    s"${id.location} ${asAnnotationAndModifierString(annotations, modifiers)} ${id.name} $formalParameterList"
  }
}

trait IMethodDeclaration extends IBodyDeclaration {
  def id: IdWithLocation
  def annotations: Array[Annotation]
  def modifiers: Array[Modifier]
  var typeRef: Option[TypeRef]
  def formalParameterList: FormalParameterList

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IMethodDeclaration]
    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) && (
      (other.typeRef.isEmpty && typeRef.isEmpty) ||
      (other.typeRef.nonEmpty && typeRef.nonEmpty &&
      other.typeRef.get.getFullName.equalsIgnoreCase(typeRef.get.getFullName))
    ) &&
    other.id == id &&
    other.formalParameterList == formalParameterList
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        typeRef.map(_.getFullName.toLowerCase().hashCode).getOrElse(0),
        id,
        formalParameterList.hashCode()
      )
    )
  }

  override def toString: String = {
    import StringUtils._
    val typRefString = typeRef match {
      case Some(tr) => tr.getFullName
      case _        => ""
    }
    s"${id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typRefString $id $formalParameterList"
  }
}

case class PropertyBlock(location: Location)

trait IPropertyDeclaration extends IBodyDeclaration with Signature {
  def id: IdWithLocation
  def annotations: Array[Annotation]
  def modifiers: Array[Modifier]
  def typeRef: TypeRef
  def propertyBlocks: Array[PropertyBlock]

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IPropertyDeclaration]
    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) &&
    other.typeRef.getFullName.equalsIgnoreCase(typeRef.getFullName) &&
    other.id == id
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        typeRef.getFullName.toLowerCase().hashCode,
        id.hashCode
      )
    )
  }

  override def toString: String = {
    s"${id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typeRef $id"
  }
}

trait IFieldDeclaration extends IBodyDeclaration with Signature {
  val id: IdWithLocation
  val annotations: Array[Annotation]
  val modifiers: Array[Modifier]
  var typeRef: TypeRef

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IFieldDeclaration]
    (other.annotations sameElements annotations) &&
    (other.modifiers sameElements modifiers) &&
    other.typeRef.getFullName.equalsIgnoreCase(typeRef.getFullName) &&
    other.id == id
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(
      Seq(
        MurmurHash3.arrayHash(annotations),
        MurmurHash3.arrayHash(modifiers),
        typeRef.getFullName.toLowerCase().hashCode,
        id.hashCode
      )
    )
  }

  override def toString: String = {
    s"${id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typeRef $id"
  }
}

// All types conform to this trait, can be used as a resolved TypeRef
trait ITypeDeclaration extends TypeRef {
  def paths: Array[String]
  def location: Location

  def id: IdWithLocation

  def typeNameSegment: TypeNameSegment

  def enclosing: Option[ITypeDeclaration]
  def extendsTypeRef: TypeRef
  def implementsTypeList: ArraySeq[TypeRef]
  def modifiers: Array[Modifier]
  def annotations: Array[Annotation]

  def initializers: ArraySeq[IInitializer]
  def innerTypes: ArraySeq[ITypeDeclaration]
  def constructors: ArraySeq[IConstructorDeclaration]
  def methods: ArraySeq[IMethodDeclaration]
  def properties: ArraySeq[IPropertyDeclaration]
  def fields: ArraySeq[IFieldDeclaration]

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

  def typeName: Array[TypeNameSegment] = {
    enclosing match {
      case Some(enc) => Array(enc.typeNameSegment, typeNameSegment)
      case None      => Array(typeNameSegment)
    }
  }

  override def getFullName: String = {
    enclosing match {
      case Some(enc) => s"${enc.id.toString}.${typeNameSegment.toString}"
      case None      => typeNameSegment.toString
    }
  }
}
