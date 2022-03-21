/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object StringUtils {

  def asString[T](o: Option[T]): String = {
    o match {
      case None => ""
      case Some(o) => s"${o.toString}"
    }
  }

  def asString[T](a: mutable.ArrayBuffer[T]): String = {
    asString(a, " ")
  }

  def asString[T](a: mutable.ArrayBuffer[T], separator: String): String = {
    a.map(_.toString).mkString(separator)
  }
}

trait IdAssignable {
  def add(i: Id): Unit
}

trait AnnotationAssignable {
  def add(a: Annotation): Unit
}

trait ModifierAssignable extends AnnotationAssignable {
  def add(m: Modifier): Unit
}

trait TypeRefAssignable {
  def add(tr: TypeRef): Unit
}

trait TypeNameAssignable {
  def add(tn: TypeName): Unit
}

trait TypeListAssignable {
  def add(tl: TypeList): Unit
}

trait TypeArgumentsAssignable {
  def add(ta: TypeArguments): Unit
}

trait ArraySubscriptsAssignable {
  def add(as: ArraySubscripts): Unit
}

trait FormalParameterAssignable {
  def add(fp: FormalParameter): Unit
}

trait MethodDeclarationAssignable {
  def add(md: MethodDeclaration): Unit
}

trait InitializerAssignable {
  def add(init: Initializer): Unit
}

trait PropertyBlockAssignable {
  def add(pb: PropertyBlock): Unit
}

trait Signature {
  val typeRef: TypeRef
  val id: Id
  val annotations: mutable.ArrayBuffer[Annotation]
  val modifiers: mutable.ArrayBuffer[Modifier]
}

trait SignatureWithParameterList extends Signature {
  val formalParameterList: FormalParameterList
}

class MemberDeclaration extends ModifierAssignable with TypeRefAssignable {

  val annotations: mutable.ArrayBuffer[Annotation] = mutable.ArrayBuffer[Annotation]()
  val modifiers: mutable.ArrayBuffer[Modifier]     = mutable.ArrayBuffer[Modifier]()
  var typeRef: Option[TypeRef]                     = None

  override def add(a: Annotation): Unit = annotations.append(a)

  override def add(m: Modifier): Unit = modifiers.append(m)

  override def add(tr: TypeRef): Unit = typeRef = Some(tr)
}

case class Annotation(qName: QualifiedName, parameters: Option[String]) {
  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Annotation]
    qName == other.qName
    // TODO: Are parameters required?
  }

  override def toString: String = {
    if (parameters.isDefined) s"@$qName(${parameters.get})" else s"@$qName"
  }
}

case class Modifier(token: Token) {
  def text: String = token.contents

  def location: Location = token.location

  override def toString: String = text

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Modifier]
    token.lowerCaseContents == other.token.lowerCaseContents
  }
}

case class Id(id: IdToken) {
  override def toString: String = id.contents

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Id]
    id.lowerCaseContents.equalsIgnoreCase(other.id.lowerCaseContents)
  }
}

class QualifiedName extends IdAssignable {
  val qName: mutable.ArrayBuffer[Id] = mutable.ArrayBuffer[Id]()

  def location: Location = {
    val start = qName.head.id.location
    val end   = qName.last.id.location
    Location.from(start, end)
  }

  override def add(i: Id): Unit = qName.append(i)

  override def toString: String = {
    import StringUtils._
    asString(qName, ".")
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[QualifiedName]
    other.qName == qName
  }
}

class TypeRef extends TypeNameAssignable with ArraySubscriptsAssignable {
  val typeNames: mutable.ArrayBuffer[TypeName]              = mutable.ArrayBuffer[TypeName]()
  val arraySubscripts: mutable.ArrayBuffer[ArraySubscripts] = mutable.ArrayBuffer[ArraySubscripts]()

  override def add(tn: TypeName): Unit = typeNames.append(tn)

  override def add(as: ArraySubscripts): Unit = arraySubscripts.append(as)

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[TypeRef]
    other.typeNames == typeNames && other.arraySubscripts == arraySubscripts
  }

  override def toString: String = {
    import StringUtils._
    s"${asString(typeNames, ".")}${asString(arraySubscripts, "")}"
  }
}

class TypeName(val id: Id) extends TypeArgumentsAssignable {
  var typeArguments: Option[TypeArguments] = None

  override def add(ta: TypeArguments): Unit = typeArguments = Some(ta)

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[TypeName]

    id == other.id && typeArguments == other.typeArguments
  }

  override def toString: String = {
    import StringUtils._
    s"$id ${asString(typeArguments)}"
  }
}

class TypeArguments extends TypeListAssignable {
  var typeList: Option[TypeList] = None

  override def add(tl: TypeList): Unit = typeList = Some(tl)

  override def toString: String = {
    import StringUtils._
    asString(typeList)
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[TypeArguments]
    other.typeList == typeList
  }
}

class TypeList extends TypeRefAssignable {
  val typeRefs: mutable.ArrayBuffer[TypeRef] = mutable.ArrayBuffer[TypeRef]()

  override def add(tr: TypeRef): Unit = typeRefs.append(tr)

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[TypeList]
    other.typeRefs == typeRefs
  }

  override def toString: String = {
    import StringUtils._
    asString(typeRefs, ", ")
  }
}

case class ArraySubscripts() {
  override def toString: String = "[]"
}

class FormalParameter extends ModifierAssignable with TypeRefAssignable with IdAssignable {

  val annotations: mutable.ArrayBuffer[Annotation] = mutable.ArrayBuffer[Annotation]()
  val modifiers: mutable.ArrayBuffer[Modifier]     = mutable.ArrayBuffer[Modifier]()
  var typeRef: Option[TypeRef]                     = None
  var id: Option[Id]                               = None

  override def add(a: Annotation): Unit = annotations.append(a)

  override def add(m: Modifier): Unit = modifiers.append(m)

  override def add(tr: TypeRef): Unit = typeRef = Some(tr)

  override def add(i: Id): Unit = id = Some(i)

  override def toString: String = {
    import StringUtils._
    s"${asString(annotations)} ${asString(modifiers)} ${asString(typeRef)} ${asString(id)}"
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[FormalParameter]

    other.annotations.sameElements(annotations) &&
    other.modifiers.sameElements(modifiers) &&
    other.typeRef == typeRef &&
    other.id == id
  }
}

class FormalParameterList extends FormalParameterAssignable {
  val formalParameters: mutable.ArrayBuffer[FormalParameter] =
    mutable.ArrayBuffer[FormalParameter]()

  override def add(fp: FormalParameter): Unit = formalParameters.append(fp)

  override def toString: String = {
    import StringUtils._
    asString(formalParameters, ",")
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[FormalParameterList]
    other.formalParameters == formalParameters
  }
}

class PropertyBlock {
  var blockLocation: Option[Location] = None
}

sealed trait BodyDeclaration {
  val id: Id
  var location: Option[Location]
  var blockLocation: Option[Location]
}

case class ConstructorDeclaration(
  annotations: mutable.ArrayBuffer[Annotation],
  modifiers: mutable.ArrayBuffer[Modifier],
  qName: QualifiedName,
  formalParameterList: FormalParameterList
) extends BodyDeclaration {

  val id: Id                          = qName.qName(0)
  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[ConstructorDeclaration]
    other.annotations.sameElements(annotations) &&
    other.modifiers.sameElements(modifiers) &&
      other.qName == qName &&
      other.formalParameterList == formalParameterList
  }

  override def toString: String = {
    import StringUtils._
    s"${qName.location} ${asString(annotations)} ${asString(modifiers)} $qName $formalParameterList"
  }
}

object ConstructorDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[ConstructorDeclaration] = mutable.ArrayBuffer[ConstructorDeclaration]()
}

case class MethodDeclaration(
                              annotations: mutable.ArrayBuffer[Annotation],
                              modifiers: mutable.ArrayBuffer[Modifier],
                              typeRef: TypeRef,
                              id: Id,
                              formalParameterList: FormalParameterList
                            ) extends BodyDeclaration
  with SignatureWithParameterList {

  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[MethodDeclaration]
    other.annotations.sameElements(annotations) &&
    other.modifiers.sameElements(modifiers) &&
    other.typeRef == typeRef &&
      other.id == id &&
      other.formalParameterList == formalParameterList
  }

  override def toString: String = {
    import StringUtils._
    s"${id.id.location} ${asString(annotations)} ${asString(modifiers)} $typeRef $id $formalParameterList"
  }
}

object MethodDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[MethodDeclaration] = mutable.ArrayBuffer[MethodDeclaration]()
}

class PropertyDeclaration(
                           val annotations: mutable.ArrayBuffer[Annotation],
                           val modifiers: mutable.ArrayBuffer[Modifier],
                           val typeRef: TypeRef,
                           val id: Id
                         ) extends BodyDeclaration
  with Signature
  with PropertyBlockAssignable {

  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  val propertyBlocks: mutable.ArrayBuffer[PropertyBlock] = mutable.ArrayBuffer[PropertyBlock]()

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[PropertyDeclaration]
    other.annotations.sameElements(annotations) &&
    other.modifiers.sameElements(modifiers) &&
    other.typeRef == typeRef &&
    other.id == id
  }

  override def toString: String = {
    import StringUtils._
    s"${id.id.location} ${asString(annotations)} ${asString(modifiers)} $typeRef $id"
  }

  override def add(pb: PropertyBlock): Unit = propertyBlocks.append(pb)
}

object PropertyDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[PropertyDeclaration] = mutable.ArrayBuffer[PropertyDeclaration]()
}

case class FieldDeclaration(
  annotations: mutable.ArrayBuffer[Annotation],
  modifiers: mutable.ArrayBuffer[Modifier],
  typeRef: TypeRef,
  id: Id
) extends BodyDeclaration
    with Signature {

  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[FieldDeclaration]
    other.annotations.sameElements(annotations) &&
    other.modifiers.sameElements(modifiers) &&
    other.typeRef == typeRef &&
    other.id == id
  }

  override def toString: String = {
    import StringUtils._
    s"${id.id.location} ${asString(annotations)} ${asString(modifiers)} ${typeRef} ${id}"
  }
}

object FieldDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[FieldDeclaration] = mutable.ArrayBuffer[FieldDeclaration]()
}

object Initializer {
  val id = Id(IdToken("initializer", Location.default))
}

case class Initializer(isStatic: Boolean) extends BodyDeclaration {
  override val id: Id = Initializer.id
  override var location: Option[Location] = None
  override var blockLocation: Option[Location] = None
}

trait ITypeDeclaration {
  def paths: ArraySeq[String]
  def location: Location

  def id: Id
  def enclosing: ClassTypeDeclaration
  def extendsTypeRef: TypeRef
  def implementsTypeList: TypeList

  def modifiers: ArraySeq[Modifier]
  def annotations: ArraySeq[Annotation]
  def initializers: ArraySeq[Initializer]

  def innerTypes: ArraySeq[TypeDeclaration]
  def constructors: ArraySeq[ConstructorDeclaration]
  def methods: ArraySeq[MethodDeclaration]
  def properties: ArraySeq[PropertyDeclaration]
  def fields: ArraySeq[FieldDeclaration]
}

sealed class TypeDeclaration(val path: String, val enclosing: ClassTypeDeclaration) extends ITypeDeclaration {
  var _location: Location = null

  var _id: Id = null
  var _extendsTypeRef: TypeRef = null
  var _implementsTypeList: TypeList = null

  var _modifiers: mutable.ArrayBuffer[Modifier] = mutable.ArrayBuffer()
  var _annotations: mutable.ArrayBuffer[Annotation] = mutable.ArrayBuffer()
  var _initializers: mutable.ArrayBuffer[Initializer] = mutable.ArrayBuffer()

  var _innerTypes: mutable.ArrayBuffer[TypeDeclaration]= mutable.ArrayBuffer()
  var _constructors: mutable.ArrayBuffer[ConstructorDeclaration]= mutable.ArrayBuffer()
  var _methods: mutable.ArrayBuffer[MethodDeclaration]= mutable.ArrayBuffer()
  var _properties: mutable.ArrayBuffer[PropertyDeclaration]= mutable.ArrayBuffer()
  var _fields: mutable.ArrayBuffer[FieldDeclaration]= mutable.ArrayBuffer()

  override def paths: ArraySeq[String] = ArraySeq(path)
  override def location: Location = _location

  override def id: Id = _id
  override def extendsTypeRef: TypeRef = _extendsTypeRef
  override def implementsTypeList: TypeList = _implementsTypeList

  override def modifiers: ArraySeq[Modifier]= ArraySeq.unsafeWrapArray(_modifiers.toArray)
  override def annotations: ArraySeq[Annotation]= ArraySeq.unsafeWrapArray(_annotations.toArray)
  override def initializers: ArraySeq[Initializer]= ArraySeq.unsafeWrapArray(_initializers.toArray)

  override def innerTypes: ArraySeq[TypeDeclaration] = ArraySeq.unsafeWrapArray(_innerTypes.toArray)
  override def constructors: ArraySeq[ConstructorDeclaration]= ArraySeq.unsafeWrapArray(_constructors.toArray)
  override def methods: ArraySeq[MethodDeclaration]= ArraySeq.unsafeWrapArray(_methods.toArray)
  override def properties: ArraySeq[PropertyDeclaration]= ArraySeq.unsafeWrapArray(_properties.toArray)
  override def fields: ArraySeq[FieldDeclaration]= ArraySeq.unsafeWrapArray(_fields.toArray)

}

class TypeDeclarationFactory {
  def createClassTypeDeclaration(path: String, enclosing: ClassTypeDeclaration): ClassTypeDeclaration = {
    new ClassTypeDeclaration(path, enclosing)
  }

  def createInterfaceTypeDeclaration(path: String, enclosing: ClassTypeDeclaration): InterfaceTypeDeclaration = {
    new InterfaceTypeDeclaration(path, enclosing)
  }

  def createEnumTypeDeclaration(path: String, enclosing: ClassTypeDeclaration): EnumTypeDeclaration = {
    new EnumTypeDeclaration(path, enclosing)
  }
}

object TypeDeclaration {
  final val emptyArrayBuffer = mutable.ArrayBuffer[TypeDeclaration]()
}

class ClassTypeDeclaration(path: String, enclosing: ClassTypeDeclaration)
  extends TypeDeclaration(path, enclosing)
    with AnnotationAssignable
    with IdAssignable
    with ModifierAssignable
    with TypeRefAssignable
    with TypeListAssignable
    with MethodDeclarationAssignable
    with InitializerAssignable {

  override def add(a: Annotation): Unit = _annotations.append(a)

  override def add(i: Id): Unit = _id = i

  override def add(m: Modifier): Unit = _modifiers.append(m)

  override def add(tr: TypeRef): Unit = _extendsTypeRef = tr

  override def add(tl: TypeList): Unit = _implementsTypeList = tl

  override def add(md: MethodDeclaration): Unit = _methods.append(md)

  override def add(init: Initializer): Unit = _initializers.append(init)

  override def toString: String = {
    import StringUtils._
    val base =
      s"""Class:      $id
         |Path:       $path
         |Location:   ${id.id.location}
         |Annotation: ${asString(_annotations)}
         |Modifiers:  ${asString(_modifiers)}
         |Extends:    ${_extendsTypeRef}
         |Implements: ${_implementsTypeList}
         |""".stripMargin

    val c =
      if (_constructors.isEmpty) ""
      else
        s"""
           |Constructors:
           |${_constructors.mkString("\n")}
           |""".stripMargin

    val m =
      if (_methods.isEmpty) ""
      else
        s"""
           |Methods:
           |${_methods.mkString("\n")}
           |""".stripMargin

    val p =
      if (_properties.isEmpty) ""
      else
        s"""
           |Properties:
           |${_properties.mkString("\n")}
           |""".stripMargin

    val f =
      if (_fields.isEmpty) ""
      else
        s"""
           |Fields:
           |${_fields.mkString("\n")}
           |""".stripMargin

    val i =
      if (innerTypes.isEmpty) ""
      else
        s"""
           |Inner types:
           |${innerTypes.mkString("\n")}
           |""".stripMargin

    base + c + m + p + f + i
  }
}

class InterfaceTypeDeclaration(path: String, enclosing: ClassTypeDeclaration)
  extends TypeDeclaration(path, enclosing)
    with AnnotationAssignable
    with IdAssignable
    with ModifierAssignable
    with TypeListAssignable
    with MethodDeclarationAssignable {

  override def add(a: Annotation): Unit = _annotations.append(a)

  override def add(i: Id): Unit = _id = i

  override def add(m: Modifier): Unit = _modifiers.append(m)

  override def add(tl: TypeList): Unit = _implementsTypeList = tl

  override def add(md: MethodDeclaration): Unit = _methods.append(md)

  override def toString: String = {
    import StringUtils._
    s"""Interface:  ${id}
       |Path:       ${path}
       |Location:   ${id.id.location}
       |Annotation: ${asString(_annotations)}
       |Modifiers:  ${asString(_modifiers)}
       |Implements: ${_implementsTypeList}
       |Methods:
       |${methods.mkString("\n")}
       |
       |""".stripMargin
  }
}

class EnumTypeDeclaration(path: String, enclosing: ClassTypeDeclaration)
  extends TypeDeclaration(path, enclosing)
    with IdAssignable
    with ModifierAssignable {

  val constants: mutable.ArrayBuffer[Id] = mutable.ArrayBuffer[Id]()

  override def add(a: Annotation): Unit = _annotations.append(a)

  override def add(i: Id): Unit = _id = i

  override def add(m: Modifier): Unit = _modifiers.append(m)

  override def toString: String = {
    import StringUtils._
    s"""Enum:        ${id}
       |Path:       ${path}
       |Location:   ${id.id.location}
       |Annotation: ${asString(_annotations)}
       |Modifiers:  ${asString(_modifiers)}
       |Constants:
       |${constants.map(i => s"${i.id.location} ${i.id.contents}").mkString("\n")}
       |
       |""".stripMargin
  }
}

object Parse {

  def parseClassType(ctd: ClassTypeDeclaration, tokens: Tokens, path: String): ClassTypeDeclaration = {
    var index = parseModifiers(0, tokens, ctd)

    if (!tokens(index).exists(_.matches(Tokens.ClassStr)))
      throw new Exception(s"Missing '${Tokens.ClassStr}'")
    index += 1
    index = parseId(index, tokens, ctd)

    index = tokens.findIndex(t => t.matches(Tokens.ExtendsStr))
    if (index != -1) {
      index = parseTypeRef(index + 1, tokens, ctd)
    }

    index = tokens.findIndex(t => t.matches(Tokens.ImplementsStr))
    if (index != -1) {
      index = parseTypeList(index + 1, tokens, ctd)
    }
    ctd
  }

  def parseInterfaceType(itd: InterfaceTypeDeclaration, tokens: Tokens, path: String): InterfaceTypeDeclaration = {
    var index = parseModifiers(0, tokens, itd)

    if (!tokens(index).exists(_.matches(Tokens.InterfaceStr)))
      throw new Exception(s"Missing '${Tokens.InterfaceStr}'")
    index += 1
    index = parseId(index, tokens, itd)

    index = tokens.findIndex(t => t.matches(Tokens.ExtendsStr))
    if (index != -1) {
      index = parseTypeList(index + 1, tokens, itd)
    }

    itd
  }

  def parseEnumType(etd: EnumTypeDeclaration, tokens: Tokens, path: String): EnumTypeDeclaration = {
    var index = parseModifiers(0, tokens, etd)

    if (!tokens(index).exists(_.matches(Tokens.EnumStr)))
      throw new Exception(s"Missing '${Tokens.EnumStr}'")
    index += 1
    index = parseId(index, tokens, etd)

    etd
  }

  private def innerTypes = Set(Tokens.ClassStr, Tokens.InterfaceStr, Tokens.EnumStr)

  def getInnerType(tokens: Tokens): Option[Token] = {
    tokens.hasToken(innerTypes)
  }

  def parseClassMember(
    ctd: ClassTypeDeclaration,
    tokens: Tokens,
    nextToken: Token
  ): (Boolean, Seq[BodyDeclaration]) = {

    if (tokens.isEmpty()) {
      return (true, addInitializer(ctd, false).toSeq)
    }

    val md    = new MemberDeclaration
    var index = parseModifiers(0, tokens, md)

    if (index >= tokens.length()) {
      val isStatic = tokens.hasToken(Set(Tokens.StaticStr)).isDefined
      return (true, addInitializer(ctd, isStatic).toSeq)
    }

    index = parseTypeRef(index, tokens, md)

    if (tokens(index).exists(_.matches(Tokens.LParenStr))) {
      (true, addConstructor(index, tokens, md, ctd).toSeq)
    } else if (tokens.findIndex(index, token => token.matches(Tokens.EqualsStr)) != -1) {
      // Consume fields in one go
      if (nextToken.lowerCaseContents == Tokens.LBraceStr) (false, Seq.empty)
      else (true, addFields(index, tokens, md, ctd, nextToken))
    } else if (tokens.findIndex(index, token => token.matches(Tokens.LParenStr)) != -1) {
      (true, addMethod(index, tokens, md, ctd).toSeq)
    } else if (nextToken.matches(Tokens.LBraceStr)) {
      (true, addProperty(index, tokens, md, ctd).toSeq)
    } else {
      // Consume fields in one go
      if (nextToken.lowerCaseContents == Tokens.LBraceStr) (false, Seq.empty)
      else (true, addFields(index, tokens, md, ctd, nextToken))
    }
  }

  def parseInterfaceMember(itd: InterfaceTypeDeclaration, tokens: Tokens): Seq[BodyDeclaration] = {
    if (tokens.isEmpty()) return Seq.empty

    val md    = new MemberDeclaration
    var index = parseModifiers(0, tokens, md)
    index = parseTypeRef(index, tokens, md)
    addMethod(index, tokens, md, itd).toSeq
  }

  def parseEnumMember(etd: EnumTypeDeclaration, tokens: Tokens): Seq[Id] = {
    if (tokens.isEmpty()) return Seq.empty

    val constant = tokenToId(tokens(0).get)
    etd.constants.append(constant)
    Seq(constant)
  }

  def parsePropertyBlock(
    propertyDeclaration: PropertyDeclaration,
    tokens: Tokens
  ): Option[PropertyBlock] = {
    val pb = new PropertyBlock
    propertyDeclaration.add(pb)
    Some(pb)
  }

  private def toQualifiedName(tr: TypeRef): QualifiedName = {
    val qName = new QualifiedName
    tr.typeNames.foreach(tn => qName.add(tn.id))
    qName
  }

  private def addInitializer(ctd: ClassTypeDeclaration, isStatic: Boolean): Option[Initializer] = {
    val initializer = Initializer(isStatic)
    ctd.add(initializer)
    Some(initializer)
  }

  private def addConstructor(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    ctd: ClassTypeDeclaration
  ): Option[ConstructorDeclaration] = {

    val formalParameterList = new FormalParameterList
    val index               = parseFormalParameterList(startIndex, tokens, formalParameterList)
    if (index < tokens.length()) {
      throw new Exception(s"Unrecognised constructor ${tokens.toString()}")
    }
    val constructor = ConstructorDeclaration(
      md.annotations,
      md.modifiers,
      toQualifiedName(md.typeRef.get),
      formalParameterList
    )
    ctd._constructors.append(constructor)
    Some(constructor)
  }

  private def addMethod(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    res: MethodDeclarationAssignable
  ): Option[MethodDeclaration] = {

    if (md.typeRef.isEmpty) {
      throw new Exception(s"Unrecognised method ${tokens.toString()}")
    }

    val id    = tokenToId(tokens(startIndex).get)
    var index = startIndex + 1

    val formalParameterList = new FormalParameterList
    index = parseFormalParameterList(index, tokens, formalParameterList)
    if (index < tokens.length()) {
      throw new Exception(s"Unrecognised method ${tokens.toString()}")
    }
    val method =
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef.get, id, formalParameterList)
    res.add(method)
    Some(method)
  }

  private def addProperty(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    ctd: ClassTypeDeclaration
  ): Option[PropertyDeclaration] = {

    val id    = tokenToId(tokens(startIndex).get)
    val index = startIndex + 1

    if (index < tokens.length()) {
      throw new Exception(s"Unrecognised property ${tokens.toString()}")
    }

    val property =
      new PropertyDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)
    ctd._properties.append(property)
    Some(property)
  }

  private def addFields(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    ctd: ClassTypeDeclaration,
    nextToken: Token
  ): Seq[FieldDeclaration] = {

    val fields = mutable.ArrayBuffer[FieldDeclaration]()

    def skipToNextField(startIndex: Int): (Location, Int) = {
      var nestedParenthesis = 0
      var angleBrackets     = 0
      var squareBrackets    = 0
      var index             = startIndex
      var found             = false
      var endLocation       = nextToken.location
      while (!found && index < tokens.length()) {
        tokens(index) match {
          case Some(t: NonIdToken) =>
            t.contents match {
              case Tokens.LParenStr =>
                nestedParenthesis += 1
                index += 1
              case Tokens.RParenStr =>
                nestedParenthesis -= 1
                index += 1
              case Tokens.LessThanStr =>
                angleBrackets += 1
                index += 1
              case Tokens.GreaterThanStr =>
                angleBrackets -= 1
                index += 1
              case Tokens.LBrackStr =>
                squareBrackets += 1
                index += 1
              case Tokens.RBrackStr =>
                squareBrackets -= 1
                index += 1
              case Tokens.CommaStr =>
                found = nestedParenthesis == 0 && angleBrackets == 0 && squareBrackets == 0
                if (found)
                  endLocation = t.location
                index += 1
              case _ => index += 1
            }
          case _ => index += 1
        }
      }
      (endLocation, index)
    }

    var index                           = startIndex
    var startLocation: Option[Location] = None
    var endLocation                     = Location.default
    while (index < tokens.length()) {
      val id = tokenToId(tokens(index).get)

      val field = FieldDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)
      ctd._fields.append(field)
      fields.append(field)

      index += 1
      startLocation = tokens(index)
        .filter(_.matches(Tokens.EqualsStr))
        .map(_.location)

      val endLocationAndIndex = skipToNextField(index)
      endLocation = endLocationAndIndex._1
      index = endLocationAndIndex._2

      if (startLocation.isDefined)
        field.blockLocation = Some(
          Location(
            startLine = startLocation.get.startLine,
            startLineOffset = startLocation.get.startLineOffset,
            startByteOffset = startLocation.get.endByteOffset + 1,
            endLine = endLocation.startLine,
            endLineOffset = endLocation.startLineOffset,
            endByteOffset = endLocation.startByteOffset
          )
        )
    }
    fields.toSeq
  }

  private def tokenToModifier(token: Token): Modifier = Modifier(token)

  private def tokenToId(token: Token): Id = Id(IdToken(token.contents, token.location))

  private def parseId(startIndex: Int, tokens: Tokens, res: IdAssignable): Int = {
    if (startIndex >= tokens.length()) {
      startIndex
    } else if (tokens(startIndex).get.isInstanceOf[IdToken]) {
      res.add(tokenToId(tokens(startIndex).get))
      startIndex + 1
    } else {
      startIndex
    }
  }

  private def parseTypeName(startIndex: Int, tokens: Tokens, res: TypeNameAssignable): Int = {
    tokens(startIndex) match {
      case Some(id: IdToken) =>
        val tn = new TypeName(tokenToId(id))
        res.add(tn)
        val nextIndex = parseTypeArguments(startIndex + 1, tokens, tn)
        nextIndex
      case _ => startIndex
    }
  }

  private def parseTypeArguments(
    startIndex: Int,
    tokens: Tokens,
    res: TypeArgumentsAssignable
  ): Int = {
    if (startIndex >= tokens.length()) return startIndex
    if (!tokens(startIndex).get.matches(Tokens.LessThanStr)) return startIndex

    val ta = new TypeArguments

    var index = startIndex
    index = parseTypeList(index + 1, tokens, ta)
    if (ta.typeList.isDefined) res.add(ta)
    if (!tokens(index).exists(_.matches(Tokens.GreaterThanStr))) throw new Exception("Missing >")
    index + 1
  }

  private def parseTypeList(startIndex: Int, tokens: Tokens, res: TypeListAssignable): Int = {
    val typeList = new TypeList
    var index    = parseTypeRef(startIndex, tokens, typeList)
    while (index < tokens.length() && tokens(index).get.matches(Tokens.CommaStr)) {
      index = parseTypeRef(index + 1, tokens, typeList)
    }
    if (typeList.typeRefs.nonEmpty) res.add(typeList)
    index
  }

  private def parseTypeRef(startIndex: Int, tokens: Tokens, res: TypeRefAssignable): Int = {

    val typeRef = new TypeRef
    res.add(typeRef)

    var index = parseTypeName(startIndex, tokens, typeRef)

    while (index < tokens.length() && tokens(index).get.matches(Tokens.DotStr)) {
      index = parseTypeName(index + 1, tokens, typeRef)
    }

    parseArraySubscripts(index, tokens, typeRef)
  }

  private def parseArraySubscripts(
    startIndex: Int,
    tokens: Tokens,
    res: ArraySubscriptsAssignable
  ): Int = {
    var index = startIndex
    while (index < tokens.length() && tokens(index).get.matches(Tokens.LBrackStr)) {
      if (tokens(index + 1).exists(!_.matches(Tokens.RBrackStr))) {
        throw new Exception(s"Missing '${Tokens.RBrackStr}'")
      }
      res.add(ArraySubscripts())
      index += 2
    }
    index
  }

  private def parseAnnotation(startIndex: Int, tokens: Tokens, res: AnnotationAssignable): Int = {
    if (!tokens(startIndex).exists(_.matches(Tokens.AtSignStr))) return startIndex

    val qName = new QualifiedName

    var index = parseId(startIndex + 1, tokens, qName)

    while (index < tokens.length() && tokens(index).get.matches(Tokens.DotStr)) {
      index = parseId(index + 1, tokens, qName)
    }

    val parameters = if (tokens(index).exists(_.matches(Tokens.LParenStr))) {
      val builder      = new StringBuilder()
      var nestingCount = 1
      //builder.append(tokens(index).get.contents)
      index += 1
      while (nestingCount > 0 && index < tokens.length()) {
        if (tokens(index).get.matches(Tokens.RParenStr)) {
          nestingCount -= 1
        } else if (tokens(index).get.matches(Tokens.LParenStr)) {
          nestingCount += 1
        }
        if (nestingCount > 0) builder.append(tokens(index).get.contents)
        index += 1
      }
      Some(builder.toString())
    } else None

    res.add(Annotation(qName, parameters))

    index
  }

  private val modifierTokenStrs = Set(
    Tokens.GlobalStr,
    Tokens.PublicStr,
    Tokens.ProtectedStr,
    Tokens.PrivateStr,
    Tokens.TransientStr,
    Tokens.StaticStr,
    Tokens.AbstractStr,
    Tokens.FinalStr,
    Tokens.WebserviceStr,
    Tokens.OverrideStr,
    Tokens.VirtualStr,
    Tokens.TestMethodStr
  )

  private val sharingModifiers = Set(Tokens.WithStr, Tokens.WithoutStr, Tokens.InheritedStr)

  private def parseModifiers(startIndex: Int, tokens: Tokens, res: ModifierAssignable): Int = {
    var index    = startIndex
    var continue = true
    while (continue && index < tokens.length()) {
      if (tokens(index).get.matches(Tokens.AtSignStr)) {
        index = parseAnnotation(index, tokens, res)
      } else if (modifierTokenStrs.contains(tokens(index).get.contents.toLowerCase)) {
        res.add(tokenToModifier(tokens(index).get))
        index += 1
      } else if (
        sharingModifiers.contains(tokens(index).get.contents.toLowerCase())
        && tokens(index + 1).exists(_.matches(Tokens.SharingStr))
      ) {
        // Combine to make sharing modifier
        res.add(
          Modifier(
            IdToken(
              s"${tokens(index).get.contents} ${tokens(index + 1).get.contents}",
              Location.from(tokens(index).get.location, tokens(index + 1).get.location)
            )
          )
        )
        index += 2
      } else {
        continue = false
      }
    }
    index
  }

  private def parseFormalParameterList(
    startIndex: Int,
    tokens: Tokens,
    res: FormalParameterList
  ): Int = {
    if (!tokens(startIndex).exists(_.matches(Tokens.LParenStr))) return startIndex

    var index        = startIndex + 1
    var indexAtStart = startIndex
    while (
      indexAtStart != index && index < tokens
        .length() && !tokens(index).get.matches(Tokens.RParenStr)
    ) {
      indexAtStart = index
      val formalParameter = new FormalParameter
      index = parseModifiers(index, tokens, formalParameter)
      index = parseTypeRef(index, tokens, formalParameter)
      index = parseId(index, tokens, formalParameter)
      if (tokens(index).exists(_.matches(Tokens.CommaStr))) index += 1
      res.add(formalParameter)
    }
    if (tokens(index).exists(_.matches(Tokens.RParenStr))) index += 1
    index
  }
}

object Compare {

  def compareClassTypeDeclarations(
    first: ClassTypeDeclaration,
    second: ClassTypeDeclaration
  ): Unit = {

    def innerClassTypeDeclarations(o: ClassTypeDeclaration): Seq[ClassTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[ClassTypeDeclaration])
        .map(_.asInstanceOf[ClassTypeDeclaration])
        .toIndexedSeq
    }

    def innerInterfaceTypeDeclarations(o: ClassTypeDeclaration): Seq[InterfaceTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[InterfaceTypeDeclaration])
        .map(_.asInstanceOf[InterfaceTypeDeclaration])
        .toIndexedSeq
    }

    def innerEnumTypeDeclarations(o: ClassTypeDeclaration): Seq[EnumTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[EnumTypeDeclaration])
        .map(_.asInstanceOf[EnumTypeDeclaration])
        .toIndexedSeq
    }

    def compareInnerClasses(
      fst: Seq[ClassTypeDeclaration],
      snd: Seq[ClassTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner class not found ${f.id} in ${first.id}")
        }
        compareClassTypeDeclarations(f, sOpt.get)
      })
    }

    def compareInnerInterfaces(
      fst: Seq[InterfaceTypeDeclaration],
      snd: Seq[InterfaceTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner interface not found ${f.id} in ${first.id}")
        }
        compareInterfaceTypeDeclarations(f, sOpt.get)
      })
    }

    def compareInnerEnums(fst: Seq[EnumTypeDeclaration], snd: Seq[EnumTypeDeclaration]): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner enum not found ${f.id} in ${first.id}")
        }
        compareEnumTypeDeclarations(f, sOpt.get)
      })
    }

    if (first._annotations != second._annotations) {
      throw new Exception(s"Different annotation ${first._annotations} != ${second._annotations}")
    }

    if (first._modifiers != second._modifiers) {
      throw new Exception(s"Different modifiers ${first._modifiers} != ${second._modifiers}")
    }

    if (first.id != second.id) {
      throw new Exception(s"Different or empty class id ${first.id} != ${second.id}")
    }

    if (first._extendsTypeRef != second._extendsTypeRef) {
      throw new Exception(s"Different extends ${first._extendsTypeRef} != ${second._extendsTypeRef}")
    }

    if (first._implementsTypeList != second._implementsTypeList) {
      throw new Exception(
        s"Different implements ${first._implementsTypeList} != ${second._implementsTypeList}"
      )
    }

    if (first._initializers.length != second._initializers.length) {
      throw new Exception(s"Different initializers ${first._initializers} != ${second._initializers}")
    }

    if (first._constructors != second._constructors) {
      throw new Exception(s"Different constructors ${first._constructors} != ${second._constructors}")
    }

    if (first._methods != second._methods) {
      throw new Exception(s"Different methods ${first._methods} != ${second._methods}")
    }

    if (first._properties != second._properties) {
      throw new Exception(s"Different properties ${first._properties} != ${second._properties}")
    }

    if (first._fields.toSet != second._fields.toSet) {
      throw new Exception(s"Different fields ${first._fields} != ${second._fields}")
    }

    compareInnerClasses(innerClassTypeDeclarations(first), innerClassTypeDeclarations(second))
    compareInnerClasses(innerClassTypeDeclarations(second), innerClassTypeDeclarations(first))

    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(first),
      innerInterfaceTypeDeclarations(second)
    )
    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(second),
      innerInterfaceTypeDeclarations(first)
    )

    compareInnerEnums(innerEnumTypeDeclarations(first), innerEnumTypeDeclarations(second))
    compareInnerEnums(innerEnumTypeDeclarations(second), innerEnumTypeDeclarations(first))
  }

  def compareInterfaceTypeDeclarations(
    first: InterfaceTypeDeclaration,
    second: InterfaceTypeDeclaration
  ): Unit = {

    if (first.annotations != second.annotations) {
      throw new Exception(s"Different annotation ${first.annotations} != ${second.annotations}")
    }

    if (first.modifiers != second.modifiers) {
      throw new Exception(s"Different modifiers ${first.modifiers} != ${second.modifiers}")
    }

    if (first.id != second.id) {
      throw new Exception(s"Different or empty interface id ${first.id} != ${second.id}")
    }

    if (first._implementsTypeList != second._implementsTypeList) {
      throw new Exception(
        s"Different extends ${first._implementsTypeList} != ${second._implementsTypeList}"
      )
    }

    if (first.methods != second.methods) {
      throw new Exception(s"Different methods ${first.methods} != ${second.methods}")
    }
  }

  def compareEnumTypeDeclarations(first: EnumTypeDeclaration, second: EnumTypeDeclaration): Unit = {

    if (first.annotations != second.annotations) {
      throw new Exception(s"Different annotation ${first.annotations} != ${second.annotations}")
    }

    if (first.modifiers != second.modifiers) {
      throw new Exception(s"Different modifiers ${first.modifiers} != ${second.modifiers}")
    }

    if (first.id != second.id) {
      throw new Exception(s"Different or empty enum id ${first.id} != ${second.id}")
    }

    if (first.constants.toSet != second.constants.toSet) {
      throw new Exception(s"Different constants ${first.constants} != ${second.constants}")
    }
  }
}
