/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object StringUtils {

  def asString[T](o: Option[T]): String = {
    o match {
      case None    => ""
      case Some(o) => s"${o.toString}"
    }
  }

  def asString[T](a: mutable.ArrayBuffer[T]): String = {
    asString(a, " ")
  }

  def asString[T](a: mutable.ArrayBuffer[T], separator: String): String = {
    a.map(_.toString).mkString(separator)
  }

  def asString[T](a: ArraySeq[T]): String = {
    asString(a, " ")
  }

  def asString[T](a: ArraySeq[T], separator: String): String = {
    a.map(_.toString).mkString(separator)
  }

  def asAnnotationAndModifierString(a: ArraySeq[Annotation], m: ArraySeq[Modifier]): String = {
    val mod        = if (m.nonEmpty) s"${m.mkString(" ")}" else ""
    val annotation = if (a.nonEmpty) s"${a.mkString(" ")} " else ""
    s"$annotation$mod"
  }

  def asSignatureString(a: Signature): String = {
    s"${asAnnotationAndModifierString(a.annotations, a.modifiers)} ${a.typeRef.getFullName} ${a.id}"
  }

  def asMethodSignatureString(a: MethodDeclaration): String = {
    val withVoid = if (a.typeRef.isEmpty) Tokens.VoidStr else a.typeRef.get.getFullName
    s"${asAnnotationAndModifierString(a.annotations, a.modifiers)} $withVoid ${a.id}(${a.formalParameterList})"
  }

  def asConstructorSignatureString(a: ConstructorDeclaration): String = {
    s"${asAnnotationAndModifierString(a.annotations, a.modifiers)} ${a.qName}(${a.formalParameterList})"
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
  def add(tr: UnresolvedTypeRef): Unit
}

trait TypeNameSegmentAssignable {
  def add(tn: TypeNameSegment): Unit
}

trait TypeListAssignable {
  def add(tl: TypeList): Unit
}

trait TypeArgumentsAssignable {
  def add(ta: TypeArguments): Unit
}

trait ArraySubscriptsAssignable {
  def addArraySubscript(): Unit
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
  var typeRef: TypeRef
  val id: Id
  val annotations: ArraySeq[Annotation]
  val modifiers: ArraySeq[Modifier]
}

class MemberDeclaration extends ModifierAssignable with TypeRefAssignable {

  val annotations: mutable.ArrayBuffer[Annotation] = mutable.ArrayBuffer[Annotation]()
  val modifiers: mutable.ArrayBuffer[Modifier]     = mutable.ArrayBuffer[Modifier]()
  var typeRef: Option[UnresolvedTypeRef]           = None

  override def add(a: Annotation): Unit = annotations.append(a)

  override def add(m: Modifier): Unit = modifiers.append(m)

  override def add(tr: UnresolvedTypeRef): Unit = typeRef = Some(tr)
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
object Modifiers {
  final val PUBLIC_MODIFIER: Modifier   = Modifier(IdToken(Tokens.PublicStr, Location.default))
  final val STATIC_MODIFIER: Modifier   = Modifier(IdToken(Tokens.StaticStr, Location.default))
  final val VIRTUAL_MODIFIER: Modifier  = Modifier(IdToken(Tokens.VirtualStr, Location.default))
  final val ABSTRACT_MODIFIER: Modifier = Modifier(IdToken(Tokens.AbstractStr, Location.default))
  final val FINAL_MODIFIER: Modifier    = Modifier(IdToken(Tokens.FinalStr, Location.default))
}

case class Id(id: IdToken) {
  override def toString: String = id.contents

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Id]
    id.lowerCaseContents.equalsIgnoreCase(other.id.lowerCaseContents)
  }

  override val hashCode: Int = id.lowerCaseContents.hashCode
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

class FormalParameter extends ModifierAssignable with TypeRefAssignable with IdAssignable {

  val annotations: mutable.ArrayBuffer[Annotation] = mutable.ArrayBuffer[Annotation]()
  val modifiers: mutable.ArrayBuffer[Modifier]     = mutable.ArrayBuffer[Modifier]()
  var typeRef: Option[TypeRef]                     = None
  var id: Option[Id]                               = None

  override def add(a: Annotation): Unit = annotations.append(a)

  override def add(m: Modifier): Unit = modifiers.append(m)

  override def add(tr: UnresolvedTypeRef): Unit = typeRef = Some(tr)

  override def add(i: Id): Unit = id = Some(i)

  override def toString: String = {
    import StringUtils._
    s"${asAnnotationAndModifierString(
      ArraySeq.unsafeWrapArray(annotations.toArray),
      ArraySeq.unsafeWrapArray(modifiers.toArray)
    )}${asString(typeRef)} ${asString(id)}"
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[FormalParameter]

    other.annotations == annotations &&
    other.modifiers == modifiers &&
    other.typeRef
      .map(_.getFullName)
      .getOrElse("")
      .equalsIgnoreCase(typeRef.map(_.getFullName).getOrElse("")) &&
    other.id == id
  }
}

class FormalParameterList extends FormalParameterAssignable {
  val formalParameters: mutable.ArrayBuffer[FormalParameter] =
    mutable.ArrayBuffer[FormalParameter]()

  override def add(fp: FormalParameter): Unit = formalParameters.append(fp)

  override def toString: String = {
    import StringUtils._
    asString(formalParameters, ", ")
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
  annotations: ArraySeq[Annotation],
  modifiers: ArraySeq[Modifier],
  qName: QualifiedName,
  formalParameterList: FormalParameterList
) extends BodyDeclaration {

  val id: Id                          = qName.qName(0)
  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[ConstructorDeclaration]
    other.annotations == annotations &&
    other.modifiers == modifiers &&
    other.qName == qName &&
    other.formalParameterList == formalParameterList
  }

  override def toString: String = {
    import StringUtils._
    s"${qName.location} ${asAnnotationAndModifierString(annotations, modifiers)} $qName $formalParameterList"
  }
}

object ConstructorDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[ConstructorDeclaration] =
    mutable.ArrayBuffer[ConstructorDeclaration]()
}

case class MethodDeclaration(
  annotations: ArraySeq[Annotation],
  modifiers: ArraySeq[Modifier],
  var typeRef: Option[TypeRef],
  id: Id,
  formalParameterList: FormalParameterList
) extends BodyDeclaration {

  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[MethodDeclaration]
    other.annotations == annotations &&
    other.modifiers == modifiers &&
    other.typeRef.nonEmpty && typeRef.nonEmpty &&
    other.typeRef.get.getFullName.equalsIgnoreCase(typeRef.get.getFullName) &&
    other.id == id &&
    other.formalParameterList == formalParameterList
  }

  override def toString: String = {
    import StringUtils._
    s"${id.id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typeRef $id $formalParameterList"
  }
}

object MethodDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[MethodDeclaration] =
    mutable.ArrayBuffer[MethodDeclaration]()

  def apply(
    annotations: ArraySeq[Annotation],
    modifiers: ArraySeq[Modifier],
    typeRef: Option[TypeRef],
    id: Id,
    formalParameterList: FormalParameterList
  ): MethodDeclaration = {
    new MethodDeclaration(annotations, modifiers, typeRef, id, formalParameterList)
  }

  def apply(
    annotations: ArraySeq[Annotation],
    modifiers: ArraySeq[Modifier],
    typeRef: TypeRef,
    id: Id,
    formalParameterList: FormalParameterList
  ): MethodDeclaration = {
    new MethodDeclaration(annotations, modifiers, Some(typeRef), id, formalParameterList)
  }
}

class PropertyDeclaration(
  val annotations: ArraySeq[Annotation],
  val modifiers: ArraySeq[Modifier],
  var typeRef: TypeRef,
  val id: Id
) extends BodyDeclaration
    with Signature
    with PropertyBlockAssignable {

  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  val propertyBlocks: mutable.ArrayBuffer[PropertyBlock] = mutable.ArrayBuffer[PropertyBlock]()

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[PropertyDeclaration]
    other.annotations == annotations &&
    other.modifiers == modifiers &&
    other.typeRef.getFullName.equalsIgnoreCase(typeRef.getFullName) &&
    other.id == id
  }

  override def toString: String = {
    import StringUtils._
    s"${id.id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typeRef $id"
  }

  override def add(pb: PropertyBlock): Unit = propertyBlocks.append(pb)
}

object PropertyDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[PropertyDeclaration] =
    mutable.ArrayBuffer[PropertyDeclaration]()
}

case class FieldDeclaration(
  annotations: ArraySeq[Annotation],
  modifiers: ArraySeq[Modifier],
  var typeRef: TypeRef,
  id: Id
) extends BodyDeclaration
    with Signature {

  var location: Option[Location]      = None
  var blockLocation: Option[Location] = None

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[FieldDeclaration]
    other.annotations == annotations &&
    other.modifiers == modifiers &&
    other.typeRef.getFullName.equalsIgnoreCase(typeRef.getFullName) &&
    other.id == id
  }

  override def toString: String = {
    import StringUtils._
    s"${id.id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typeRef $id"
  }
}

object FieldDeclaration {
  final val emptyArrayBuffer: mutable.ArrayBuffer[FieldDeclaration] =
    mutable.ArrayBuffer[FieldDeclaration]()
}

object Initializer {
  val id: Id = Id(IdToken("initializer", Location.default))
}

case class Initializer(isStatic: Boolean) extends BodyDeclaration {
  override val id: Id                          = Initializer.id
  override var location: Option[Location]      = None
  override var blockLocation: Option[Location] = None
}

object Parse {

  def parseClassType(ctd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
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

  def parseInterfaceType(itd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
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

  def parseEnumType(etd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
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
    ctd: IMutableTypeDeclaration,
    tokens: Tokens,
    nextToken: Token
  ): (Boolean, Seq[BodyDeclaration]) = {

    if (tokens.isEmpty()) {
      return (true, addInitializer(ctd, isStatic = false).toSeq)
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

  def parseInterfaceMember(itd: IMutableTypeDeclaration, tokens: Tokens): Seq[BodyDeclaration] = {
    if (tokens.isEmpty()) return Seq.empty

    val md    = new MemberDeclaration
    var index = parseModifiers(0, tokens, md)
    index = parseTypeRef(index, tokens, md)
    addMethod(index, tokens, md, itd).toSeq
  }

  def parseEnumMember(etd: IMutableTypeDeclaration, tokens: Tokens): Seq[Id] = {
    if (tokens.isEmpty()) return Seq.empty

    val constant = tokenToId(tokens(0).get)
    val field = new FieldDeclaration(
      ArraySeq(),
      ArraySeq(Modifier(IdToken(Tokens.StaticStr, constant.id.location))),
      etd,
      constant
    )
    etd.appendField(field)
    Seq(constant)
  }

  def parsePropertyBlock(propertyDeclaration: PropertyDeclaration): Option[PropertyBlock] = {
    val pb = new PropertyBlock
    propertyDeclaration.add(pb)
    Some(pb)
  }

  private def toQualifiedName(tr: UnresolvedTypeRef): QualifiedName = {
    val qName = new QualifiedName
    tr.typeNameSegments.foreach(tn => qName.add(tn.id))
    qName
  }

  private def addInitializer(
    ctd: IMutableTypeDeclaration,
    isStatic: Boolean
  ): Option[Initializer] = {
    val initializer = Initializer(isStatic)
    ctd.add(initializer)
    Some(initializer)
  }

  private def addConstructor(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    ctd: IMutableTypeDeclaration
  ): Option[ConstructorDeclaration] = {

    val formalParameterList = new FormalParameterList
    val index               = parseFormalParameterList(startIndex, tokens, formalParameterList)
    if (index < tokens.length()) {
      throw new Exception(s"Unrecognised constructor ${tokens.toString()}")
    }
    val constructor = ConstructorDeclaration(
      ArraySeq.unsafeWrapArray(md.annotations.toArray),
      ArraySeq.unsafeWrapArray(md.modifiers.toArray),
      toQualifiedName(md.typeRef.get),
      formalParameterList
    )
    ctd.appendConstructor(constructor)
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
      MethodDeclaration(
        ArraySeq.unsafeWrapArray(md.annotations.toArray),
        ArraySeq.unsafeWrapArray(md.modifiers.toArray),
        md.typeRef.get,
        id,
        formalParameterList
      )
    res.add(method)
    Some(method)
  }

  private def addProperty(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    ctd: IMutableTypeDeclaration
  ): Option[PropertyDeclaration] = {

    val id    = tokenToId(tokens(startIndex).get)
    val index = startIndex + 1

    if (index < tokens.length()) {
      throw new Exception(s"Unrecognised property ${tokens.toString()}")
    }

    val property =
      new PropertyDeclaration(
        ArraySeq.unsafeWrapArray(md.annotations.toArray),
        ArraySeq.unsafeWrapArray(md.modifiers.toArray),
        md.typeRef.get,
        id
      )
    ctd.appendProperty(property)
    Some(property)
  }

  private def addFields(
    startIndex: Int,
    tokens: Tokens,
    md: MemberDeclaration,
    ctd: IMutableTypeDeclaration,
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

      val field = FieldDeclaration(
        ArraySeq.unsafeWrapArray(md.annotations.toArray),
        ArraySeq.unsafeWrapArray(md.modifiers.toArray),
        md.typeRef.get,
        id
      )
      ctd.appendField(field)
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

  private def parseTypeName(
    startIndex: Int,
    tokens: Tokens,
    res: TypeNameSegmentAssignable
  ): Int = {
    tokens(startIndex) match {
      case Some(id: IdToken) =>
        val tn = new TypeNameSegment(tokenToId(id))
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

    val typeRef = new UnresolvedTypeRef
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
      res.addArraySubscript()
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
