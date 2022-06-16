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

trait TypeRefAssignable {
  def add(tr: UnresolvedTypeRef): Unit
}

trait TypeListAssignable {
  def add(tl: TypeList): Unit
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

class MemberDeclaration extends TypeRefAssignable {

  var modifiers: ArraySeq[Modifier]      = Modifiers.emptyArraySeq
  var annotations: ArraySeq[Annotation]  = Annotations.emptyArraySeq
  var typeRef: Option[UnresolvedTypeRef] = None

  def setModifiers(modifiers: ArraySeq[Modifier]): Unit       = this.modifiers = modifiers
  def setAnnotations(annotations: ArraySeq[Annotation]): Unit = this.annotations = annotations

  override def add(tr: UnresolvedTypeRef): Unit = typeRef = Some(tr)
}

case class Annotation(qName: QualifiedName, parameters: Option[String]) {
  override def toString: String = {
    if (parameters.isDefined) s"@$qName(${parameters.get})" else s"@$qName"
  }
}

object Annotations {
  final val emptyArraySeq = ArraySeq[Annotation]()
}

case class Modifier(text: String) {
  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Modifier]
    text.equalsIgnoreCase(other.text)
  }

  override def toString: String = text
}

object Modifiers {
  final val emptyArraySeq               = ArraySeq[Modifier]()
  final val PUBLIC_MODIFIER: Modifier   = Modifier(Tokens.PublicStr)
  final val STATIC_MODIFIER: Modifier   = Modifier(Tokens.StaticStr)
  final val VIRTUAL_MODIFIER: Modifier  = Modifier(Tokens.VirtualStr)
  final val ABSTRACT_MODIFIER: Modifier = Modifier(Tokens.AbstractStr)
  final val FINAL_MODIFIER: Modifier    = Modifier(Tokens.FinalStr)
}

case class Id(id: IdToken) {
  override def toString: String = id.contents

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Id]
    id.lowerCaseContents.equals(other.id.lowerCaseContents)
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

class FormalParameter extends TypeRefAssignable with IdAssignable {

  var annotations: ArraySeq[Annotation] = Annotations.emptyArraySeq
  var modifiers: ArraySeq[Modifier]     = Modifiers.emptyArraySeq
  var typeRef: Option[TypeRef]          = None
  var id: Option[Id]                    = None

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

  override def add(tr: UnresolvedTypeRef): Unit = typeRef = Some(tr)

  override def add(i: Id): Unit = id = Some(i)

  def setModifiers(modifiers: ArraySeq[Modifier]): Unit = this.modifiers = modifiers

  def setAnnotations(annotations: ArraySeq[Annotation]): Unit = this.annotations = annotations

  override def toString: String = {
    import StringUtils._
    s"${asAnnotationAndModifierString(
      ArraySeq.unsafeWrapArray(annotations.toArray),
      ArraySeq.unsafeWrapArray(modifiers.toArray)
    )}${typeRef.get.getFullName} ${asString(id)}"
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

final case class FormalParameterList(formalParameters: ArraySeq[FormalParameter]) {
  override def toString: String = {
    import StringUtils._
    asString(formalParameters, ", ")
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[FormalParameterList]
    other.formalParameters == formalParameters
  }
}

object FormalParameterList {
  final val empty = FormalParameterList(ArraySeq())
}

class PropertyBlock {
  var blockLocation: Option[Location] = None
}

sealed trait BodyDeclaration {
  val id: Id

  // These are inlined to save memory, block location is optional
  private var startLine: Int       = _
  private var startLineOffset: Int = _
  private var startByteOffset: Int = _
  private var endLine: Int         = _
  private var endLineOffset: Int   = _
  private var endByteOffset: Int   = _

  private var startBlockLine: Int       = -1
  private var startBlockLineOffset: Int = _
  private var startBlockByteOffset: Int = _
  private var endBlockLine: Int         = _
  private var endBlockLineOffset: Int   = _
  private var endBlockByteOffset: Int   = _

  def location: Option[Location] = {
    endByteOffset match {
      case 0 => None
      case _ =>
        Some(
          Location(
            startLine,
            startLineOffset,
            startByteOffset,
            endLine,
            endLineOffset,
            endByteOffset
          )
        )
    }
  }
  def blockLocation: Option[Location] = {
    startBlockLine match {
      case -1 => None
      case _ =>
        Some(
          Location(
            startBlockLine,
            startBlockLineOffset,
            startBlockByteOffset,
            endBlockLine,
            endBlockLineOffset,
            endBlockByteOffset
          )
        )
    }
  }

  def setLocation(startPosition: Position, endPosition: Position): Unit = {
    startLine = startPosition.line
    startLineOffset = startPosition.lineOffset
    startByteOffset = startPosition.byteOffset
    endLine = endPosition.line
    endLineOffset = endPosition.lineOffset
    endByteOffset = endPosition.byteOffset
  }

  def setBlockLocation(startPosition: Position, endPosition: Position): Unit = {
    startBlockLine = startPosition.line
    startBlockLineOffset = startPosition.lineOffset
    startBlockByteOffset = startPosition.byteOffset
    endBlockLine = endPosition.line
    endBlockLineOffset = endPosition.lineOffset
    endBlockByteOffset = endPosition.byteOffset
  }

  /** Set both location && blockLocation, with same endpoint */
  def setLocations(
    startPosition: Position,
    blockStartPosition: Position,
    endPosition: Position
  ): Unit = {
    startLine = startPosition.line
    startLineOffset = startPosition.lineOffset
    startByteOffset = startPosition.byteOffset
    endLine = endPosition.line
    endLineOffset = endPosition.lineOffset
    endByteOffset = endPosition.byteOffset

    startBlockLine = blockStartPosition.line
    startBlockLineOffset = blockStartPosition.lineOffset
    startBlockByteOffset = blockStartPosition.byteOffset
    endBlockLine = endPosition.line
    endBlockLineOffset = endPosition.lineOffset
    endBlockByteOffset = endPosition.byteOffset
  }
}

case class ConstructorDeclaration(
  annotations: ArraySeq[Annotation],
  modifiers: ArraySeq[Modifier],
  qName: QualifiedName,
  formalParameterList: FormalParameterList
) extends BodyDeclaration {

  val id: Id = qName.qName(0)

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

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

  def annotationsAndModifiers: String = (annotations ++ modifiers).mkString(" ")

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
    val typRefString = typeRef match {
      case Some(tr) => tr.getFullName
      case _        => ""
    }
    s"${id.id.location} ${asAnnotationAndModifierString(annotations, modifiers)} $typRefString $id $formalParameterList"
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
  override val id: Id = Initializer.id
}

object Parse {

  def parseClassType(ctd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
    var (index, modifiers, annotations) = parseModifiersAndAnnotations(0, tokens)
    ctd.setModifiers(modifiers)
    ctd.setAnnotations(annotations)

    if (!tokens.matches(index, Tokens.ClassStr))
      throw new Exception(s"Missing '${Tokens.ClassStr}'")
    index += 1
    index = parseId(index, tokens, ctd)

    index = tokens.findIndex(t => t.matches(Tokens.ExtendsStr))
    if (index != -1) {
      val (newIndex, typeRef) = parseTypeRef(index + 1, tokens)
      index = newIndex
      ctd.add(typeRef)
    }

    index = tokens.findIndex(t => t.matches(Tokens.ImplementsStr))
    if (index != -1) {
      val (newIndex, typeList) = parseTypeList(index + 1, tokens)
      ctd.add(typeList)
    }
    ctd
  }

  def parseInterfaceType(itd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
    var (index, modifiers, annotations) = parseModifiersAndAnnotations(0, tokens)
    itd.setModifiers(modifiers)
    itd.setAnnotations(annotations)

    if (!tokens.matches(index, Tokens.InterfaceStr))
      throw new Exception(s"Missing '${Tokens.InterfaceStr}'")
    index += 1
    index = parseId(index, tokens, itd)

    index = tokens.findIndex(t => t.matches(Tokens.ExtendsStr))
    if (index != -1) {
      val (newIndex, typeList) = parseTypeList(index + 1, tokens)
      itd.add(typeList)
    }

    itd
  }

  def parseEnumType(etd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
    var (index, modifiers, annotations) = parseModifiersAndAnnotations(0, tokens)
    etd.setModifiers(modifiers)
    etd.setAnnotations(annotations)

    if (!tokens.matches(index, Tokens.EnumStr))
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

    if (tokens.isEmpty) {
      return (true, addInitializer(ctd, isStatic = false).toSeq)
    }

    val md                              = new MemberDeclaration
    var (index, modifiers, annotations) = parseModifiersAndAnnotations(0, tokens)
    md.setModifiers(modifiers)
    md.setAnnotations(annotations)

    if (index >= tokens.length) {
      val isStatic = tokens.hasToken(Set(Tokens.StaticStr)).isDefined
      return (true, addInitializer(ctd, isStatic).toSeq)
    }

    val (newIndex, typeRef) = parseTypeRef(index, tokens)
    index = newIndex
    md.add(typeRef)

    if (tokens.matches(index, Tokens.LParenStr)) {
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
    if (tokens.isEmpty) return Seq.empty

    val md                              = new MemberDeclaration
    var (index, modifiers, annotations) = parseModifiersAndAnnotations(0, tokens)
    md.setModifiers(modifiers)
    md.setAnnotations(annotations)
    val (newIndex, typeRef) = parseTypeRef(index, tokens)
    index = newIndex
    md.add(typeRef)
    addMethod(index, tokens, md, itd).toSeq
  }

  def parseEnumMember(etd: IMutableTypeDeclaration, tokens: Tokens): Seq[Id] = {
    if (tokens.isEmpty) return Seq.empty

    val constant = tokenToId(tokens.head)
    val field =
      new FieldDeclaration(ArraySeq(), ArraySeq(Modifier(Tokens.StaticStr)), etd, constant)
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

    val (index, formalParameterList) = parseFormalParameterList(startIndex, tokens)
    if (index < tokens.length || formalParameterList.isEmpty) {
      throw new Exception(s"Unrecognised constructor ${tokens.toString()}")
    }

    val constructor = ConstructorDeclaration(
      ArraySeq.unsafeWrapArray(md.annotations.toArray),
      ArraySeq.unsafeWrapArray(md.modifiers.toArray),
      toQualifiedName(md.typeRef.get),
      formalParameterList.get
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

    val id                           = tokenToId(tokens.get(startIndex))
    val (index, formalParameterList) = parseFormalParameterList(startIndex + 1, tokens)
    if (index < tokens.length || formalParameterList.isEmpty) {
      throw new Exception(s"Unrecognised method ${tokens.toString()}")
    }
    val method =
      MethodDeclaration(
        ArraySeq.unsafeWrapArray(md.annotations.toArray),
        ArraySeq.unsafeWrapArray(md.modifiers.toArray),
        md.typeRef.get,
        id,
        formalParameterList.get
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

    val id    = tokenToId(tokens.get(startIndex))
    val index = startIndex + 1

    if (index < tokens.length) {
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
      while (!found && index < tokens.length) {
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
    while (index < tokens.length) {
      val id = tokenToId(tokens.get(index))

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

      if (startLocation.isDefined) {
        // WARNING: I don't think there is a code path where this is not overwritten
        field.setBlockLocation(
          Position(
            startLocation.get.startLine,
            startLocation.get.startLineOffset,
            startLocation.get.endByteOffset + 1
          ),
          Position(endLocation.startLine, endLocation.startLineOffset, endLocation.startByteOffset)
        )
      }
    }
    fields.toSeq
  }

  private def tokenToModifier(token: Token): Modifier = Modifier(token.contents)

  private def tokenToId(token: Token): Id = Id(IdToken(token.contents, token.location))

  private def parseId(startIndex: Int, tokens: Tokens, res: IdAssignable): Int = {
    if (startIndex >= tokens.length) {
      startIndex
    } else if (tokens.get(startIndex).isInstanceOf[IdToken]) {
      res.add(tokenToId(tokens.get(startIndex)))
      startIndex + 1
    } else {
      startIndex
    }
  }

  private def parseTypeNameSegment(
    startIndex: Int,
    tokens: Tokens
  ): (Int, Option[TypeNameSegment]) = {
    tokens(startIndex) match {
      case Some(token: IdToken) =>
        val id                         = tokenToId(token)
        val (nextIndex, typeArguments) = parseTypeArguments(startIndex + 1, tokens)
        (nextIndex, Some(TypeNameSegment(id, typeArguments)))
      case _ => (startIndex, None)
    }
  }

  private def parseTypeArguments(startIndex: Int, tokens: Tokens): (Int, TypeArguments) = {
    if (startIndex >= tokens.length) return (startIndex, TypeArguments.empty)
    if (!tokens.get(startIndex).matches(Tokens.LessThanStr))
      return (startIndex, TypeArguments.empty)

    val (index, typeList) = parseTypeList(startIndex + 1, tokens)
    if (!tokens(index).exists(_.matches(Tokens.GreaterThanStr))) throw new Exception("Missing >")
    (index + 1, TypeArguments(typeList))
  }

  private def parseTypeList(startIndex: Int, tokens: Tokens): (Int, TypeList) = {
    val typeRefs         = mutable.ArrayBuffer[TypeRef]()
    var (index, typeRef) = parseTypeRef(startIndex, tokens)
    typeRefs.append(typeRef)
    while (index < tokens.length && tokens.get(index).matches(Tokens.CommaStr)) {
      val (newIndex, typeRef) = parseTypeRef(index + 1, tokens)
      index = newIndex
      typeRefs.append(typeRef)
    }
    if (typeRefs.nonEmpty)
      (index, TypeList(ArraySeq.unsafeWrapArray(typeRefs.toArray)))
    else
      (index, TypeList.empty)
  }

  private def parseTypeRef(startIndex: Int, tokens: Tokens): (Int, UnresolvedTypeRef) = {

    val typeRef                  = new UnresolvedTypeRef
    var (index, typeNameSegment) = parseTypeNameSegment(startIndex, tokens)
    if (typeNameSegment.isEmpty) {
      throw new Exception(s"Missing Identifier")
    }
    typeRef.typeNameSegments.append(typeNameSegment.get)

    while (index < tokens.length && tokens.get(index).matches(Tokens.DotStr)) {
      val (newIndex, typeNameSegment) = parseTypeNameSegment(index + 1, tokens)
      index = newIndex

      if (typeNameSegment.isEmpty) {
        throw new Exception(s"Missing Identifier")
      }
      typeRef.typeNameSegments.append(typeNameSegment.get)
    }

    val (newIndex, count) = parseArraySubscripts(index, tokens)
    typeRef.arraySubscripts = count
    (newIndex, typeRef)
  }

  private def parseArraySubscripts(startIndex: Int, tokens: Tokens): (Int, Int) = {
    var index = startIndex
    var count = 0
    while (index < tokens.length && tokens.get(index).matches(Tokens.LBrackStr)) {
      if (tokens(index + 1).exists(!_.matches(Tokens.RBrackStr))) {
        throw new Exception(s"Missing '${Tokens.RBrackStr}'")
      }
      count += 1
      index += 2
    }
    (index, count)
  }

  private def parseAnnotation(
    startIndex: Int,
    tokens: Tokens,
    accum: mutable.ArrayBuffer[Annotation]
  ): Int = {
    if (!tokens(startIndex).exists(_.matches(Tokens.AtSignStr))) return startIndex

    val qName = new QualifiedName

    var index = parseId(startIndex + 1, tokens, qName)

    while (index < tokens.length && tokens.get(index).matches(Tokens.DotStr)) {
      index = parseId(index + 1, tokens, qName)
    }

    val parameters = if (tokens(index).exists(_.matches(Tokens.LParenStr))) {
      val builder      = new mutable.StringBuilder()
      var nestingCount = 1
      index += 1
      while (nestingCount > 0 && index < tokens.length) {
        if (tokens.get(index).matches(Tokens.RParenStr)) {
          nestingCount -= 1
        } else if (tokens.get(index).matches(Tokens.LParenStr)) {
          nestingCount += 1
        }
        if (nestingCount > 0) builder.append(tokens.get(index).contents)
        index += 1
      }
      Some(builder.toString())
    } else None

    accum.append(Annotation(qName, parameters))

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

  private def parseModifiersAndAnnotations(
    startIndex: Int,
    tokens: Tokens
  ): (Int, ArraySeq[Modifier], ArraySeq[Annotation]) = {
    var modifiers: mutable.ArrayBuffer[Modifier]     = null
    var annotations: mutable.ArrayBuffer[Annotation] = null

    var index    = startIndex
    var continue = true
    while (continue && index < tokens.length) {
      if (tokens.get(index).matches(Tokens.AtSignStr)) {
        if (annotations == null)
          annotations = new mutable.ArrayBuffer[Annotation]()
        index = parseAnnotation(index, tokens, annotations)
      } else if (modifierTokenStrs.contains(tokens.get(index).contents.toLowerCase)) {
        if (modifiers == null)
          modifiers = new mutable.ArrayBuffer[Modifier]()
        modifiers.append(tokenToModifier(tokens.get(index)))
        index += 1
      } else if (
        sharingModifiers.contains(tokens.get(index).contents.toLowerCase())
        && tokens(index + 1).exists(_.matches(Tokens.SharingStr))
      ) {
        // Combine to make sharing modifier
        if (modifiers == null)
          modifiers = new mutable.ArrayBuffer[Modifier]()
        modifiers.append(
          Modifier(s"${tokens.get(index).contents} ${tokens.get(index + 1).contents}")
        )
        index += 2
      } else {
        continue = false
      }
    }

    (
      index,
      if (modifiers == null) Modifiers.emptyArraySeq
      else ArraySeq.unsafeWrapArray(modifiers.toArray),
      if (annotations == null) Annotations.emptyArraySeq
      else ArraySeq.unsafeWrapArray(annotations.toArray)
    )
  }

  private def parseFormalParameterList(
    startIndex: Int,
    tokens: Tokens
  ): (Int, Option[FormalParameterList]) = {
    if (!tokens(startIndex).exists(_.matches(Tokens.LParenStr))) return (startIndex, None)

    var formalParameters: mutable.ArrayBuffer[FormalParameter] = null
    var index                                                  = startIndex + 1
    var indexAtStart                                           = startIndex
    while (
      indexAtStart != index && index < tokens.length && !tokens.get(index).matches(Tokens.RParenStr)
    ) {
      indexAtStart = index
      val formalParameter                    = new FormalParameter
      val (newIndex, modifiers, annotations) = parseModifiersAndAnnotations(index, tokens)
      index = newIndex
      formalParameter.setModifiers(modifiers)
      formalParameter.setAnnotations(annotations)
      val (newIndex2, typeRef) = parseTypeRef(index, tokens)
      index = newIndex2
      formalParameter.add(typeRef)
      index = parseId(index, tokens, formalParameter)
      if (tokens(index).exists(_.matches(Tokens.CommaStr))) index += 1
      if (formalParameters == null)
        formalParameters = mutable.ArrayBuffer()
      formalParameters.append(formalParameter)
    }
    if (tokens(index).exists(_.matches(Tokens.RParenStr))) index += 1
    (
      index,
      Some(
        if (formalParameters == null) FormalParameterList.empty
        else new FormalParameterList(ArraySeq.unsafeWrapArray(formalParameters.toArray))
      )
    )
  }
}
