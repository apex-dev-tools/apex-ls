/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import com.financialforce.types.{base, _}
import com.financialforce.types.base.{
  Annotation,
  IdLocationHolder,
  IdWithLocation,
  Location,
  Modifier,
  Position,
  PropertyBlock,
  QualifiedName,
  TypeNameSegment,
  TypeRef,
  UnresolvedTypeRef
}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait TypeRefAssignable {
  def add(tr: UnresolvedTypeRef): Unit
}

class MemberDeclaration extends TypeRefAssignable {

  var modifiers: Array[Modifier]         = Modifier.emptyArray
  var annotations: Array[Annotation]     = Annotation.emptyArray
  var typeRef: Option[UnresolvedTypeRef] = None

  def setModifiers(modifiers: Array[Modifier]): Unit       = this.modifiers = modifiers
  def setAnnotations(annotations: Array[Annotation]): Unit = this.annotations = annotations

  override def add(tr: UnresolvedTypeRef): Unit = typeRef = Some(tr)
}

class FormalParameter private (
  val annotations: Array[Annotation],
  val modifiers: Array[Modifier],
  var typeRef: TypeRef,
  val name: String,
  location: Location
) extends IdLocationHolder(location)
    with IFormalParameter

object FormalParameter {
  val emptyArraySeq: ArraySeq[FormalParameter] = ArraySeq[FormalParameter]()
  def apply(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: LocatableIdToken
  ): FormalParameter = {
    new FormalParameter(
      Annotation.intern(annotations),
      Modifier.intern(modifiers),
      typeRef,
      id.name,
      id.location
    )
  }
}

sealed trait BodyDeclaration extends IBodyDeclaration {
  def id: IdWithLocation

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

  def bodyLocation: Option[Location] = {
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

case class ConstructorDeclaration private (
  annotations: Array[Annotation],
  modifiers: Array[Modifier],
  qname: QualifiedName,
  formalParameters: ArraySeq[FormalParameter]
) extends BodyDeclaration
    with IConstructorDeclaration
    with MutableTypeAppendable {
  def id: IdWithLocation = qname.parts.last
}

object ConstructorDeclaration {
  final val emptyArraySeq = ArraySeq[ConstructorDeclaration]()

  def apply(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    qName: QualifiedName,
    formalParameterList: ArraySeq[FormalParameter]
  ): ConstructorDeclaration = {
    new ConstructorDeclaration(
      Annotation.intern(annotations),
      Modifier.intern(modifiers),
      qName,
      formalParameterList
    )
  }
}

class MethodDeclaration private (
  val annotations: Array[Annotation],
  val modifiers: Array[Modifier],
  var typeRef: Option[TypeRef],
  val id: IdWithLocation,
  val formalParameters: ArraySeq[FormalParameter]
) extends BodyDeclaration
    with IMethodDeclaration
    with MutableTypeAppendable

object MethodDeclaration {
  final val emptyArraySeq = ArraySeq[MethodDeclaration]()

  def apply(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: Option[TypeRef],
    id: LocatableIdToken,
    formalParameterList: ArraySeq[FormalParameter]
  ): MethodDeclaration = {
    new MethodDeclaration(
      Annotation.intern(annotations),
      Modifier.intern(modifiers),
      typeRef,
      id,
      formalParameterList
    )
  }
}

class PropertyDeclaration private (
  val annotations: Array[Annotation],
  val modifiers: Array[Modifier],
  var typeRef: TypeRef,
  var propertyBlocks: Array[PropertyBlock],
  val id: LocatableIdToken
) extends BodyDeclaration
    with IPropertyDeclaration
    with MutableTypeAppendable

object PropertyDeclaration {
  final val emptyArraySeq = ArraySeq[PropertyDeclaration]()

  def apply(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    propertyBlocks: Array[PropertyBlock],
    id: LocatableIdToken
  ): PropertyDeclaration = {
    new PropertyDeclaration(
      Annotation.intern(annotations),
      Modifier.intern(modifiers),
      typeRef,
      propertyBlocks,
      id
    )
  }
}

case class FieldDeclaration private (
  annotations: Array[Annotation],
  modifiers: Array[Modifier],
  var typeRef: TypeRef,
  id: LocatableIdToken
) extends BodyDeclaration
    with IFieldDeclaration
    with MutableTypeAppendable

object FieldDeclaration {
  final val emptyArraySeq = ArraySeq[FieldDeclaration]()

  def apply(
    annotations: Array[Annotation],
    modifiers: Array[Modifier],
    typeRef: TypeRef,
    id: LocatableIdToken
  ): FieldDeclaration = {
    new FieldDeclaration(Annotation.intern(annotations), Modifier.intern(modifiers), typeRef, id)
  }
}

case class Initializer(isStatic: Boolean)
    extends BodyDeclaration
    with IInitializer
    with MutableTypeAppendable {
  override val id: LocatableIdToken = Initializer.id
}

object Initializer {
  final val id: LocatableIdToken = LocatableIdToken("initializer", Location.default)
  final val emptyArraySeq        = ArraySeq[Initializer]()
}

object Parse {

  def parseClassType(ctd: IMutableTypeDeclaration, tokens: Tokens): IMutableTypeDeclaration = {
    var (index, modifiers, annotations) = parseModifiersAndAnnotations(0, tokens)
    ctd.setModifiers(modifiers)
    ctd.setAnnotations(annotations)

    if (!tokens.matches(index, Tokens.ClassStr))
      throw new Exception(s"Missing '${Tokens.ClassStr}'")
    index += 1
    index = setId(index, tokens, ctd)

    index = tokens.findIndex(t => t.matches(Tokens.ExtendsStr))
    if (index != -1) {
      val (newIndex, typeRef) = parseTypeRef(index + 1, tokens)
      index = newIndex
      ctd.setExtends(typeRef)
    }

    index = tokens.findIndex(t => t.matches(Tokens.ImplementsStr))
    if (index != -1) {
      val (_, typeList) = parseTypeList(index + 1, tokens)
      ctd.setImplements(typeList)
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
    index = setId(index, tokens, itd)

    index = tokens.findIndex(t => t.matches(Tokens.ExtendsStr))
    if (index != -1) {
      val (_, typeList) = parseTypeList(index + 1, tokens)
      // On interfaces we set 'extends' to 'implements' for type consistency with classes
      itd.setImplements(typeList)
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
    index = setId(index, tokens, etd)

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

  def parseEnumMember(etd: IMutableTypeDeclaration, tokens: Tokens): Seq[LocatableIdToken] = {
    if (tokens.isEmpty) return Seq.empty

    val constant = tokenToId(tokens.head)
    val field    = FieldDeclaration(Array(), Array(Modifier(Tokens.StaticStr)), etd, constant)
    etd.appendField(field)
    Seq(constant)
  }

  private def toQualifiedName(tr: UnresolvedTypeRef): QualifiedName = {
    QualifiedName(tr.typeNameSegments.map(_.id))
  }

  private def addInitializer(
    ctd: IMutableTypeDeclaration,
    isStatic: Boolean
  ): Option[Initializer] = {
    val initializer = Initializer(isStatic)
    ctd.appendInitializer(initializer)
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
      md.annotations,
      md.modifiers,
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
    res: IMutableTypeDeclaration
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
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef, id, formalParameterList.get)
    res.appendMethod(method)
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

    val property = PropertyDeclaration(md.annotations, md.modifiers, md.typeRef.get, Array(), id)
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

      val field = FieldDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)
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

  private def tokenToId(token: Token): LocatableIdToken =
    LocatableIdToken(token.contents, token.location)

  private def getId(startIndex: Int, tokens: Tokens): (Int, Option[LocatableIdToken]) = {
    if (startIndex >= tokens.length) {
      (startIndex, None)
    } else if (tokens.get(startIndex).isInstanceOf[LocatableIdToken]) {
      val id = tokenToId(tokens.get(startIndex))
      (startIndex + 1, Some(id))
    } else {
      (startIndex, None)
    }
  }

  private def setId[T <: IMutableTypeDeclaration](startIndex: Int, tokens: Tokens, res: T): Int = {
    val (newIndex, id) = getId(startIndex, tokens)
    id.foreach(res.setId)
    newIndex
  }

  private def parseTypeNameSegment(
    startIndex: Int,
    tokens: Tokens
  ): (Int, Option[TypeNameSegment]) = {
    tokens(startIndex) match {
      case Some(token: LocatableIdToken) =>
        val id                         = tokenToId(token)
        val (nextIndex, typeArguments) = parseTypeArguments(startIndex + 1, tokens)
        (nextIndex, Some(TypeNameSegment(id, typeArguments)))
      case _ => (startIndex, None)
    }
  }

  private def parseTypeArguments(startIndex: Int, tokens: Tokens): (Int, ArraySeq[TypeRef]) = {
    if (startIndex >= tokens.length) return (startIndex, TypeRef.emptyArraySeq)
    if (!tokens.get(startIndex).matches(Tokens.LessThanStr))
      return (startIndex, TypeRef.emptyArraySeq)

    val (index, typeList) = parseTypeList(startIndex + 1, tokens)
    if (!tokens(index).exists(_.matches(Tokens.GreaterThanStr))) throw new Exception("Missing >")
    (index + 1, typeList)
  }

  private def parseTypeList(startIndex: Int, tokens: Tokens): (Int, ArraySeq[TypeRef]) = {
    val typeRefs         = mutable.ArrayBuffer[TypeRef]()
    var (index, typeRef) = parseTypeRef(startIndex, tokens)
    typeRefs.append(typeRef)
    while (index < tokens.length && tokens.get(index).matches(Tokens.CommaStr)) {
      val (newIndex, typeRef) = parseTypeRef(index + 1, tokens)
      index = newIndex
      typeRefs.append(typeRef)
    }
    if (typeRefs.nonEmpty)
      (index, ArraySeq.unsafeWrapArray(typeRefs.toArray))
    else
      (index, TypeRef.emptyArraySeq)
  }

  private def parseTypeRef(startIndex: Int, tokens: Tokens): (Int, UnresolvedTypeRef) = {

    val segments                 = new ArrayBuffer[TypeNameSegment]()
    var (index, typeNameSegment) = parseTypeNameSegment(startIndex, tokens)
    if (typeNameSegment.isEmpty) {
      throw new Exception(s"Missing Identifier")
    }
    segments.append(typeNameSegment.get)

    while (index < tokens.length && tokens.get(index).matches(Tokens.DotStr)) {
      val (newIndex, typeNameSegment) = parseTypeNameSegment(index + 1, tokens)
      index = newIndex

      if (typeNameSegment.isEmpty) {
        throw new Exception(s"Missing Identifier")
      }
      segments.append(typeNameSegment.get)
    }

    val (newIndex, count) = parseArraySubscripts(index, tokens)
    (newIndex, base.UnresolvedTypeRef(segments.toArray, count))
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

    val names         = new mutable.ArrayBuffer[LocatableIdToken]()
    var (index, name) = getId(startIndex + 1, tokens)
    name.foreach(names.append)
    while (index < tokens.length && tokens.get(index).matches(Tokens.DotStr)) {
      val (newIndex, newName) = getId(index + 1, tokens)
      newName.foreach(names.append)
      index = newIndex
    }
    val qName = QualifiedName(names.toArray)

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

    accum.append(Annotation(qName.toString, parameters))

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
  ): (Int, Array[Modifier], Array[Annotation]) = {
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
      if (modifiers == null) Modifier.emptyArray
      else modifiers.toArray,
      if (annotations == null) Annotation.emptyArray
      else annotations.toArray
    )
  }

  private def parseFormalParameterList(
    startIndex: Int,
    tokens: Tokens
  ): (Int, Option[ArraySeq[FormalParameter]]) = {
    if (!tokens(startIndex).exists(_.matches(Tokens.LParenStr))) return (startIndex, None)

    var formalParameters: mutable.ArrayBuffer[FormalParameter] = null
    var index                                                  = startIndex + 1
    var indexAtStart                                           = startIndex
    while (
      indexAtStart != index && index < tokens.length && !tokens.get(index).matches(Tokens.RParenStr)
    ) {
      indexAtStart = index

      val (newIndex, modifiers, annotations) = parseModifiersAndAnnotations(index, tokens)
      index = newIndex
      val (newIndex2, typeRef) = parseTypeRef(index, tokens)
      index = newIndex2
      val (newIndex3, id) = getId(index, tokens)
      index = newIndex3
      if (tokens(index).exists(_.matches(Tokens.CommaStr))) index += 1

      id.foreach(id => {
        if (formalParameters == null)
          formalParameters = mutable.ArrayBuffer()
        val formalParameter = FormalParameter(annotations, modifiers, typeRef, id)
        formalParameters.append(formalParameter)
      })
    }
    if (tokens(index).exists(_.matches(Tokens.RParenStr))) index += 1
    (
      index,
      Some(
        if (formalParameters == null) FormalParameter.emptyArraySeq
        else ArraySeq.unsafeWrapArray(formalParameters.toArray)
      )
    )
  }
}
