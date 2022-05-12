/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait TypeRef {
  def getFullName: String
  override def toString: String = {
    getFullName
  }
}

object TypeRef {
  final val emptyArraySeq: ArraySeq[TypeRef] = ArraySeq()
}

// TODO: Make immutable
final case class UnresolvedTypeRef(
  typeNameSegments: mutable.ArrayBuffer[TypeNameSegment] = mutable.ArrayBuffer[TypeNameSegment](),
  var arraySubscripts: Int = 0
) extends TypeRef {

  override def getFullName: String = {
    toString
  }

  override def toString: String = {
    import StringUtils._
    asString(typeNameSegments, ".") + ("[]" * arraySubscripts)
  }
}

object UnresolvedTypeRef {
  def apply(typeNameSegments: Array[TypeNameSegment], arraySubscripts: Int): UnresolvedTypeRef = {
    val typeRef = new UnresolvedTypeRef
    typeNameSegments.foreach(typeRef.typeNameSegments.append)
    typeRef.arraySubscripts = arraySubscripts
    typeRef
  }

  /* Convert a string into a UnresolvedTypeRef. Note: This is really only suitable for well formed type names because
   * it does not handle whitespace or array subscripts or even do much error handling. */
  def apply(typeName: String): Either[String, UnresolvedTypeRef] = {
    val parts = safeSplit(typeName, '.')

    val segments: List[Either[String, TypeNameSegment]] = parts.map(part => {
      if (part.contains('<')) {
        val argSplit = part.split("<", 2)
        if (argSplit.length != 2 || argSplit(1).length < 2 || part.last != '>')
          Left(s"Unmatched '<' found in '$part'")
        else {
          buildTypeNames(argSplit(1).take(argSplit(1).length - 1)) match {
            case Right(argTypes) =>
              Right(TypeNameSegment(argSplit.head, argTypes))
            case Left(err) =>
              Left(err)
          }
        }
      } else {
        Right(TypeNameSegment(part))
      }
    })

    val errors = segments.collect { case Left(error) => error }
    if (errors.nonEmpty)
      return Left(errors.head)
    Right(apply(segments.collect { case Right(segment) => segment }.toArray, 0))
  }

  /* Split a list of comma delimited type names */
  private def buildTypeNames(value: String): Either[String, Array[UnresolvedTypeRef]] = {
    val args =
      safeSplit(value, ',').foldLeft(ArrayBuffer[Either[String, UnresolvedTypeRef]]())(
        (acc, arg) => {
          acc.append(apply(arg))
          acc
        }
      )

    args
      .collectFirst { case Left(err) => err }
      .map(err => Left(err))
      .getOrElse(Right(args.collect { case Right(tn) => tn }.toArray))
  }

  /** Split a string at 'separator' but ignoring if within '<...>' blocks. */
  private def safeSplit(value: String, separator: Char): List[String] = {
    var parts: List[String] = Nil
    var current             = new StringBuffer()
    var depth               = 0
    value.foreach {
      case '<' => depth += 1; current.append('<')
      case '>' => depth -= 1; current.append('>')
      case x if x == separator && depth == 0 =>
        parts = current.toString :: parts; current = new StringBuffer()
      case x => current.append(x)
    }
    parts = current.toString :: parts
    parts.reverse
  }
}

final case class TypeNameSegment(id: Id, typeArguments: TypeArguments) {

  def getArguments: ArraySeq[TypeRef] = {
    ArraySeq.unsafeWrapArray(typeArguments.typeList.typeRefs.toArray)
  }

  def replaceArguments(args: ArraySeq[TypeRef]): TypeNameSegment = {
    TypeNameSegment(id, TypeArguments(TypeList(args)))
  }

  override def toString: String = {
    if (typeArguments.typeList.typeRefs.nonEmpty)
      s"$id<${getArguments.map(_.getFullName).mkString(",")}>"
    else
      id.toString
  }
}

object TypeNameSegment {
  def apply(name: String): TypeNameSegment = {
    new TypeNameSegment(Id(IdToken(name, Location.default)), TypeArguments.empty)
  }

  def apply(name: String, typeArguments: TypeArguments): TypeNameSegment = {
    new TypeNameSegment(Id(IdToken(name, Location.default)), typeArguments)
  }

  def apply(name: String, typeArguments: Array[UnresolvedTypeRef]): TypeNameSegment = {
    new TypeNameSegment(
      Id(IdToken(name, Location.default)),
      TypeArguments(TypeList(ArraySeq.unsafeWrapArray(typeArguments)))
    )
  }

  def apply(name: String, params: Array[String]): TypeNameSegment = {
    new TypeNameSegment(Id(IdToken(name, Location.default)), TypeArguments(params))
  }
}

// TODO: Can this be removed
final case class TypeArguments(var typeList: TypeList) {
  override def toString: String = {
    typeList.toString
  }
}

object TypeArguments {
  final val empty = TypeArguments(TypeList.empty)

  def apply(params: Array[String]): TypeArguments = {
    val typeArguments = ArraySeq.unsafeWrapArray(params.map(tp => {
      val typeRef = new UnresolvedTypeRef()
      typeRef.typeNameSegments.append(
        new TypeNameSegment(Id(IdToken(tp, Location.default)), TypeArguments.empty)
      )
      typeRef
    }))

    TypeArguments(TypeList(typeArguments))
  }
}

final case class TypeList(typeRefs: ArraySeq[TypeRef]) {

  override def toString: String = {
    import StringUtils._
    asString(typeRefs, ",")
  }
}

object TypeList {
  final val empty = TypeList(TypeRef.emptyArraySeq)
}
