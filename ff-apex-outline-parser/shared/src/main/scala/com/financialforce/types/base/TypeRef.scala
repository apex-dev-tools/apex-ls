/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.types.base

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing.MurmurHash3

/** Reference holder to a type. Typically TypeRefs start as a UnresolvedTypeRef and are replaced by a ITypeDeclaration
  * during a resolve phase. The fullName can be used to determine equivalence between these two types of TypeRef.
  */
trait TypeRef {
  def fullName: String

  def sameRef(other: TypeRef): Boolean = {
    other.fullName.equalsIgnoreCase(fullName)
  }
}

object TypeRef {
  final val emptyArraySeq: ArraySeq[TypeRef] = ArraySeq()

  def toTypeRefs(params: Array[String]): ArraySeq[TypeRef] = {
    ArraySeq.unsafeWrapArray(params.map(tp => {
      UnresolvedTypeRef(
        Array(new TypeNameSegment(new LocatableId(tp, Location.default), emptyArraySeq)),
        0
      )
    }))
  }
}

/** A reference to a type in raw form as it appears in source files. This just captures the segments in the TypeRef
  * and any array subscripts. We expect a resolve process to use this information to locate an ITypeDeclaration.
  */
final case class UnresolvedTypeRef(typeNameSegments: Array[TypeNameSegment], arraySubscripts: Int)
    extends TypeRef {

  override def fullName: String = {
    typeNameSegments.mkString(".") + ("[]" * arraySubscripts)
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[UnresolvedTypeRef]
    typeNameSegments.sameElements(other.typeNameSegments) &&
    arraySubscripts == other.arraySubscripts
  }

  override def hashCode(): Int = {
    MurmurHash3.orderedHash(Seq(arraySubscripts, MurmurHash3.arrayHash(typeNameSegments)))
  }

  override def toString: String = fullName
}

object UnresolvedTypeRef {

  /** Convert a string into a UnresolvedTypeRef. Note: This is really only intended for internal use as the
    * error handling is limited.
    */
  def apply(typeName: String): Either[String, UnresolvedTypeRef] = {

    // Strip trailing array subscripts
    var remaining: String = typeName.trim.replaceAll("\\s", "")
    var arraySubscripts   = 0
    while (remaining.endsWith("[]")) {
      arraySubscripts += 1
      remaining = remaining.substring(0, remaining.length - 2)
    }

    val parts = safeSplit(remaining, '.')
    val segments: List[Either[String, TypeNameSegment]] = parts.map(part => {
      // Handle segment type arguments
      if (part.contains('<')) {
        val argSplit = part.split("<", 2)
        if (argSplit.length != 2 || argSplit(1).length < 2 || remaining.last != '>')
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
    Right(apply(segments.collect { case Right(segment) => segment }.toArray, arraySubscripts))
  }

  /** Split a list of comma delimited type names */
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

  /** Split a string at 'separator' but ignoring if within '&lt;...>' blocks. */
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

/** A single segment of a TypeRef, essentially and id with optional type arguments */
final case class TypeNameSegment(id: IdWithLocation, typeArguments: ArraySeq[TypeRef]) {

  def replaceArguments(args: ArraySeq[TypeRef]): TypeNameSegment = {
    TypeNameSegment(id, args)
  }

  override def toString: String = {
    if (typeArguments.nonEmpty)
      s"$id<${typeArguments.map(_.fullName).mkString(",")}>"
    else
      id.toString
  }
}

object TypeNameSegment {
  def apply(name: String): TypeNameSegment = {
    new TypeNameSegment(new LocatableId(name, Location.default), TypeRef.emptyArraySeq)
  }

  def apply(name: String, typeArguments: ArraySeq[TypeRef]): TypeNameSegment = {
    new TypeNameSegment(new LocatableId(name, Location.default), typeArguments)
  }

  def apply(name: String, typeArguments: Array[UnresolvedTypeRef]): TypeNameSegment = {
    new TypeNameSegment(
      new LocatableId(name, Location.default),
      ArraySeq.unsafeWrapArray(typeArguments)
    )
  }

  def apply(name: String, params: Array[String]): TypeNameSegment = {
    new TypeNameSegment(new LocatableId(name, Location.default), TypeRef.toTypeRefs(params))
  }
}
