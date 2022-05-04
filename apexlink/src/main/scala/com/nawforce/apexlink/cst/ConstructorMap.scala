package com.nawforce.apexlink.cst
import com.nawforce.apexlink.cst.AssignableSupport.isAssignable
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexConstructorLike, ApexDeclaration}
import com.nawforce.apexlink.types.core.{ConstructorDeclaration, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.modifiers.PRIVATE_MODIFIER
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.PathLocation

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

final case class ConstructorMap(
  typeName: Option[TypeName],
  td: Option[ApexClassDeclaration],
  constructorsByParam: Map[Int, Array[ConstructorDeclaration]],
  superConstructorsByParam: Option[ConstructorMap],
  errors: List[Issue]
) {
  val deepHash: Int = td.map(_.deepHash).getOrElse(0)

  def allConstructors: ArraySeq[ConstructorDeclaration] = {
    val buffer = new mutable.ArrayBuffer[ConstructorDeclaration]()
    constructorsByParam.values.foreach(ctor => buffer.addAll(ctor))
    ArraySeq.unsafeWrapArray(buffer.toArray)
  }

  def findConstructorByParams(
    params: ArraySeq[TypeName],
    context: VerifyContext
  ): Either[String, ConstructorDeclaration] = {
    val matched = constructorsByParam.get(params.length)
    if (matched.isEmpty) {
      return Left(s"No constructor defined with ${params.length} arguments")
    }
    val assignable = matched.get.filter(c => {
      val argZip = c.parameters.map(_.typeName).zip(params)
      argZip.forall(argPair => isAssignable(argPair._1, argPair._2, strict = false, context))
    })
    val potential =
      if (assignable.isEmpty)
        None
      else if (assignable.length == 1)
        Some(assignable.head)
      else {
        assignable.find(
          ctor => ctor.isMoreSpecific(ctor.parameters, params, context).contains(true)
        )
      }

    potential match {
      case Some(ctor) =>
        val isReallyPrivate =
          ctor.visibility == PRIVATE_MODIFIER && !areInSameApexFile(ctor, context.thisType)
        if (!isReallyPrivate) Right(ctor)
        else Left(s"Constructor is not visible: ${ctor.toString}")
      case _ =>
        typeName match {
          case Some(name) =>
            Left(s"Constructor not defined: void $name.<constructor>(${params.mkString(",")})")
          case None => Left(s"Constructor not defined: void <constructor>(${params.mkString(",")})")
        }
    }
  }

  private def areInSameApexFile(
    ctor: ConstructorDeclaration,
    calledFrom: TypeDeclaration
  ): Boolean = {
    (ctor, calledFrom) match {
      case (acl: ApexConstructorLike, ad: ApexDeclaration) => ad.location.path == acl.location.path
      case _                                               => false
    }
  }
}

object ConstructorMap {
  type WorkingMap = mutable.HashMap[Int, List[ConstructorDeclaration]]

  def apply(td: TypeDeclaration): ConstructorMap = {
    //TODO
    ConstructorMap.empty
  }

  def apply(
    td: TypeDeclaration,
    location: Option[PathLocation],
    ctors: ArraySeq[ConstructorDeclaration],
    superClassMap: Option[ConstructorMap]
  ): ConstructorMap = {
    val workingMap = new WorkingMap()
    val errors     = mutable.Buffer[Issue]()

    val deduped = deDupeConstructors(ctors, errors)

    deduped.foreach(ctor => {
      val key     = ctor.parameters.length
      val matched = workingMap.getOrElse(key, Nil)
      workingMap.put(key, ctor :: matched)
    })

    td match {
      case td: ApexClassDeclaration =>
        new ConstructorMap(
          Some(td.typeName),
          Some(td),
          toMap(workingMap),
          superClassMap,
          errors.toList
        )
      case td: TypeDeclaration =>
        new ConstructorMap(Some(td.typeName), None, toMap(workingMap), superClassMap, errors.toList)
    }
  }

  def empty: ConstructorMap = {
    new ConstructorMap(None, None, Map(), None, Nil)
  }

  private def deDupeConstructors(
    ctors: ArraySeq[ConstructorDeclaration],
    errors: mutable.Buffer[Issue]
  ): ArraySeq[ConstructorDeclaration] = {
    val dupes = ctors.duplicates(_.parameters.map(_.typeName.toString()).mkString(","));
    dupes.foreach(duplicates => {
      duplicates._2.foreach(dup => {
        duplicates._1 match {
          case ac: ApexConstructorLike =>
            setConstructorError(
              dup,
              s"Constructor is a duplicate of an earlier constructor at ${ac.idLocation.displayPosition}",
              errors
            )
          case _ =>
            //TODO: Im pretty sure this will never hit, so should probably remove this
            setConstructorError(dup, s"Constructor is a duplicate of another constructor", errors)
        }
      })
    })
    ctors.filterNot(dupes.keys.toArray.contains)
  }

  private def setConstructorError(
    constructor: ConstructorDeclaration,
    error: String,
    errors: mutable.Buffer[Issue]
  ): Unit = {
    constructor match {
      case ac: ApexConstructorLike =>
        errors.append(new Issue(ac.location.path, Diagnostic(ERROR_CATEGORY, ac.idLocation, error)))
//    case pc: PlatformConstructor =>  errors.append(new Issue(TODO, Diagnostic(ERROR_CATEGORY, TODO, error)))
      case _ => ()
    }
  }
  private def toMap(workingMap: WorkingMap): Map[Int, Array[ConstructorDeclaration]] = {
    workingMap.map(kv => (kv._1, kv._2.toArray)).toMap
  }
}
