package com.nawforce.apexlink.cst
import com.nawforce.apexlink.cst.AssignableSupport.isAssignable
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexConstructorLike}
import com.nawforce.apexlink.types.core.{ConstructorDeclaration, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
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
  ): Option[ConstructorDeclaration] = {
    val matched = constructorsByParam.get(params.length)
    if (matched.isEmpty) {
      return None
    }
    val assignable = matched.get.filter(c => {
      val argZip = c.parameters.map(_.typeName).zip(params)
      argZip.forall(argPair => isAssignable(argPair._1, argPair._2, false, context))
    })
    if (assignable.isEmpty)
      None
    else if (assignable.length == 1)
      Some(assignable.head)
    else {
      //TODO: use matched.get.find(_.isMoreSpecific(params, context))?
      None
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
