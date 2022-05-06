package com.nawforce.apexlink.cst
import com.nawforce.apexlink.cst.AssignableSupport.isAssignable
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexConstructorLike, ApexDeclaration}
import com.nawforce.apexlink.types.core.{ConstructorDeclaration, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.modifiers.{ModifierResults, PRIVATE_MODIFIER, PUBLIC_MODIFIER}
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
          ctor => ctor.hasMoreSpecificParams(ctor.parameters, params, context).contains(true)
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
  val emptyIssues: ArraySeq[Issue]           = ArraySeq.empty
  val emptyParams: ArraySeq[FormalParameter] = ArraySeq.empty

  def apply(td: TypeDeclaration): ConstructorMap = {
    val workingMap = new WorkingMap()
    td.constructors.foreach(ctor => {
      val key = ctor.parameters.length
      workingMap.put(key, ctor :: workingMap.getOrElse(key, Nil))
    })
    new ConstructorMap(Some(td.typeName), None, toMap(workingMap), None, Nil)
  }

  def apply(
    td: TypeDeclaration,
    location: Option[PathLocation],
    ctors: ArraySeq[ConstructorDeclaration],
    superClassMap: ConstructorMap
  ): ConstructorMap = {
    val workingMap = new WorkingMap()
    val errors     = mutable.Buffer[Issue]()

    val deduped = deDupeConstructors(ctors, errors)

    deduped.foreach(ctor => {
      val key                      = ctor.parameters.length
      val ctorsWithSameParamLength = workingMap.getOrElse(key, Nil)
      val platformGenericDupes =
        ctorsWithSameParamLength.find(
          x => x.hasSameParameters(ctor, allowPlatformGenericEquivalence = true)
        )
      if (platformGenericDupes.nonEmpty)
        setConstructorDuplicateError(ctor, platformGenericDupes.head, errors)
      workingMap.put(key, ctor :: ctorsWithSameParamLength)
    })

    injectDefaultConstructorIfNeeded(td, workingMap)

    td match {
      case td: ApexClassDeclaration =>
        new ConstructorMap(
          Some(td.typeName),
          Some(td),
          toMap(workingMap),
          Some(superClassMap),
          errors.toList
        )
      case td: TypeDeclaration =>
        new ConstructorMap(
          Some(td.typeName),
          None,
          toMap(workingMap),
          Some(superClassMap),
          errors.toList
        )
    }
  }

  def empty: ConstructorMap = {
    new ConstructorMap(None, None, Map(), None, Nil)
  }

  private def injectDefaultConstructorIfNeeded(td: TypeDeclaration, workingMap: WorkingMap) = {
    if (workingMap.keys.isEmpty) {
      val qNames = td.outerTypeName.map(x => List(x.name)).getOrElse(Nil) ++ List(td.name)

      val dCtor = ApexConstructorDeclaration(
        new ModifierResults(ArraySeq(PUBLIC_MODIFIER), emptyIssues),
        QualifiedName(qNames),
        emptyParams,
        td.inTest,
        EagerBlock.empty
      )
      workingMap.put(0, List(dCtor))
    }
  }

  private def deDupeConstructors(
    ctors: ArraySeq[ConstructorDeclaration],
    errors: mutable.Buffer[Issue]
  ): ArraySeq[ConstructorDeclaration] = {
    val dupes = ctors.duplicates(_.parameters.map(_.typeName.toString()).mkString(","))
    dupes.foreach(duplicates => {
      duplicates._2.foreach(dup => {
        setConstructorDuplicateError(dup, duplicates._1, errors)
      })
    })
    ctors.filterNot(dupes.values.flatten.toSeq.contains)
  }

  private def setConstructorDuplicateError(
    constructor: ConstructorDeclaration,
    duplicateOf: ConstructorDeclaration,
    errors: mutable.Buffer[Issue]
  ): Unit = {
    (constructor, duplicateOf) match {
      case (ac: ApexConstructorLike, dup: ApexConstructorLike) =>
        errors.append(
          new Issue(
            ac.location.path,
            Diagnostic(
              ERROR_CATEGORY,
              ac.idLocation,
              s"Constructor is a duplicate of an earlier constructor at ${dup.idLocation.displayPosition}"
            )
          )
        )
      case _ =>
    }
  }

  private def toMap(workingMap: WorkingMap): Map[Int, Array[ConstructorDeclaration]] = {
    workingMap.map(kv => (kv._1, kv._2.toArray)).toMap
  }
}
