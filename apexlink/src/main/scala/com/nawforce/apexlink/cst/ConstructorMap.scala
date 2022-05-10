package com.nawforce.apexlink.cst
import com.nawforce.apexlink.cst.AssignableSupport.isAssignable
import com.nawforce.apexlink.finding.{RelativeTypeContext, RelativeTypeName}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexConstructorLike, ApexDeclaration}
import com.nawforce.apexlink.types.core.{ConstructorDeclaration, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.modifiers.{ModifierResults, PRIVATE_MODIFIER, PUBLIC_MODIFIER}
import com.nawforce.pkgforce.names.{Name, TypeName}
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
    val matched = constructorsByParam
      .get(params.length)

    if (matched.isEmpty)
      return Left(s"No constructor defined with ${params.length} arguments")

    val assignable = matched.get.filter(c => {
      val argZip = c.parameters.map(_.typeName).zip(params)
      argZip.forall(argPair => isAssignable(argPair._1, argPair._2, strict = false, context))
    })

    findPotentialMatch(assignable, params, context)

  }

  private def findPotentialMatch(
    assignable: Array[ConstructorDeclaration],
    params: ArraySeq[TypeName],
    context: VerifyContext
  ): Either[String, ConstructorDeclaration] = {
    val potential =
      if (assignable.isEmpty)
        None
      else if (assignable.length == 1)
        Some(assignable.head)
      else {
        assignable.find(
          ctor =>
            assignable.forall(
              c =>
                c == ctor || ctor
                  .hasMoreSpecificParams(c.parameters, params, context)
                  .contains(true)
            )
        )
      }
    potential match {
      case Some(ctor) =>
        val isReallyPrivate =
          ctor.visibility == PRIVATE_MODIFIER && !areInSameApexFile(ctor, context.thisType)
        if (!isReallyPrivate) Right(ctor)
        else if (ctor.isTestVisible && context.thisType.inTest) Right(ctor)
        else {
          //Check the rest of 'assignable' for accessible ctors, if not revert to the original error
          findPotentialMatch(assignable.filterNot(_ == ctor), params, context) match {
            case Right(ctor) => Right(ctor)
            case _           => Left(s"Constructor is not visible: ${ctor.toString}")
          }
        }
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
  val publicModifierResult                   = new ModifierResults(ArraySeq(PUBLIC_MODIFIER), emptyIssues)

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

    applySyntheticsCtors(td, workingMap)

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

  private def applySyntheticsCtors(td: TypeDeclaration, workingMap: WorkingMap): Unit = {
    def toCtor(
      qNames: List[Name],
      params: ArraySeq[FormalParameter] = emptyParams
    ): ApexConstructorDeclaration = {
      ApexConstructorDeclaration(
        publicModifierResult,
        QualifiedName(qNames),
        params,
        td.inTest,
        EagerBlock.empty
      )
    }
    def toParam(id: String, typeCntxt: RelativeTypeContext, typeName: TypeName): FormalParameter = {
      FormalParameter(publicModifierResult, RelativeTypeName(typeCntxt, typeName), Id(Name(id)))
    }

    lazy val qNames = td.outerTypeName.map(x => List(x.name)).getOrElse(Nil) ++ List(td.name)
    if (workingMap.keys.isEmpty && !td.isCustomException)
      workingMap.put(0, List(toCtor(qNames)))

    if (td.isCustomException) {
      val synthetics = td match {
        case cd: ClassDeclaration =>
          ArraySeq(
            toCtor(qNames),
            toCtor(qNames, ArraySeq(toParam("param1", cd.typeContext, TypeNames.String))),
            toCtor(qNames, ArraySeq(toParam("param1", cd.typeContext, TypeNames.Exception))),
            toCtor(
              qNames,
              ArraySeq(
                toParam("param1", cd.typeContext, TypeNames.String),
                toParam("param2", cd.typeContext, TypeNames.Exception)
              )
            )
          )
        case _ => ArraySeq.empty
      }
      synthetics.foreach(s => {
        val key = s.parameters.length
        workingMap.put(key, s :: workingMap.getOrElse(key, Nil))
      })
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
