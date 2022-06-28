/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import com.financialforce.types._
import com.financialforce.types.base.{TypeNameSegment, TypeRef, UnresolvedTypeRef}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SubsetComparator(
  firstTd: ITypeDeclaration,
  var fResolver: Option[TypeIdResolver],
  var sResolver: Option[TypeIdResolver]
) {
  private val warnings: ArrayBuffer[String] = ArrayBuffer()

  def getWarnings: Set[String] = {
    warnings.toSet
  }

  def clearWarnings(): Unit = {
    warnings.clear()
  }

  def unresolvedSubsetOf(secondTd: ITypeDeclaration): Unit = {
    if (fResolver.isEmpty) {
      fResolver = Some(TypeIdCollector(List(firstTd)))
    }
    if (sResolver.isEmpty) {
      sResolver = Some(TypeIdCollector(List(secondTd)))
    }
    firstTd match {
      case cls: TestClassTypeDeclaration =>
        subsetOffClassDeclarations(cls, secondTd.asInstanceOf[TestClassTypeDeclaration])
      case int: TestInterfaceTypeDeclaration =>
        compareInterfaceTypeDeclarations(int, secondTd.asInstanceOf[TestInterfaceTypeDeclaration])
      case enm: TestEnumTypeDeclaration =>
        compareEnumTypeDeclarations(enm, secondTd.asInstanceOf[TestEnumTypeDeclaration])
      case _ =>
    }
  }

  private def subsetOffClassDeclarations(
    first: TestClassTypeDeclaration,
    second: TestClassTypeDeclaration
  ): Unit = {
    def innerClassTypeDeclarations(o: TestClassTypeDeclaration): Seq[TestClassTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[TestClassTypeDeclaration])
        .map(_.asInstanceOf[TestClassTypeDeclaration])
    }

    def innerInterfaceTypeDeclarations(
      o: TestClassTypeDeclaration
    ): Seq[TestInterfaceTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[TestInterfaceTypeDeclaration])
        .map(_.asInstanceOf[TestInterfaceTypeDeclaration])
    }

    def innerEnumTypeDeclarations(o: TestClassTypeDeclaration): Seq[TestEnumTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[TestEnumTypeDeclaration])
        .map(_.asInstanceOf[TestEnumTypeDeclaration])
    }

    def compareInnerClasses(
      fst: Seq[TestClassTypeDeclaration],
      snd: Seq[TestClassTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner class not found ${f.id} in ${first.id}")
        }
        subsetOffClassDeclarations(f, sOpt.get)
      })
    }

    def compareInnerInterfaces(
      fst: Seq[TestInterfaceTypeDeclaration],
      snd: Seq[TestInterfaceTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner interface not found ${f.id} in ${first.id}")
        }
        compareInterfaceTypeDeclarations(f, sOpt.get)
      })
    }

    def compareInnerEnums(
      fst: Seq[TestEnumTypeDeclaration],
      snd: Seq[TestEnumTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner enum not found ${f.id} in ${first.id}")
        }
        compareEnumTypeDeclarations(f, sOpt.get)
      })
    }

    //Start comparing
    checkAndThrowIfDiff("Different Annotations", first.annotations, second.annotations)
    checkAndThrowIfDiff("Different modifiers", first.modifiers, second.modifiers)

    if (first.id != second.id) {
      throw new Exception(s"Different or empty class id ${first.id} != ${second.id}")
    }

    if (first.extendsTypeRef != second.extendsTypeRef) {
      throw new Exception(s"Different extends ${first.extendsTypeRef} != ${second.extendsTypeRef}")
    }

    if (first.implementsTypeList != second.implementsTypeList) {
      val firstTypeRefs  = Option(first.implementsTypeList).get
      val secondTypeRefs = Option(second.implementsTypeList).get
      val isSubset       = areTypeRefsSubsets(firstTypeRefs, secondTypeRefs)
      if (!isSubset)
        throw new Exception(
          s"Different implements ${first.implementsTypeList} != ${second.implementsTypeList}"
        )
      else
        warnings.append(
          prettyWarnings(
            "Waring: implementsTypeList not strictly equal but are subsets",
            ArraySeq(first.implementsTypeList),
            ArraySeq(second.implementsTypeList)
          )
        )
    }

    if (
      first.initializers.filter(_.isStatic) != second.initializers.filter(
        _.isStatic
      ) || first.initializers.filterNot(_.isStatic) != second.initializers.filterNot(_.isStatic)
    )
      throw new Exception(s"Different initializers ${first.initializers} != ${second.initializers}")

    checkAndThrowIfDiff("Different constructors", first.constructors, second.constructors)

    checkAndThrowIfDiffForMethods(first.methods, second.methods)
    checkAndThrowIfDiffForSignatures("Different properties", first.properties, second.properties)
    checkAndThrowIfDiffForSignatures("Different fields", first.fields, second.fields)

    compareInnerClasses(innerClassTypeDeclarations(first), innerClassTypeDeclarations(second))

    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(first),
      innerInterfaceTypeDeclarations(second)
    )

    compareInnerEnums(innerEnumTypeDeclarations(first), innerEnumTypeDeclarations(second))

  }

  private def compareInterfaceTypeDeclarations(
    first: TestInterfaceTypeDeclaration,
    second: TestInterfaceTypeDeclaration
  ): Unit = {

    checkAndThrowIfDiff("Different Annotations", first.annotations, second.annotations)
    checkAndThrowIfDiff("Different modifiers", first.modifiers, second.modifiers)

    if (first.id != second.id) {
      throw new Exception(s"Different or empty interface id ${first.id} != ${second.id}")
    }

    if (first.implementsTypeList != second.implementsTypeList) {
      throw new Exception(
        s"Different extends ${first.implementsTypeList} != ${second.implementsTypeList}"
      )
    }

    checkAndThrowIfDiffForMethods(first.methods, second.methods)
  }

  private def compareEnumTypeDeclarations(
    first: TestEnumTypeDeclaration,
    second: TestEnumTypeDeclaration
  ): Unit = {

    checkAndThrowIfDiff("Different Annotations", first.annotations, second.annotations)
    checkAndThrowIfDiff("Different modifiers", first.modifiers, second.modifiers)

    if (first.id != second.id) {
      throw new Exception(s"Different or empty enum id ${first.id} != ${second.id}")
    }

    if (
      !(first.fields
        .forall(second.fields.contains) && second.fields.forall(first.fields.contains))
    ) {
      throw new Exception(s"Different constants ${first.fields} != ${second.fields}")
    }
  }

  private def prettyWarnings[T](msg: String, first: ArraySeq[T], second: ArraySeq[T]): String = {
    s"""$msg
       |FirstTypes: $first
       |SecondTypes: $second
       |""".stripMargin
  }

  private def getDiffIfThereIsAny[T](
    first: ArraySeq[T],
    second: ArraySeq[T]
  ): (Boolean, ArraySeq[T], ArraySeq[T]) = {
    val diff = first.filterNot(second.contains)
    if (diff.nonEmpty) {
      val otherDiff = second.filterNot(first.contains)
      return (true, diff, otherDiff)
    }
    (false, ArraySeq(), ArraySeq())
  }

  private def getTypeArgumentTypeRefs(typ: UnresolvedTypeRef): ArraySeq[TypeRef] = {
    ArraySeq.unsafeWrapArray(
      typ.typeNameSegments
        .flatMap(_.typeArguments)
    )
  }

  private def compareTypeRef(first: Option[TypeRef], second: Option[TypeRef]): Boolean = {
    first match {
      case Some(fType) =>
        if (second.nonEmpty) {
          return compareTypeRef(fType, second.get)
        }
        false
      case None => second.isEmpty
    }

  }

  private def compareTypeRef(first: TypeRef, second: TypeRef): Boolean = {
    def checkTypeArguments(first: UnresolvedTypeRef, second: UnresolvedTypeRef): Boolean = {
      val firstTypArgumentTypeRefs  = getTypeArgumentTypeRefs(first)
      val secondTypArgumentTypeRefs = getTypeArgumentTypeRefs(second)
      (firstTypArgumentTypeRefs.isEmpty && secondTypArgumentTypeRefs.isEmpty) || areTypeRefsSubsets(
        firstTypArgumentTypeRefs,
        secondTypArgumentTypeRefs
      )
    }

    if (first == second) {
      return true
    }

    first match {
      case td: ITestTypeDeclaration => return td.toString == second.toString
      case _                        =>
    }

    val fUnresolvedType = first.asInstanceOf[UnresolvedTypeRef]
    val sUnresolvedType = second.asInstanceOf[UnresolvedTypeRef]
    val fTypeNamesRemovedResolvedTypes =
      fUnresolvedType.typeNameSegments.filterNot(f => fResolver.get.canBeResolved(f.id))
    val sTypeNamesRemovedResolvedTypes =
      sUnresolvedType.typeNameSegments.filterNot(s => sResolver.get.canBeResolved(s.id))
    //if fTypeNamesRemovedResolvedTypes and sTypeNamesRemovedResolvedTypes are empty then all the types could be resolved
    var checks =
      (fTypeNamesRemovedResolvedTypes sameElements sTypeNamesRemovedResolvedTypes) && fUnresolvedType.arraySubscripts == sUnresolvedType.arraySubscripts
    if (sTypeNamesRemovedResolvedTypes.isEmpty && fTypeNamesRemovedResolvedTypes.nonEmpty) {
      // we have all resolved types in second
      checks = checkTypeArguments(
        fUnresolvedType,
        sUnresolvedType
      ) && fUnresolvedType.arraySubscripts == sUnresolvedType.arraySubscripts
      if (checks)
        warnings.append(
          prettyWarnings(
            "TypeRefs in second has all names fully resolved the other does not",
            ArraySeq(first),
            ArraySeq(second)
          )
        )
    }
    if (!checks) {
      if (
        fUnresolvedType.typeNameSegments.map(_.id) sameElements sUnresolvedType.typeNameSegments
          .map(_.id)
      ) {
        //Type arguments for type names must have failed
        checks = checkTypeArguments(fUnresolvedType, sUnresolvedType)
      } else {
        checks = compareListAdnArraySubscriptIfAny(first, second)
        if (checks) {
          warnings.append(
            prettyWarnings(
              "TypeRef Array Subscript resolved to List in other",
              ArraySeq(first),
              ArraySeq(second)
            )
          )
        }
      }
    }
    checks
  }

  private def compareListAdnArraySubscriptIfAny(first: TypeRef, second: TypeRef): Boolean = {
    //Check if type is array subscript and the other has matching number of List type
    first match {
      case td: ITestTypeDeclaration => return td.toString == second.toString
      case _                        =>
    }
    //TODO we need to compare typerefs again make sure the other are matching
    val fUnresolvedType = first.asInstanceOf[UnresolvedTypeRef]
    val sUnresolvedType = second.asInstanceOf[UnresolvedTypeRef]

    if (fUnresolvedType.arraySubscripts != 0) {
      val allTypeNames = ArrayBuffer[TypeNameSegment]()
      val typeRefQueue = mutable.Queue[TypeRef]()

      getTypeArgumentTypeRefs(sUnresolvedType).foreach(typeRefQueue.append)
      sUnresolvedType.typeNameSegments.foreach(allTypeNames.append)
      while (typeRefQueue.nonEmpty) {
        typeRefQueue
          .clone()
          .foreach(x => {
            val unresolvedX = x.asInstanceOf[UnresolvedTypeRef]
            getTypeArgumentTypeRefs(unresolvedX).foreach(typeRefQueue.append)
            unresolvedX.typeNameSegments.foreach(allTypeNames.append)
            typeRefQueue.dequeue()
          })
      }
      return allTypeNames
        .map(_.id.lowerCaseName)
        .count(_.equalsIgnoreCase("list")) == fUnresolvedType.arraySubscripts
    }
    false
  }

  private def areTypeRefsSubsets(first: ArraySeq[TypeRef], second: ArraySeq[TypeRef]): Boolean = {
    val isSubset = first.nonEmpty && first.size == second.size &&
      first.forall(f => second.exists(s => compareTypeRef(f, s)))
    isSubset
  }

  private def subsetCompare[T <: IVariable](
    first: ArraySeq[T],
    second: ArraySeq[T]
  ): (Boolean, ArraySeq[T], ArraySeq[T]) = {
    val (_, firstDiff, secondDiff) = getDiffIfThereIsAny(first, second)
    var check                      = firstDiff.isEmpty && secondDiff.isEmpty
    if (!check) {
      //They are not exactly equal so we fall back to check the subset
      val nonTypRefSubsetTypes =
        firstDiff.filterNot(f => findAndCheckTypeRefSubSet(f, secondDiff))
      check = nonTypRefSubsetTypes.isEmpty
      if (check) {
        warnings.append(
          prettyWarnings(
            "Warning: Some Types are not strictly equal, but are subsets",
            firstDiff,
            secondDiff
          )
        )
      }
      return (
        check,
        nonTypRefSubsetTypes,
        secondDiff.filter(s => nonTypRefSubsetTypes.exists(f => f.id == s.id))
      )
    }

    (check, ArraySeq(), ArraySeq())
  }

  private def findAndCheckTypeRefSubSet[T <: IVariable](
    firstSig: T,
    secondDiff: ArraySeq[T]
  ): Boolean = {
    secondDiff.find(firstSig.id == _.id) match {
      case Some(secondSig) =>
        val typeRefIdCheck = compareTypeRef(firstSig.typeRef, secondSig.typeRef)
        typeRefIdCheck &&
        (firstSig.annotations sameElements secondSig.annotations) &&
        (firstSig.modifiers sameElements secondSig.modifiers)
      case _ => false
    }
  }

  private def findAndCompareMethods(
    first: MethodDeclaration,
    second: ArraySeq[MethodDeclaration]
  ) = {
    val numberOfParams = first.formalParameters.length
    val possibleMethods = second.filter(
      s =>
        s.formalParameters.length ==
          numberOfParams
    )
    possibleMethods.nonEmpty && possibleMethods.exists(secondMethod => {
      val typeRefIdCheck = compareTypeRef(first.typeRef, secondMethod.typeRef)
      typeRefIdCheck && checkForParameterListAgainst(
        first.formalParameters,
        secondMethod.formalParameters
      ) &&
      (first.annotations sameElements secondMethod.annotations) &&
      (first.modifiers sameElements secondMethod.modifiers)
    })
  }

  private def checkForParameterListAgainst(
    first: ArraySeq[FormalParameter],
    second: ArraySeq[FormalParameter]
  ): Boolean = {
    if (first.nonEmpty) {
      return second.nonEmpty && first.zipWithIndex.forall(f => {
        val s = second(f._2)
        compareTypeRef(f._1.typeRef, s.typeRef) && !getDiffIfThereIsAny(
          ArraySeq.unsafeWrapArray(f._1.annotations),
          ArraySeq.unsafeWrapArray(s.annotations)
        )._1 && !getDiffIfThereIsAny(
          ArraySeq.unsafeWrapArray(f._1.modifiers),
          ArraySeq.unsafeWrapArray(s.modifiers)
        )._1 && s.name == f._1.name
      })
    }
    second.isEmpty
  }

  private def checkAndThrowIfDiffForMethods(
    first: ArraySeq[MethodDeclaration],
    second: ArraySeq[MethodDeclaration]
  ): Unit = {
    val (_, firstDiff, secondDiff) = getDiffIfThereIsAny(first, second)
    var check                      = firstDiff.isEmpty && secondDiff.isEmpty
    if (check) {
      return
    }
    check = firstDiff.forall(m => findAndCompareMethods(m, second))
    if (!check)
      throw new Exception(s"Different methods $firstDiff != $secondDiff")
    else
      warnings.append(
        prettyWarnings(
          "Warning: Some Types are not strictly equal, but are subsets",
          firstDiff,
          secondDiff
        )
      )
  }

  private def checkAndThrowIfDiffForSignatures[T <: IVariable](
    errorMsg: String,
    first: ArraySeq[T],
    second: ArraySeq[T]
  ): Unit = {
    val (areSubsets, failedFirst, failedSecond) = subsetCompare(first, second)
    if (!areSubsets) {
      throw new Exception(s"$errorMsg $failedFirst != $failedSecond")
    }
  }

  private def checkAndThrowIfDiff[T](
    errorMsg: String,
    first: ArraySeq[T],
    second: ArraySeq[T]
  ): Unit = {
    val (isDiff, firstDiff, secondDiff) = getDiffIfThereIsAny(first, second)
    if (isDiff) {
      throw new Exception(s"$errorMsg: $firstDiff != $secondDiff")
    }
  }

  private def checkAndThrowIfDiff[T](errorMsg: String, first: Array[T], second: Array[T]): Unit = {
    checkAndThrowIfDiff(errorMsg, ArraySeq.unsafeWrapArray(first), ArraySeq.unsafeWrapArray(second))
  }

}

object SubsetComparator {

  def apply(
    first: ITestTypeDeclaration,
    fResolver: TypeIdResolver,
    sResolver: TypeIdResolver
  ): SubsetComparator = {
    new SubsetComparator(first, Some(fResolver), Some(sResolver))
  }

  def apply(first: ITestTypeDeclaration): SubsetComparator = {
    new SubsetComparator(first, None, None)
  }
}
