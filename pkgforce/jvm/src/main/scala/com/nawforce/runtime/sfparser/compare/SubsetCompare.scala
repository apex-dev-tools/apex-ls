/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser.compare

import com.financialforce.oparser._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SubsetCompare(
  firstTd: TypeDeclaration,
  secondTd: TypeDeclaration,
  fResolver: TypeIdResolver,
  sResolver: TypeIdResolver
) {
  private val warnings: ArrayBuffer[String] = ArrayBuffer()

  def getWarnings: Set[String] = {
    warnings.toSet
  }

  def clearWarnings(): Unit = {
    warnings.clear()
  }

  def compare(): Unit = {
    firstTd match {
      case cls: ClassTypeDeclaration =>
        subsetOffClassDeclarations(cls, secondTd.asInstanceOf[ClassTypeDeclaration])
      case int: InterfaceTypeDeclaration =>
        compareInterfaceTypeDeclarations(int, secondTd.asInstanceOf[InterfaceTypeDeclaration])
      case enm: EnumTypeDeclaration =>
        compareEnumTypeDeclarations(enm, secondTd.asInstanceOf[EnumTypeDeclaration])
      case _ =>
    }
  }

  def subsetOffClassDeclarations(
    first: ClassTypeDeclaration,
    second: ClassTypeDeclaration
  ): Unit = {
    def innerClassTypeDeclarations(o: ClassTypeDeclaration): Seq[ClassTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[ClassTypeDeclaration])
        .map(_.asInstanceOf[ClassTypeDeclaration])
        .toIndexedSeq
    }

    def innerInterfaceTypeDeclarations(o: ClassTypeDeclaration): Seq[InterfaceTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[InterfaceTypeDeclaration])
        .map(_.asInstanceOf[InterfaceTypeDeclaration])
        .toIndexedSeq
    }

    def innerEnumTypeDeclarations(o: ClassTypeDeclaration): Seq[EnumTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[EnumTypeDeclaration])
        .map(_.asInstanceOf[EnumTypeDeclaration])
        .toIndexedSeq
    }

    def compareInnerClasses(
      fst: Seq[ClassTypeDeclaration],
      snd: Seq[ClassTypeDeclaration]
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
      fst: Seq[InterfaceTypeDeclaration],
      snd: Seq[InterfaceTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner interface not found ${f.id} in ${first.id}")
        }
        compareInterfaceTypeDeclarations(f, sOpt.get)
      })
    }

    def compareInnerEnums(fst: Seq[EnumTypeDeclaration], snd: Seq[EnumTypeDeclaration]): Unit = {

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

    if (first.id != second.id || first.id.isEmpty) {
      throw new Exception(s"Different or empty class id ${first.id} != ${second.id}")
    }

    if (first.typeParameters != second.typeParameters) {
      throw new Exception(
        s"Different typeParameters ${first.typeParameters} != ${second.typeParameters}"
      )
    }

    if (first.extendsTypeRef != second.extendsTypeRef) {
      throw new Exception(s"Different extends ${first.extendsTypeRef} != ${second.extendsTypeRef}")
    }

    if (first.implementsTypeList != second.implementsTypeList) {
      val firstTypeRefs  = first.implementsTypeList.map(_.typeRefs).get
      val secondTypeRefs = second.implementsTypeList.map(_.typeRefs).get
      val isSubset       = areTypeRefsSubsets(firstTypeRefs, secondTypeRefs)
      if (!isSubset)
        throw new Exception(
          s"Different implements ${first.implementsTypeList} != ${second.implementsTypeList}"
        )
      else
        warnings.append(
          prettyWarnings(
            "Waring: implementsTypeList not strictly equal but are subsets",
            ArrayBuffer(first.implementsTypeList.get),
            ArrayBuffer(second.implementsTypeList.get)
          )
        )
    }

    //TODO: Initializer checks

    checkAndThrowIfDiff("Different constructors", first.constructors, second.constructors)
    checkAndThrowIfDiffForSignatures("Different methods", first.methods, second.methods)
    checkAndThrowIfDiffForSignatures("Different properties", first.properties, second.properties)
    checkAndThrowIfDiffForSignatures("Different fields", first.fields, second.fields)

    compareInnerClasses(innerClassTypeDeclarations(first), innerClassTypeDeclarations(second))

    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(first),
      innerInterfaceTypeDeclarations(second)
    )

    compareInnerEnums(innerEnumTypeDeclarations(first), innerEnumTypeDeclarations(second))

  }

  def compareInterfaceTypeDeclarations(
    first: InterfaceTypeDeclaration,
    second: InterfaceTypeDeclaration
  ): Unit = {

    checkAndThrowIfDiff("Different Annotations", first.annotations, second.annotations)
    checkAndThrowIfDiff("Different modifiers", first.modifiers, second.modifiers)

    if (first.id != second.id || first.id.isEmpty) {
      throw new Exception(s"Different or empty interface id ${first.id} != ${second.id}")
    }

    if (first.extendsTypeList != second.extendsTypeList) {
      throw new Exception(
        s"Different extends ${first.extendsTypeList} != ${second.extendsTypeList}"
      )
    }
    checkAndThrowIfDiffForSignatures("Different methods", first.methods, second.methods)
  }

  def compareEnumTypeDeclarations(first: EnumTypeDeclaration, second: EnumTypeDeclaration): Unit = {

    checkAndThrowIfDiff("Different Annotations", first.annotations, second.annotations)
    checkAndThrowIfDiff("Different modifiers", first.modifiers, second.modifiers)

    if (first.id != second.id || first.id.isEmpty) {
      throw new Exception(s"Different or empty enum id ${first.id} != ${second.id}")
    }

    if (
      !(first.constants
        .forall(second.constants.contains) && second.constants.forall(first.constants.contains))
    ) {
      throw new Exception(s"Different constants ${first.constants} != ${second.constants}")
    }
  }

  private def prettyWarnings[T](
    msg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): String = {
    s"""$msg
       |FirstTypes: $first
       |SecondTypes: $second
       |""".stripMargin
  }

  private def getDiffIfThereIsAny[T](
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): (Boolean, ArrayBuffer[T], ArrayBuffer[T]) = {
    val diff = first.filterNot(second.contains)
    if (diff.nonEmpty) {
      val otherDiff = second.filterNot(first.contains)
      return (true, diff, otherDiff)
    }
    (false, ArrayBuffer(), ArrayBuffer())
  }

  private def getTypeArgumentTypeRefs(typ: TypeRef): ArrayBuffer[TypeRef] = {
    typ.typeNames
      .flatMap(_.typeArguments)
      .flatMap(_.typeList)
      .flatMap(_.typeRefs)
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
    if (first == second) {
      return true
    }

    val fTypeNamesRemovedResolvedTypes =
      first.typeNames.filterNot(f => fResolver.canBeResolved(f.id))
    val sTypeNamesRemovedResolvedTypes =
      second.typeNames.filterNot(s => sResolver.canBeResolved(s.id))
    //if fTypeNamesRemovedResolvedTypes and sTypeNamesRemovedResolvedTypes are empty then all the types could be resolved
    var checks =
      fTypeNamesRemovedResolvedTypes == sTypeNamesRemovedResolvedTypes && first.arraySubscripts == second.arraySubscripts
    if (sTypeNamesRemovedResolvedTypes.isEmpty && fTypeNamesRemovedResolvedTypes.nonEmpty) {
      checks = true
      warnings.append(
        prettyWarnings(
          "TypeRef has resolved resolved FQD name and the other doesn't",
          ArrayBuffer(first),
          ArrayBuffer(second)
        )
      )
    }
    if (!checks) {
      if (first.typeNames.map(_.id) == second.typeNames.map(_.id)) {
        //Type arguments for type names must have failed
        val firstTypArgumentTypeRefs  = getTypeArgumentTypeRefs(first)
        val secondTypArgumentTypeRefs = getTypeArgumentTypeRefs(second)
        checks = areTypeRefsSubsets(firstTypArgumentTypeRefs, secondTypArgumentTypeRefs)
      } else {
        checks = compareListAdnArraySubscriptIfAny(first, second)
        if (checks) {
          warnings.append(
            prettyWarnings(
              "TypeRef Array Subscript resolved to List in other",
              ArrayBuffer(first),
              ArrayBuffer(second)
            )
          )
        }
      }
    }
    checks
  }

  private def compareListAdnArraySubscriptIfAny(first: TypeRef, second: TypeRef): Boolean = {
    //Check if type is array subscript and the other has matching number of List type
    //TODO we need to compare typerefs again make sure the other are matching
    if (first.arraySubscripts.nonEmpty) {
      val allTypeNames = ArrayBuffer[TypeName]()
      val typeRefQueue = mutable.Queue[TypeRef]()

      getTypeArgumentTypeRefs(second).foreach(typeRefQueue.append)
      second.typeNames.foreach(allTypeNames.append)
      while (typeRefQueue.nonEmpty) {
        typeRefQueue
          .clone()
          .foreach(x => {
            getTypeArgumentTypeRefs(x).foreach(typeRefQueue.append)
            x.typeNames.foreach(allTypeNames.append)
            typeRefQueue.dequeue()
          })
      }
      return allTypeNames
        .map(_.id.id.lowerCaseContents)
        .count(_.equalsIgnoreCase("list")) == first.arraySubscripts.length
    }
    false
  }

  private def areTypeRefsSubsets(
    first: ArrayBuffer[TypeRef],
    second: ArrayBuffer[TypeRef]
  ): Boolean = {
    val isSubset = first.nonEmpty &&
      first.forall(f => second.exists(s => compareTypeRef(f, s)))
    isSubset
  }

  private def subsetCompare[T <: Signature](
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): (Boolean, ArrayBuffer[T], ArrayBuffer[T]) = {
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

    (check, ArrayBuffer(), ArrayBuffer())
  }

  private def findAndCheckTypeRefSubSet[T <: Signature](
    firstSig: T,
    secondDiff: ArrayBuffer[T]
  ): Boolean = {

    firstSig match {
      case fMethod: MethodDeclaration =>
        findAndCompareMethods(fMethod, secondDiff.asInstanceOf[ArrayBuffer[MethodDeclaration]])
      case _ =>
        secondDiff.find(firstSig.id == _.id) match {
          case Some(secondSig) =>
            val typeRefIdCheck = compareTypeRef(firstSig.typeRef, secondSig.typeRef)
            typeRefIdCheck && firstSig.annotations == secondSig.annotations && firstSig.modifiers == secondSig.modifiers
          case _ => false
        }
    }
  }

  private def findAndCompareMethods(
    first: MethodDeclaration,
    second: ArrayBuffer[MethodDeclaration]
  ) = {
    val numberOfParams = first.formalParameterList.formalParameters.length
    val possibleMethods = second.filter(
      s =>
        s.asInstanceOf[SignatureWithParameterList].formalParameterList.formalParameters.length ==
          numberOfParams
    )
    possibleMethods.nonEmpty && possibleMethods.exists(secondMethod => {
      val typeRefIdCheck = compareTypeRef(first.typeRef, secondMethod.typeRef)
      typeRefIdCheck && checkForParameterListAgainst(
        first.formalParameterList,
        secondMethod.formalParameterList
      ) && first.annotations == secondMethod.annotations && first.modifiers == secondMethod.modifiers
    })
  }

  private def checkForParameterListAgainst(
    first: FormalParameterList,
    second: FormalParameterList
  ): Boolean = {
    if (first.formalParameters.nonEmpty) {
      return second.formalParameters.nonEmpty && first.formalParameters.zipWithIndex.forall(f => {
        val s = second.formalParameters(f._2)
        compareTypeRef(f._1.typeRef, s.typeRef) && !getDiffIfThereIsAny(
          f._1.annotations,
          s.annotations
        )._1 && !getDiffIfThereIsAny(f._1.modifiers, s.modifiers)._1 && s.id == f._1.id
      })
    }
    second.formalParameters.isEmpty
  }

  private def checkAndThrowIfDiffForSignatures[T <: Signature](
    errorMsg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Unit = {
    val (areSubsets, failedFirst, failedSecond) = subsetCompare(first, second)
    if (!areSubsets) {
      throw new Exception(s"$errorMsg $failedFirst != $failedSecond")
    }
  }

  private def checkAndThrowIfDiff[T](
    errorMsg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Unit = {
    val (isDiff, firstDiff, secondDiff) = getDiffIfThereIsAny(first, second)
    if (isDiff) {
      throw new Exception(s"$errorMsg: $firstDiff != $secondDiff")
    }
  }

}

object SubsetCompare {

  def apply(
    first: TypeDeclaration,
    second: TypeDeclaration,
    fResolver: TypeIdResolver,
    sResolver: TypeIdResolver
  ): SubsetCompare = {
    new SubsetCompare(first, second, fResolver, sResolver)
  }

  def apply(first: TypeDeclaration, second: TypeDeclaration): SubsetCompare = {
    val fResolver = new TypeIdCollector(List(first))
    val sResolver = new TypeIdCollector(List(second))
    new SubsetCompare(first, second, fResolver, sResolver)
  }
}
