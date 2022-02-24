package com.nawforce.runtime.sfparser

import com.financialforce.oparser.{
  ClassTypeDeclaration,
  EnumTypeDeclaration,
  Id,
  InterfaceTypeDeclaration,
  Signature,
  SignatureWithParameterList,
  TypeRef
}

import scala.collection.mutable.ArrayBuffer

object SubsetCompare {
  private val warnings: ArrayBuffer[String] = ArrayBuffer()

  def getWarnings(): Set[String] = {
    warnings.toSet
  }

  def clearWarnings(): Unit = {
    warnings.clear()
  }

  def subsetOffClassDeclarations(
    first: ClassTypeDeclaration,
    second: ClassTypeDeclaration,
    fInnerIds: ArrayBuffer[Id] = ArrayBuffer(),
    sInnerIds: ArrayBuffer[Id] = ArrayBuffer()
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
      fInrIds: ArrayBuffer[Id],
      sInrIds: ArrayBuffer[Id],
      fst: Seq[ClassTypeDeclaration],
      snd: Seq[ClassTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner class not found ${f.id} in ${first.id}")
        }
        subsetOffClassDeclarations(f, sOpt.get, fInrIds, sInrIds)
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
    val fInIds = first.innerTypes.flatMap(_.id).append(first.id.get)
    val sInIds = second.innerTypes.flatMap(_.id).append(first.id.get)
    fInnerIds.foreach(fInIds.append)
    sInnerIds.foreach(sInIds.append)

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
      val isSubset       = areTypeRefsSubsets(fInIds, sInIds, firstTypeRefs, secondTypeRefs)
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
    checkAndThrowIfDiffForSignatures(
      fInIds,
      sInIds,
      "Different methods",
      first.methods,
      second.methods
    )
    checkAndThrowIfDiffForSignatures(
      fInIds,
      sInIds,
      "Different properties",
      first.properties,
      second.properties
    )
    checkAndThrowIfDiffForSignatures(
      fInIds,
      sInIds,
      "Different fields",
      first.fields,
      second.fields
    )

    compareInnerClasses(
      fInIds,
      sInIds,
      innerClassTypeDeclarations(first),
      innerClassTypeDeclarations(second)
    )

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
    checkAndThrowIfDiffForSignatures(
      ArrayBuffer(),
      ArrayBuffer(),
      "Different methods",
      first.methods,
      second.methods
    )
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
  ): (ArrayBuffer[T], ArrayBuffer[T]) = {
    val diff = first.filterNot(second.contains)
    if (diff.nonEmpty) {
      val otherDiff = second.filterNot(first.contains)
      return (diff, otherDiff)
    }
    (ArrayBuffer(), ArrayBuffer())
  }

  private def compareTypeRef(
    firstIds: ArrayBuffer[Id],
    secondIds: ArrayBuffer[Id],
    first: TypeRef,
    second: TypeRef
  ): Boolean = {
    def getTypeArguments(typ: TypeRef): ArrayBuffer[TypeRef] = {
      typ.typeNames
        .flatMap(_.typeArguments)
        .flatMap(_.typeList)
        .flatMap(_.typeRefs)
    }

    if (first == second) {
      return true
    }
    val fTypeNamesRemovedInnerTypes = first.typeNames.filterNot(f => firstIds.contains(f.id))
    val sTypeNamesRemovedInnerTypes = second.typeNames.filterNot(s => secondIds.contains(s.id))
    //if fTypeNamesRemovedInnerTypes and sTypeNamesRemovedInnerTypes are empty then all the types are from inner types
    var checks =
      fTypeNamesRemovedInnerTypes == sTypeNamesRemovedInnerTypes && first.arraySubscripts == second.arraySubscripts
    if (!checks) {
      if (first.typeNames.map(_.id) == second.typeNames.map(_.id)) {
        //Type arguments for type names must have failed
        val firstTypArgumentTypeRefs  = getTypeArguments(first)
        val secondTypArgumentTypeRefs = getTypeArguments(second)
        checks = areTypeRefsSubsets(
          firstIds,
          secondIds,
          firstTypArgumentTypeRefs,
          secondTypArgumentTypeRefs
        )
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
    //Check if type is array subscript and the other has a List type
    //TODO: this doesn't doo deep list checks
    first.arraySubscripts.nonEmpty && second.typeNames
      .map(_.id.id.contents)
      .contains("List")
    //TODO we need to check the others values again ignoring this list and substring stuff
  }

  private def areTypeRefsSubsets(
    firstIds: ArrayBuffer[Id],
    secondIds: ArrayBuffer[Id],
    first: ArrayBuffer[TypeRef],
    second: ArrayBuffer[TypeRef]
  ): Boolean = {
    val isSubset = first.nonEmpty &&
      first.forall(f => second.exists(s => compareTypeRef(firstIds, secondIds, f, s)))
    isSubset
  }

  private def compareForTypeRefAndId[T <: Signature](
    fInnerIds: ArrayBuffer[Id],
    sInnerIds: ArrayBuffer[Id],
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Boolean = {
    val (firstDiff, secondDiff) = getDiffIfThereIsAny(first, second)
    var check                   = firstDiff.isEmpty && secondDiff.isEmpty
    if (!check) {
      //They are not exactly equal so we fall back to check the subset
      check = firstDiff.forall(f => findAndCheckTypeRefSubSet(fInnerIds, sInnerIds, f, secondDiff))
      if (check) {
        warnings.append(
          prettyWarnings(
            "Warning: Some Types are not strictly equal, but are subsets",
            firstDiff,
            secondDiff
          )
        )
      }
    }

    check
  }

  private def findAndCheckTypeRefSubSet[T <: Signature](
    fIds: ArrayBuffer[Id],
    sIds: ArrayBuffer[Id],
    firstSig: T,
    secondDiff: ArrayBuffer[T]
  ): Boolean = {
    def checkAnnotationAgainst(s: T): Boolean = {
      val (fD, sD) = getDiffIfThereIsAny(firstSig.annotations, s.annotations)
      fD == sD
    }

    def checkForModifiersAgainst(s: T): Boolean = {
      val (fD, sD) = getDiffIfThereIsAny(firstSig.annotations, s.annotations)
      fD == sD
    }

    def checkForParameterListAgainst(s: T): Boolean = {
      firstSig.asInstanceOf[SignatureWithParameterList].formalParameterList == s
        .asInstanceOf[SignatureWithParameterList]
        .formalParameterList
    }
    firstSig match {
      case fMethod: SignatureWithParameterList =>
        val numberOfParams = fMethod.formalParameterList.formalParameters.map(_.id)
        secondDiff.find(
          s =>
            firstSig.id == s.id && s
              .asInstanceOf[SignatureWithParameterList]
              .formalParameterList
              .formalParameters
              .map(_.id)
              == numberOfParams
        ) match {
          case Some(secondSig) =>
            val typeRefIdCheck = compareTypeRef(fIds, sIds, firstSig.typeRef, secondSig.typeRef)
            typeRefIdCheck & checkForParameterListAgainst(secondSig) && checkForModifiersAgainst(
              secondSig
            ) && checkAnnotationAgainst(secondSig)
        }
      case _ =>
        secondDiff.find(firstSig.id == _.id) match {
          case Some(secondSig) =>
            val typeRefIdCheck = compareTypeRef(fIds, sIds, firstSig.typeRef, secondSig.typeRef)
            typeRefIdCheck && checkForModifiersAgainst(secondSig) && checkAnnotationAgainst(
              secondSig
            )
          case _ => false
        }
    }
  }

  private def checkAndThrowIfDiffForSignatures[T <: Signature](
    fInnerIds: ArrayBuffer[Id],
    sInnerIds: ArrayBuffer[Id],
    errorMsg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Unit = {
    if (!compareForTypeRefAndId(fInnerIds, sInnerIds, first, second)) {
      val (fDiff, sDiff) = getDiffIfThereIsAny(first, second)
      throw new Exception(s"$errorMsg $fDiff != $sDiff")
    }
  }

  private def checkAndThrowIfDiff[T](
    errorMsg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Unit = {
    val (firstDiff, secondDiff) = getDiffIfThereIsAny(first, second)
    if (firstDiff.nonEmpty) {
      throw new Exception(s"${errorMsg}: ${firstDiff} != ${secondDiff}")
    }
  }

}
