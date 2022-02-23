package com.nawforce.runtime.sfparser

import com.financialforce.oparser.{
  ClassTypeDeclaration,
  EnumTypeDeclaration,
  Id,
  InterfaceTypeDeclaration,
  MethodDeclaration,
  TypeList,
  TypeRef,
  TypeRefAndId
}

import scala.collection.mutable.ArrayBuffer

object SubsetCompare {
  private val warnings: ArrayBuffer[String] = ArrayBuffer()

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

  private def getDiffIfTheresIsAny[T](
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

  private def compareArraysShowDiff[T](
    errorMsg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Unit = {
    val (firstDiff, secondDiff) = getDiffIfTheresIsAny(first, second)
    if (firstDiff.nonEmpty) {
      throw new Exception(s"${errorMsg}: ${firstDiff} != ${secondDiff}")
    }
  }

  private def checkTypeRefSubsets(
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
      //Type arguments for type names must have failed
      val firstTypArgumentTypeRefs  = getTypeArguments(first)
      val secondTypArgumentTypeRefs = getTypeArguments(second)
      checks =
        checkTypeRefSubset(firstIds, secondIds, firstTypArgumentTypeRefs, secondTypArgumentTypeRefs)
      if (!checks) {
        //Check if type is array subscript and the other has a List type
        checks = (first.arraySubscripts.nonEmpty && second.typeNames
          .map(_.id.id.contents)
          .contains("List")) || (second.arraySubscripts.nonEmpty && first.typeNames
          .map(_.id.id.contents)
          .contains("List"))
        if (checks)
          warnings.append(
            prettyWarnings(
              "TypeRef Array Subscript resolved to List in other",
              ArrayBuffer(first),
              ArrayBuffer(second)
            )
          )
      }
    }
    checks
  }

  private def checkTypeRefSubset(
    firstIds: ArrayBuffer[Id],
    secondIds: ArrayBuffer[Id],
    first: ArrayBuffer[TypeRef],
    second: ArrayBuffer[TypeRef]
  ): Boolean = {
    val isSubset =
      first.forall(x => second.exists(s => checkTypeRefSubsets(firstIds, secondIds, s, x)))
    isSubset
  }

  private def compareForTypeRefAndId[T <: TypeRefAndId](
    fInnerIds: ArrayBuffer[Id],
    sInnerIds: ArrayBuffer[Id],
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ): Boolean = {
    val (firstDiff, secondDiff) = getDiffIfTheresIsAny(first, second)
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

  private def findAndCheckTypeRefSubSet[T <: TypeRefAndId](
    fIds: ArrayBuffer[Id],
    sIds: ArrayBuffer[Id],
    f: T,
    secondDiff: ArrayBuffer[T]
  ): Boolean = {
    val eleFromDiff = secondDiff.find(f.id == _.id)
    if (eleFromDiff.nonEmpty) {
      val fModifiers   = f.modifiers
      val sModifiers   = eleFromDiff.get.modifiers
      val fAnnotations = f.annotations
      val sAnnotations = eleFromDiff.get.annotations
      return checkTypeRefSubsets(
        fIds,
        sIds,
        f.typeRef,
        eleFromDiff.get.typeRef
      ) && fModifiers == sModifiers && fAnnotations == sAnnotations
    }
    false
  }

  private def checkAndThrowIfDiff[T <: TypeRefAndId](
    fInnerIds: ArrayBuffer[Id],
    sInnerIds: ArrayBuffer[Id],
    errorMsg: String,
    first: ArrayBuffer[T],
    second: ArrayBuffer[T]
  ):Unit = {
    if (!compareForTypeRefAndId(fInnerIds, sInnerIds, first, second)) {
      val (fDiff, sDiff) = getDiffIfTheresIsAny(first, second)
      throw new Exception(s"$errorMsg $fDiff != $sDiff")
    }
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

    def compareMethods(
      fInnerIds: ArrayBuffer[Id],
      sInnerIds: ArrayBuffer[Id],
      errorMsg: String,
      first: ArrayBuffer[MethodDeclaration],
      second: ArrayBuffer[MethodDeclaration]
    ): Unit = {
      val fList = first.map(_.formalParameterList)
      val sList = second.map(_.formalParameterList)

      if (!compareForTypeRefAndId(fInnerIds, sInnerIds, first, second) && fList == sList) {
        val (fDiff, sDiff) = getDiffIfTheresIsAny(first, second)
        throw new Exception(s"$errorMsg $fDiff != $sDiff")
      }
    }

    //Start comparing
    val fInIds = first.innerTypes.flatMap(_.id).append(first.id.get)
    val sInIds = second.innerTypes.flatMap(_.id).append(first.id.get)
    fInnerIds.foreach(fInIds.append)
    sInnerIds.foreach(sInIds.append)

    compareArraysShowDiff("Different Annotations", first.annotations, second.annotations)
    compareArraysShowDiff("Different modifiers", first.modifiers, second.modifiers)

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
      val isSubset       = checkTypeRefSubset(fInIds, sInIds, firstTypeRefs, secondTypeRefs)
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

    compareArraysShowDiff("Different constructors", first.constructors, second.constructors)
    compareMethods(fInIds, sInIds, "Different methods", first.methods, second.methods)
    checkAndThrowIfDiff(fInIds, sInIds, "Different properties", first.properties, second.properties)
    checkAndThrowIfDiff(fInIds, sInIds, "Different fields", first.fields, second.fields)

    compareInnerClasses(
      fInIds,
      sInIds,
      innerClassTypeDeclarations(first),
      innerClassTypeDeclarations(second)
    )
    compareInnerClasses(
      fInIds,
      sInIds,
      innerClassTypeDeclarations(second),
      innerClassTypeDeclarations(first)
    )

    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(first),
      innerInterfaceTypeDeclarations(second)
    )
    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(second),
      innerInterfaceTypeDeclarations(first)
    )

    compareInnerEnums(innerEnumTypeDeclarations(first), innerEnumTypeDeclarations(second))
    compareInnerEnums(innerEnumTypeDeclarations(second), innerEnumTypeDeclarations(first))

  }

  def compareInterfaceTypeDeclarations(
    first: InterfaceTypeDeclaration,
    second: InterfaceTypeDeclaration
  ): Unit = {

    compareArraysShowDiff("Different Annotations", first.annotations, second.annotations)
    compareArraysShowDiff("Different modifiers", first.modifiers, second.modifiers)

    if (first.id != second.id || first.id.isEmpty) {
      throw new Exception(s"Different or empty interface id ${first.id} != ${second.id}")
    }

    if (first.extendsTypeList != second.extendsTypeList) {
      throw new Exception(
        s"Different extends ${first.extendsTypeList} != ${second.extendsTypeList}"
      )
    }
    checkAndThrowIfDiff(
      ArrayBuffer(),
      ArrayBuffer(),
      "Different methods",
      first.methods,
      second.methods
    )
  }

  def compareEnumTypeDeclarations(first: EnumTypeDeclaration, second: EnumTypeDeclaration): Unit = {

    compareArraysShowDiff("Different Annotations", first.annotations, second.annotations)
    compareArraysShowDiff("Different modifiers", first.modifiers, second.modifiers)

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

  def getWarnings: Set[String] = {
    warnings.toSet
  }

  def clearWarnings(): Unit = {
    warnings.clear()
  }

}
