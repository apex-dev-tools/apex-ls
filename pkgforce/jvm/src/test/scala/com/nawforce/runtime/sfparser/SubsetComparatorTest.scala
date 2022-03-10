package com.nawforce.runtime.sfparser

import com.financialforce.oparser.{FormalParameter, QualifiedName, _}
import com.nawforce.runtime.sfparser.compare.{SubsetComparator, TypeIdResolver}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class SubsetComparatorTest extends AnyFunSuite with DeclarationGeneratorHelper {

  private def generateClassDeclaration(path: String, name: String): ClassTypeDeclaration = {
    val ctd = new ClassTypeDeclaration(path)
    ctd.add(toId(name))
    ctd.add(toAnnotation(Array("TestVisible"), None))
    ctd.add(toModifier("private"))
    ctd.add(toTypeRef(Map("Foo" -> None, "Bar" -> None)))
    ctd.add(toTypeList(Array(toTypeRef(Map("ExtendsType" -> None)))))
    ctd.add(
      toMethodDeclaration(
        Array(toAnnotation(Array("Test"), None)),
        Array("public", "static").map(toModifier),
        toTypeRef(Map("void" -> None)),
        toId("method"),
        toParameterList(
          Array(
            toParameter(
              Array[Annotation](),
              Array[Modifier](),
              Some(toTypeRef(Map("String" -> None))),
              Some(toId("s"))
            )
          )
        )
      )
    )
    ctd.constructors.append(
      toConstructor(
        Array(toAnnotation(Array("Test"), None)),
        Array("public").map(toModifier),
        Array(name),
        toParameterList(
          Array(
            toParameter(
              Array[Annotation](),
              Array[Modifier](),
              Some(toTypeRef(Map("String" -> None))),
              Some(toId("s"))
            )
          )
        )
      )
    )
    ctd.properties.append(
      toPropertyDeclaration(
        Array(toAnnotation(Array("Test"), None)),
        Array("private").map(toModifier),
        toTypeRef(Map("String" -> None)),
        toId("prop")
      )
    )
    ctd.fields.append(
      toFieldDeclaration(
        Array(toAnnotation(Array("Test"), None)),
        Array("private").map(toModifier),
        toTypeRef(Map("String" -> None)),
        toId("prop")
      )
    )
    ctd
  }

  private def generateEmptyClassDeclaration(path: String, name: String): ClassTypeDeclaration = {
    val ctd = new ClassTypeDeclaration(path)
    ctd.id = Some(toId(name))
    ctd
  }

  private def getMockResolver(resolvedTypes: Array[String]): TypeIdResolver = {
    new TypeIdResolver() {
      val ids: Array[Id] = resolvedTypes.map(toId)

      override def canBeResolved(id: Id): Boolean = ids.contains(id)
    }
  }

  test("No Errors when all are equal") {
    //Given
    val first  = generateClassDeclaration("Dummy.cls", "Dummy")
    val second = generateClassDeclaration("Dummy.cls", "Dummy")

    //When
    val comparator = SubsetComparator(first)
    comparator.subsetOf(second)

    //Then
    assert(comparator.getWarnings.isEmpty)
  }

  test("Annotations are Equal") {
    //Given
    val first  = generateEmptyClassDeclaration("dummy.cls", "dummy")
    val second = generateEmptyClassDeclaration("dummy.cls", "dummy")
    first.add(toAnnotation(Array("TestAnnotation"), Some("param")))
    second.add(toAnnotation(Array("TestAnnotation"), Some("param")))
    //When
    val comparator = SubsetComparator(first)
    comparator.subsetOf(second)

    //Then
    assert(comparator.getWarnings.isEmpty)
  }

  test("Annotations are not Equal") {
    //Given
    val first  = generateEmptyClassDeclaration("dummy.cls", "dummy")
    val second = generateEmptyClassDeclaration("dummy.cls", "dummy")
    first.add(toAnnotation(Array("TestAnnotation"), Some("param")))
    second.add(toAnnotation(Array("diff"), Some("param")))
    val comparator = SubsetComparator(first)

    //When //Then
    assertThrows[Exception] {
      comparator.subsetOf(second)
    }
  }

  test("Fails even when Annotations are subsets") {
    //Annotations must be exactly equal or not
    //Given
    val first  = generateEmptyClassDeclaration("dummy.cls", "dummy")
    val second = generateEmptyClassDeclaration("dummy.cls", "dummy")
    first.add(toAnnotation(Array("TestAnnotation"), None))
    second.add(toAnnotation(Array("QualifiedName", "TestAnnotation"), None))
    //When
    val comparator =
      SubsetComparator(first, getMockResolver(Array()), getMockResolver(Array("QualifiedName")))

    //Then
    assertThrows[Exception] {
      comparator.subsetOf(second)
    }
  }

  test("Modifiers are Equal") {
    //Given
    val first  = generateEmptyClassDeclaration("dummy.cls", "dummy")
    val second = generateEmptyClassDeclaration("dummy.cls", "dummy")
    Array("private", "with sharing").map(toModifier).foreach(first.add)
    Array("private", "with sharing").map(toModifier).foreach(second.add)
    //When
    val comparator = SubsetComparator(first)
    comparator.subsetOf(second)

    //Then
    assert(comparator.getWarnings.isEmpty)
  }

  test("Modifiers are not Equal") {
    //Given
    val first  = generateEmptyClassDeclaration("dummy.cls", "dummy")
    val second = generateEmptyClassDeclaration("dummy.cls", "dummy")
    Array("private", "with sharing").map(toModifier).foreach(first.add)
    Array("private", "without sharing").map(toModifier).foreach(second.add)
    //When
    val comparator = SubsetComparator(first)

    assertThrows[Exception] {
      comparator.subsetOf(second)
    }
  }

  test("Modifiers are not Equal") {
    //Given
    val first  = generateEmptyClassDeclaration("dummy.cls", "dummy")
    val second = generateEmptyClassDeclaration("dummy.cls", "dummy")
    Array("private", "with sharing").map(toModifier).foreach(first.add)
    Array("private", "without sharing").map(toModifier).foreach(second.add)
    //When
    val comparator = SubsetComparator(first)

    assertThrows[Exception] {
      comparator.subsetOf(second)
    }
  }

}
