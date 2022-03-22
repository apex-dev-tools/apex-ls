package com.nawforce.runtime.sfparser

import com.financialforce.oparser._
import com.nawforce.runtime.sfparser.compare.{SubsetComparator, TypeIdResolver}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, ModuleClassTypeDeclaration}
import org.scalatest.funspec.AnyFunSpec

class SubsetComparatorTest extends AnyFunSpec with DeclarationGeneratorHelper {

  private def generateClassDeclaration(name: String): ClassTypeDeclaration = {
    val ctd = new ModuleClassTypeDeclaration("path/Dummy.ls", null)
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
    ctd._constructors.append(getBasicConstructor("TestVisible", "private", name, "String", "s"))
    ctd._properties.append(
      toPropertyDeclaration(
        Array(toAnnotation(Array("Test"), None)),
        Array("private").map(toModifier),
        toTypeRef(Map("String" -> None)),
        toId("prop")
      )
    )
    ctd._fields.append(
      toFieldDeclaration(
        Array(toAnnotation(Array("Test"), None)),
        Array("private").map(toModifier),
        toTypeRef(Map("String" -> None)),
        toId("prop")
      )
    )
    ctd
  }

  private def getBasicConstructor(
    annotation: String,
    modifier: String,
    name: String,
    paramType: String,
    paramName: String
  ): ConstructorDeclaration = {
    toConstructor(
      Array(toAnnotation(Array(annotation), None)),
      Array(modifier).map(toModifier),
      Array(name),
      toParameterList(
        Array(
          toParameter(
            Array[Annotation](),
            Array[Modifier](),
            Some(toTypeRef(Map(paramType -> None))),
            Some(toId(paramName))
          )
        )
      )
    )
  }

  private def generateEmptyClassDeclaration(name: String): ClassTypeDeclaration = {
    val ctd = new ModuleClassTypeDeclaration(name, null)
    ctd._id = toId(name)
    ctd
  }

  private def getMockResolver(resolvedTypes: Array[String] = Array()): TypeIdResolver = {
    new TypeIdResolver() {
      val ids: Array[Id] = resolvedTypes.map(toId)

      override def canBeResolved(id: Id): Boolean = ids.contains(id)
    }
  }

  describe("Initializer blocks") {
    it("should be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._initializers.append(new Initializer(true))
      first._initializers.append(new Initializer(false))

      second._initializers.append(new Initializer(true))
      second._initializers.append(new Initializer(false))

      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("should be not be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._initializers.append(new Initializer(true))
      first._initializers.append(new Initializer(false))

      second._initializers.append(new Initializer(false))
      second._initializers.append(new Initializer(false))
      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different initializers"))
    }
  }

  describe("Annotations") {
    it("should pass when annotations are Equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toAnnotation(Array("TestAnnotation"), Some("param")))
      second.add(toAnnotation(Array("TestAnnotation"), Some("param")))
      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("should not pass when annotations are not Equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toAnnotation(Array("TestAnnotation"), Some("param")))
      second.add(toAnnotation(Array("diff"), Some("param")))
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //When //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different Annotations"))
    }

    it("should fail even when Annotations are subsets") {
      //Annotations must be exactly equal or not
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toAnnotation(Array("TestAnnotation"), None))
      second.add(toAnnotation(Array("QualifiedName", "TestAnnotation"), None))
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("QualifiedName")))

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different Annotations"))
    }
  }

  describe("Modifiers") {
    it("should be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      Array("private", "with sharing").map(toModifier).foreach(first.add)
      Array("private", "with sharing").map(toModifier).foreach(second.add)
      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("should fail when modifiers are not Equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      Array("private", "with sharing").map(toModifier).foreach(first.add)
      Array("private", "without sharing").map(toModifier).foreach(second.add)
      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different modifiers"))
    }
  }

  describe("Class ImplementsTypeList") {
    it("should be subsets when implementsTypeList type has a fully qualified name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toTypeList(Array(toTypeRef(Map("Foo" -> None)), toTypeRef(Map("Bar" -> None)))))
      second.add(
        toTypeList(
          Array(
            toTypeRef(Map("Foo" -> None)),
            toTypeRef(Map("fflib_BatchJob" -> None, "Bar" -> None))
          )
        )
      )
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("fflib_BatchJob")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head
          .contains("implementsTypeList not strictly equal but are subsets")
      )
    }

    it("should not be equal when implementsTypeList have typename that cannot be resolved") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toTypeList(Array(toTypeRef(Map("Foo" -> None)), toTypeRef(Map("Bar" -> None)))))
      second.add(
        toTypeList(
          Array(
            toTypeRef(Map("Foo" -> None)),
            toTypeRef(Map("fflib_BatchJob" -> None, "Bar" -> None))
          )
        )
      )
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different implements"))
    }

    it("should not be equal when implementsTypeList has types that are not proper subsets") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toTypeList(Array(toTypeRef(Map("Foo" -> None)), toTypeRef(Map("Bar" -> None)))))
      second.add(
        toTypeList(
          Array(
            toTypeRef(Map("Foo" -> None)),
            toTypeRef(Map("Bar" -> None)),
            toTypeRef(Map("Baz" -> None))
          )
        )
      )
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different implements"))
    }

    it("should not be equal when first implementsTypeList has empty types") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first.add(toTypeList(Array()))
      second.add(toTypeList(Array(toTypeRef(Map("Foo" -> None)))))
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different implements"))
    }
  }

  describe("Constructors") {
    it("should be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )
      second._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )

      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      comparator.subsetOf(second)

      assert(comparator.getWarnings.isEmpty)
    }

    it("should not be equal when it has different annotations") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )
      second._constructors.append(
        getBasicConstructor("Something", "private", "Dummy", "String", "s")
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different constructors"))
    }

    it("should not be equal when it has different modifiers") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )
      second._constructors.append(
        getBasicConstructor("TestVisible", "public", "Dummy", "String", "s")
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different constructors"))
    }

    it("should not be equal when it has different Ids") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )
      second._constructors.append(
        getBasicConstructor("TestVisible", "private", "NotDummy", "String", "s")
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different constructors"))
    }

    it("should not be equal when it has different TypeRefs") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )
      second._constructors.append(
        getBasicConstructor("TestVisible", "private", "NotDummy", "Integer", "s")
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different constructors"))
    }

    it("should not be equal when it has different typeRef names") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(
        getBasicConstructor("TestVisible", "private", "Dummy", "String", "s")
      )
      second._constructors.append(
        getBasicConstructor("TestVisible", "private", "NotDummy", "String", "a")
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different constructors"))
    }

    it("should not be subsets when it typeRef has resolved names") {
      //We expect constructors to be either equal or not and do not check for subsets
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._constructors.append(getBasicConstructor("TestVisible", "private", "Dummy", "Foo", "f"))
      val sCon = getBasicConstructor("TestVisible", "private", "Dummy", "Foo", "f")
      sCon.formalParameterList.formalParameters.head.typeRef.get
        .asInstanceOf[UnresolvedTypeRef]
        .typeNames
        .prepend(toTypeNames("ResolvedName", None))
      second._constructors.append(sCon)

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("ResolvedName")))

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different constructors"))
    }
  }

  describe("Methods") {
    def generateMethod(
      annotations: Array[Annotation] = Array(toAnnotation(Array("Override"), None)),
      modifiers: Array[Modifier] = Array("public", "static").map(toModifier),
      typeRef: TypeRef = toTypeRef(Map("void" -> None)),
      id: Id = toId("method"),
      parameters: FormalParameterList = generateParameterList()
    ): MethodDeclaration = {
      toMethodDeclaration(annotations, modifiers, typeRef, id, parameters)
    }

    def generateParameterList(
      annotations: Array[Annotation] = Array[Annotation](),
      modifiers: Array[Modifier] = Array[Modifier](),
      typeRef: TypeRef = toTypeRef(Map("String" -> None)),
      id: Id = toId("s")
    ): FormalParameterList = {
      toParameterList(Array(toParameter(annotations, modifiers, Some(typeRef), Some(id))))
    }

    it("should be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._methods.append(generateMethod())
      second._methods.append(generateMethod())

      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("should be not be equal when return types have unresolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._methods.append(generateMethod(typeRef = toTypeRef(Map("Bar" -> None))))
      second._methods.append(generateMethod(typeRef = toTypeRef(Map("Foo" -> None, "Bar" -> None))))

      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different methods"))
    }

    it("should be subsets when return types have resolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._methods.append(generateMethod(typeRef = toTypeRef(Map("Bar" -> None))))
      second._methods.append(
        generateMethod(typeRef = toTypeRef(Map("ResolvedName" -> None, "Bar" -> None)))
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("ResolvedName")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head
          .contains("Some Types are not strictly equal, but are subsets")
      )
    }

    it("should be subsets when return types have array subscripts and the other has List") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._methods.append(generateMethod(typeRef = toTypeRef(Map("String" -> None), 1)))
      second._methods.append(
        generateMethod(typeRef =
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("String" -> None))))))
        )
      )

      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("ResolvedName")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head
          .contains("TypeRef Array Subscript resolved to List in other")
      )
    }

    it("should be subsets when parameterList have all resolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._methods.append(
        generateMethod(parameters =
          generateParameterList(
            typeRef = toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Bar" -> None)))))),
            id = toId("s")
          )
        )
      )
      second._methods.append(
        generateMethod(parameters =
          generateParameterList(
            typeRef = toTypeRef(
              Map("List" -> Some(Array(toTypeRef(Map("ResolvedName" -> None, "Bar" -> None)))))
            ),
            id = toId("s")
          )
        )
      )

      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("ResolvedName", "Bar")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head
          .contains("TypeRefs in second has all names fully resolved the other does not")
      )
    }

    it("should not be equal when parameterList have different type arguments") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._methods.append(
        generateMethod(parameters =
          generateParameterList(typeRef = toTypeRef(Map("Foo" -> None)), id = toId("s"))
        )
      )
      second._methods.append(
        generateMethod(parameters =
          generateParameterList(typeRef = toTypeRef(Map("Bar" -> None)), id = toId("s"))
        )
      )
      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])

      //Then
      val caught = intercept[Exception] {
        comparator.subsetOf(second)
      }
      assert(caught.getMessage.contains("Different methods"))
    }

    it("should be subsets when parameterList has a resolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._methods.append(
        generateMethod(
          parameters = generateParameterList(typeRef = toTypeRef(Map("Bar" -> None))),
          id = toId("s")
        )
      )
      second._methods.append(
        generateMethod(
          parameters =
            generateParameterList(typeRef = toTypeRef(Map("ResolvedName" -> None, "Bar" -> None))),
          id = toId("s")
        )
      )
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("ResolvedName")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head
          .contains("Some Types are not strictly equal, but are subsets")
      )
    }

    it("should be subsets when parameterList with type arguments has a resolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")
      first._methods.append(
        generateMethod(
          parameters = generateParameterList(typeRef =
            toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Bar" -> None))))))
          ),
          id = toId("s")
        )
      )
      second._methods.append(
        generateMethod(parameters =
          generateParameterList(
            typeRef = toTypeRef(
              Map("List" -> Some(Array(toTypeRef(Map("ResolvedName" -> None, "Bar" -> None)))))
            ),
            id = toId("s")
          )
        )
      )

      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("ResolvedName")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head
          .contains("Some Types are not strictly equal, but are subsets")
      )
    }

  }

  describe("Fields") {
    //Don't need extensive testing since Methods uses the same code path
    it("should be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._fields.append(
        toFieldDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("String" -> None)))))),
          toId("field")
        )
      )
      second._fields.append(
        toFieldDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("String" -> None)))))),
          toId("field")
        )
      )

      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("should be subsets when type has resolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._fields.append(
        toFieldDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Bar" -> None)))))),
          toId("field")
        )
      )
      second._fields.append(
        toFieldDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Foo" -> None, "Bar" -> None)))))),
          toId("field")
        )
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("Foo")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head.contains("Some Types are not strictly equal, but are subsets")
      )
    }
  }

  describe("Properties") {
    //Don't need extensive testing since Methods uses the same code path
    it("should be equal") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._properties.append(
        toPropertyDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("String" -> None)))))),
          toId("field")
        )
      )
      second._properties.append(
        toPropertyDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("String" -> None)))))),
          toId("field")
        )
      )

      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("should be subsets when type has resolved name") {
      //Given
      val first  = generateEmptyClassDeclaration("Dummy")
      val second = generateEmptyClassDeclaration("Dummy")

      first._properties.append(
        toPropertyDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Bar" -> None)))))),
          toId("field")
        )
      )
      second._properties.append(
        toPropertyDeclaration(
          Array(toAnnotation(Array("TestVisible"), None)),
          Array(toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Foo" -> None, "Bar" -> None)))))),
          toId("field")
        )
      )

      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("Foo")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head.contains("Some Types are not strictly equal, but are subsets")
      )
    }
  }

  describe("InnerClasses") {
    it("Should be equal when inner types are equal") {
      //Given
      val first  = generateClassDeclaration("Dummy")
      val second = generateClassDeclaration("Dummy")
      first._innerTypes.append(generateClassDeclaration("InnerType"))
      second._innerTypes.append(generateClassDeclaration("InnerType"))
      //When
      val comparator = SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.isEmpty)
    }

    it("Should be subsets when inner types has typeRefs with names that are resolved") {
      //Given
      val first  = generateClassDeclaration("Dummy")
      val second = generateClassDeclaration("Dummy")
      val fInner = generateEmptyClassDeclaration("InnerType")
      val sInner = generateEmptyClassDeclaration("InnerType")
      fInner._fields.append(
        toFieldDeclaration(
          Array[Annotation](),
          Array[Modifier](),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Foo" -> None)))))),
          toId("stringList")
        )
      )
      sInner._fields.append(
        toFieldDeclaration(
          Array[Annotation](),
          Array[Modifier](),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("Bar" -> None, "Foo" -> None)))))),
          toId("stringList")
        )
      )
      first._innerTypes.append(fInner)
      second._innerTypes.append(sInner)
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration], getMockResolver(), getMockResolver(Array("Bar")))
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head.contains("Some Types are not strictly equal, but are subsets")
      )
    }

    it("Should be subsets when inner types are referenced by full name") {
      //Given
      val first  = generateClassDeclaration("Dummy")
      val second = generateClassDeclaration("Dummy")
      val fInner = generateEmptyClassDeclaration("InnerType")
      val sInner = generateEmptyClassDeclaration("InnerType")
      first._innerTypes.append(fInner)
      second._innerTypes.append(sInner)
      first._fields.append(
        toFieldDeclaration(
          Array[Annotation](),
          Array[Modifier](toModifier("private")),
          toTypeRef(Map("List" -> Some(Array(toTypeRef(Map("InnerType" -> None)))))),
          toId("stringList")
        )
      )
      second._fields.append(
        toFieldDeclaration(
          Array[Annotation](),
          Array[Modifier](toModifier("private")),
          toTypeRef(
            Map("List" -> Some(Array(toTypeRef(Map("Dummy" -> None, "InnerType" -> None)))))
          ),
          toId("stringList")
        )
      )
      //When
      val comparator =
        SubsetComparator(first.asInstanceOf[IModuleTypeDeclaration])
      comparator.subsetOf(second)

      //Then
      assert(comparator.getWarnings.nonEmpty)
      assert(
        comparator.getWarnings.head.contains("Some Types are not strictly equal, but are subsets")
      )
    }
  }
}
