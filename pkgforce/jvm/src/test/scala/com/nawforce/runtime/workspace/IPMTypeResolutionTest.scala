package com.nawforce.runtime.workspace

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration
import org.scalatest.funsuite.AnyFunSuite

class IPMTypeResolutionTest extends AnyFunSuite {
  def getType(typeId: String, index: IPM.Index): Option[IModuleTypeDeclaration] = {
    index.rootModule.get.findExactTypeId(typeId)
  }

  test("Resolves nested type") {
    val sources = Map("Foo.cls" -> "public class Foo { private FooBar b; public class FooBar {} }")
    FileSystemHelper.run(sources) { root: PathLike =>
      val index  = new IPM.Index(root)
      val foo    = getType("Foo", index)
      val foobar = getType("Foo.FooBar", index)
      assert(foo.nonEmpty && foobar.nonEmpty)
      assert(foo.get.fields.head.typeRef.getFullName == "Foo.FooBar")
      assert(foo.get.fields.head.typeRef == foobar.get)
    }
  }

  test("Resolves peer type") {
    val sources = Map(
      "Dummy.cls" -> "public class Dummy { public class InnerType {private PeerType innerField;} public class PeerType{}}"
    )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index     = new IPM.Index(root)
      val dummy     = getType("Dummy", index)
      val innerType = getType("Dummy.InnerType", index)
      val peerType  = getType("Dummy.PeerType", index)

      assert(dummy.nonEmpty && innerType.nonEmpty && peerType.nonEmpty)
      assert(innerType.get.fields.head.typeRef == peerType.get)
      assert(innerType.get.fields.head.typeRef.getFullName == "Dummy.PeerType")
    }
  }

  test("Resolves from super type") {
    val sources = Map(
      "Baz.cls"   -> "public class Baz { public class  BazBar {} }",
      "Foo.cls"   -> "public class Foo extends Baz { }",
      "Dummy.cls" -> "public class Dummy extends Foo { private BazBar t; }"
    )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index  = new IPM.Index(root)
      val dummy  = getType("Dummy", index)
      val bazBar = getType("Baz.BazBar", index)

      assert(dummy.nonEmpty && bazBar.nonEmpty)
      val dummyField = dummy.get.fields.head

      assert(dummyField.typeRef.getFullName == "Baz.BazBar")
      assert(dummyField.typeRef == bazBar.get)
    }
  }

  test("Resolves from super type that's an inner") {
    val sources =
      Map(
        "Dummy.cls" -> "public class Dummy extends Foo { private Unresolved t; public class Foo {} }"
      )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)
      val foo   = getType("Dummy.Foo", index)

      val dummyExtends = dummy.get.extendsTypeRef
      assert(dummyExtends.getFullName == "Dummy.Foo")
      assert(dummyExtends == foo.get)
    }
  }

  test("Resolves Type") {
    val sources = Map(
      "Foo.cls"   -> "public class Foo { }",
      "Dummy.cls" -> "public class Dummy extends Foo { private Foo f; }"
    )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)
      val foo   = getType("Foo", index)

      assert(dummy.nonEmpty && foo.nonEmpty)
      assert(dummy.get.fields.head.typeRef == foo.get)
    }
  }

  test("Resolves with nested type arguments") {
    val sources =
      Map("Dummy.cls" -> "public class Dummy { private Map<String,List<List<String>>> t; }")
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)

      val dummyField = dummy.get.fields.head
      assert(
        dummyField.typeRef.getFullName == "System.Map<System.String,System.List<System.List<System.String>>>"
      )
    }
  }

  test("Resolves with nested type with relative arguments") {
    val sources =
      Map(
        "Dummy.cls" -> "public class Dummy { private Map<String,List<Bar>> t; public class Bar {}}"
      )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)

      val dummyField = dummy.get.fields.head
      assert(dummyField.typeRef.getFullName == "System.Map<System.String,System.List<Dummy.Bar>>")
    }
  }

  test("Resolves Internal Object type") {
    val sources =
      Map("Dummy.cls" -> "public class Dummy { private ObJeCt func(){}}")
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)

      val dummyMethod = dummy.get.methods.head
      assert(dummyMethod.typeRef.isInstanceOf[PlatformTypeDeclaration])
      assert(dummyMethod.typeRef.getFullName == "Internal.Object$")
      assert(dummyMethod.typeRef.toString == "Object")
    }
  }
}
