package com.nawforce.runtime.workspace

import com.financialforce.oparser.TypeDeclaration
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class IPMTypeResolutionTest extends AnyFunSuite {
  def getType(typeId: String, index: IPM.Index): Option[TypeDeclaration] = {
    index.rootModule.get.findExactTypeId(typeId)
  }

  test("Resolves nested type") {
    val sources = Map(
      "Foo.cls" -> "public class Foo { private FooBar b; public class FooBar {} }",
    )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val foo = getType("Foo", index)
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
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)
      val innerType = getType("Dummy.InnerType", index)
      val peerType = getType("Dummy.PeerType", index)

      assert(dummy.nonEmpty && innerType.nonEmpty && peerType.nonEmpty)
      assert(innerType.get.fields.head.typeRef == peerType.get)
      assert(innerType.get.fields.head.typeRef.getFullName == "Dummy.PeerType")
    }
  }


  test("Resolves from super type") {
    val sources = Map(
      "Baz.cls" -> "public class Baz { public class  BazBar {} }",
      "Foo.cls" -> "public class Foo extends Baz { }",
      "Dummy.cls" -> "public class Dummy extends Foo { private BazBar t; }"
    )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)
      val bazBar = getType("Baz.BazBar", index)

      assert(dummy.nonEmpty && bazBar.nonEmpty)
      val dummyField = dummy.get.fields.head

      assert(dummyField.typeRef.getFullName == "Baz.BazBar")
      assert(dummyField.typeRef == bazBar.get)
    }
  }

  test("Resolves Type") {
    val sources = Map(
      "Foo.cls" -> "public class Foo { }",
      "Dummy.cls" -> "public class Dummy extends Foo { private Foo f; }"
    )
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      val dummy = getType("Dummy", index)
      val foo = getType("Foo", index)

      assert(dummy.nonEmpty && foo.nonEmpty)
      assert(dummy.get.fields.head.typeRef == foo.get)
    }
  }
}

