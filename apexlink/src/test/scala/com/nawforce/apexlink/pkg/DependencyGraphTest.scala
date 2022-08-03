package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.rpc.{DependencyLink, DependencyNode}
import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.names.{Name, TypeIdentifier, TypeName}
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class DependencyGraphTest extends AnyFunSuite with TestHelper {

  test("Correct dependencies returned at depth 1") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "//MaxDependencyCount(10)\n public class A extends B { C c;}",
        "B.cls" -> "public class B {C c;}",
        "C.cls" -> "public class C {D d;}",
        "D.cls" -> "public class D {}"
      )
    ) { root: PathLike =>
      val org = createOrg(root)
      val result = org.getDependencyGraph(
        Array(TypeIdentifier(None, TypeName(Name("A")))),
        1,
        apexOnly = true,
        Array()
      )
      assert(
        result.nodeData sameElements Array(
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("A"))),
            58,
            "class",
            3,
            Some(10),
            isEntryPoint = false,
            Array(TypeIdentifier(None, TypeName(Name("B")))),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("C"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("B"))),
            21,
            "class",
            2,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("C"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("C"))),
            21,
            "class",
            1,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("D"))))
          )
        )
      )
      assert(
        result.linkData sameElements Array(
          DependencyLink(0, 1, "extends"),
          DependencyLink(0, 2, "uses"),
          DependencyLink(1, 2, "uses")
        )
      )
    }
  }

  test("Correct dependencies returned at depth 2") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "public class A extends B { C c;}",
        "B.cls" -> "public class B {C c;}",
        "C.cls" -> "public class C {D d;}",
        "D.cls" -> "public class D {}"
      )
    ) { root: PathLike =>
      val org = createOrg(root)
      val result = org.getDependencyGraph(
        Array(TypeIdentifier(None, TypeName(Name("A")))),
        2,
        apexOnly = true,
        Array()
      )
      assert(
        result.nodeData sameElements Array(
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("A"))),
            32,
            "class",
            3,
            None,
            isEntryPoint = false,
            Array(TypeIdentifier(None, TypeName(Name("B")))),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("C"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("B"))),
            21,
            "class",
            2,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("C"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("C"))),
            21,
            "class",
            1,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("D"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("D"))),
            17,
            "class",
            0,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array()
          )
        )
      )
      assert(
        result.linkData sameElements Array(
          DependencyLink(0, 1, "extends"),
          DependencyLink(0, 2, "uses"),
          DependencyLink(1, 2, "uses"),
          DependencyLink(2, 3, "uses")
        )
      )
    }
  }

  test("Correct dependencies returned at depth 2 with ignored") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "public class A extends B { C c;}",
        "B.cls" -> "public class B {C c;}",
        "C.cls" -> "public class C {D d;}",
        "D.cls" -> "public class D {}"
      )
    ) { root: PathLike =>
      val org = createOrg(root)
      val result = org.getDependencyGraph(
        Array(TypeIdentifier(None, TypeName(Name("A")))),
        2,
        apexOnly = true,
        Array(TypeIdentifier(None, TypeName(Name("B"))))
      )
      assert(
        result.nodeData sameElements Array(
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("A"))),
            32,
            "class",
            2,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("C"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("C"))),
            21,
            "class",
            1,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array(TypeIdentifier(None, TypeName(Name("D"))))
          ),
          DependencyNode(
            TypeIdentifier(None, TypeName(Name("D"))),
            17,
            "class",
            0,
            None,
            isEntryPoint = false,
            Array(),
            Array(),
            Array()
          )
        )
      )
      assert(
        result.linkData sameElements Array(
          DependencyLink(0, 1, "uses"),
          DependencyLink(1, 2, "uses")
        )
      )
    }
  }

  test("Detects global entry points") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "global class A { }",
        "B.cls" ->
          """public class B {
            | @AuraEnabled(cacheable=true)
            | public static Account getAccount() {}
            |}""".stripMargin
      )
    ) { root: PathLike =>
      val org = createOrg(root)
      val result = org.getDependencyGraph(
        Array(TypeIdentifier(None, TypeName(Name("A"))), TypeIdentifier(None, TypeName(Name("B")))),
        1,
        apexOnly = true,
        Array()
      )
      assert(result.nodeData.length == 2)
      assert(result.nodeData.forall(_.isEntryPoint))
    }
  }

  test("Detects page controller entry points") {
    FileSystemHelper.run(
      Map("C.cls" -> "public class C {}", "VF.page" -> "<apex:page controller=\"C\"></apex:page>")
    ) { root: PathLike =>
      val org = createOrg(root)
      val result = org.getDependencyGraph(
        Array(TypeIdentifier(None, TypeName(Name("C")))),
        1,
        apexOnly = true,
        Array()
      )
      assert(result.nodeData.length == 1)
      assert(result.nodeData.forall(_.isEntryPoint))
    }
  }

  test("Detects async entry points") {
    FileSystemHelper.run(
      Map(
        "D.cls" -> "public class D implements Queueable {}",
        "E.cls" -> "public abstract class E implements Database.Batchable<SObject> {}",
        "F.cls" -> "public class F extends E {}"
      )
    ) { root: PathLike =>
      val org = createOrg(root)
      val result = org.getDependencyGraph(
        Array(
          TypeIdentifier(None, TypeName(Name("D"))),
          TypeIdentifier(None, TypeName(Name("E"))),
          TypeIdentifier(None, TypeName(Name("F")))
        ),
        1,
        apexOnly = true,
        Array()
      )
      assert(result.nodeData.length == 3)
      assert(
        result.nodeData.map(d => (d.identifier.toString(), d.isEntryPoint)) sameElements Array(
          ("D", true),
          ("E", false),
          ("F", true)
        )
      )
    }
  }

  test("Detects trigger entry points") {
    FileSystemHelper.run(Map("T.trigger" -> "trigger T on Account (before insert) {}")) {
      root: PathLike =>
        val org = createOrg(root)
        val result = org.getDependencyGraph(
          Array(TypeIdentifier(None, TypeName(Name("__sfdc_trigger/T")))),
          1,
          apexOnly = true,
          Array()
        )
        assert(result.nodeData.length == 1)
        assert(result.nodeData.forall(_.isEntryPoint))
    }
  }
}
