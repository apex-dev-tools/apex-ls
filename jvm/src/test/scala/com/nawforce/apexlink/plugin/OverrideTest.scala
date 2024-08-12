/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */

package com.nawforce.apexlink.plugin
import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.plugins.OverridePlugin
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OverrideTest extends AnyFunSuite with Matchers with TestHelper {

  def createOrgWithOverride(root: PathLike): OPM.OrgImpl = {
    createOrgWithPlugin(root, classOf[OverridePlugin])
  }

  def orgIssuesFor(org: OPM.OrgImpl, path: PathLike): String = {
    val messages = org.issueManager.issuesForFileInternal(path).map(_.asString()).mkString("\n")
    if (messages.nonEmpty) messages + "\n" else ""
  }

  forAll(Set("", "private")) { baseVisibility =>
    val baseVisibilityDescribe = if (baseVisibility.isEmpty) "implicit private" else "private"
    forAll(Set("private", "protected", "public")) { overrideVisibility =>
      test(s"Override $baseVisibilityDescribe with $overrideVisibility") {
        FileSystemHelper.run(
          Map(
            "Base.cls" -> s"public virtual class Base {$baseVisibility void foo() {}}",
            "Over.cls" -> s"public class Over extends Base {$overrideVisibility void foo() {}}"
          )
        ) { root: PathLike =>
          val org = createOrgWithOverride(root)
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "The overrides of this private method will fail in v61, see Over\n"
          )
          orgIssuesFor(org, root.join("Over.cls")) should include(
            "This override of a private method will fail in v61, see /Base.cls: line 1 at"
          )
        }
      }

      test(s"Override $baseVisibilityDescribe with $overrideVisibility in same file") {
        FileSystemHelper.run(
          Map(
            "Base.cls" -> s"public virtual class Base {$baseVisibility void foo() {} public class Over extends Base {$overrideVisibility void foo() {}}}"
          )
        ) { root: PathLike =>
          val org = createOrgWithOverride(root)
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "Method 'foo' can not override non-virtual method\n"
          )
        }
      }

      test(s"Override $baseVisibilityDescribe virtual with $overrideVisibility") {
        FileSystemHelper.run(
          Map(
            "Base.cls" -> s"public virtual class Base {$baseVisibility virtual void foo() {}}",
            "Over.cls" -> s"public class Over extends Base {$overrideVisibility void foo() {}}"
          )
        ) { root: PathLike =>
          val org = createOrgWithOverride(root)
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "The overrides of this private method will fail in v61, see Over\n"
          )
          orgIssuesFor(org, root.join("Over.cls")) should include(
            "This override of a private method will fail in v61, see /Base.cls: line 1"
          )
        }
      }

      test(s"Override $baseVisibilityDescribe virtual with $overrideVisibility in same file") {
        FileSystemHelper.run(
          Map(
            "Base.cls" -> s"public virtual class Base {private virtual void foo() {} public class Over extends Base {$overrideVisibility override void foo() {}}}"
          )
        ) { root: PathLike =>
          val org = createOrgWithOverride(root)
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "The overrides of this private method will fail in v61, see Base.Over\n"
          )
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "This override of a private method will fail in v61, see /Base.cls: line 1 at"
          )
        }
      }

      test(s"Override $baseVisibilityDescribe abstract with $overrideVisibility") {
        FileSystemHelper.run(
          Map(
            "Base.cls" -> s"public abstract class Base {$baseVisibility abstract void foo();}",
            "Over.cls" -> s"public class Over extends Base {$overrideVisibility void foo() {}}"
          )
        ) { root: PathLike =>
          val org = createOrgWithOverride(root)
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "The overrides of this private method will fail in v61, see Over\n"
          )
          orgIssuesFor(org, root.join("Over.cls")) should include(
            "This override of a private method will fail in v61, see /Base.cls: line 1"
          )
        }
      }

      test(s"Override $baseVisibilityDescribe abstract with $overrideVisibility in same file") {
        FileSystemHelper.run(
          Map(
            "Base.cls" -> s"public abstract class Base {private abstract void foo(); public class Over extends Base {$overrideVisibility override void foo() {}}}"
          )
        ) { root: PathLike =>
          val org = createOrgWithOverride(root)
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "The overrides of this private method will fail in v61, see Base.Over\n"
          )
          orgIssuesFor(org, root.join("Base.cls")) should include(
            "This override of a private method will fail in v61, see /Base.cls: line 1 at"
          )
        }
      }
    }
  }

  test(s"Three trier private/private/private override ") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> s"public virtual class A {private void foo() {} }",
        "B.cls" -> s"public virtual class B extends A {private void foo() {} }",
        "C.cls" -> s"public virtual class C extends B {private void foo() {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithOverride(root)
      assert(
        orgIssuesFor(org, root.join("A.cls"))
          == "Error: line 1 at 37-40: The overrides of this private method will fail in v61, see B\n"
      )
      assert(
        orgIssuesFor(org, root.join("B.cls"))
          == "Error: line 1 at 47-50: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n" +
          "Error: line 1 at 47-50: The overrides of this private method will fail in v61, see C\n"
      )
      assert(
        orgIssuesFor(org, root.join("C.cls"))
          == "Error: line 1 at 47-50: This override of a private method will fail in v61, see /B.cls: line 1 at 42-55\n"
      )
    }
  }

  test(s"Three trier private/protected/protected override ") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> s"public virtual class A {private void foo() {} }",
        "B.cls" -> s"public virtual class B extends A {protected virtual void foo() {} }",
        "C.cls" -> s"public virtual class C extends B {protected override void foo() {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithOverride(root)
      assert(
        orgIssuesFor(org, root.join("A.cls"))
          == "Error: line 1 at 37-40: The overrides of this private method will fail in v61, see B\n"
      )
      assert(
        orgIssuesFor(org, root.join("B.cls"))
          == "Error: line 1 at 57-60: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
      assert(
        orgIssuesFor(org, root.join("C.cls"))
          == "Error: line 1 at 58-61: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
    }
  }

  test(s"Three trier private/protected/public override ") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "public virtual class A {private void foo() {} }",
        "B.cls" -> "public virtual class B extends A {protected virtual void foo() {} }",
        "C.cls" -> "public virtual class C extends B {public override void foo() {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithOverride(root)
      assert(
        orgIssuesFor(org, root.join("A.cls"))
          == "Error: line 1 at 37-40: The overrides of this private method will fail in v61, see B\n"
      )
      assert(
        orgIssuesFor(org, root.join("B.cls"))
          == "Error: line 1 at 57-60: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
      assert(
        orgIssuesFor(org, root.join("C.cls"))
          == "Error: line 1 at 55-58: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
    }
  }

  test(s"Three trier private/public/public override ") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "public virtual class A {private void foo() {} }",
        "B.cls" -> "public virtual class B extends A {public virtual void foo() {} }",
        "C.cls" -> "public virtual class C extends B {public override void foo() {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithOverride(root)
      assert(
        orgIssuesFor(org, root.join("A.cls"))
          == "Error: line 1 at 37-40: The overrides of this private method will fail in v61, see B\n"
      )
      assert(
        orgIssuesFor(org, root.join("B.cls"))
          == "Error: line 1 at 54-57: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
      assert(
        orgIssuesFor(org, root.join("C.cls"))
          == "Error: line 1 at 55-58: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
    }
  }

  test(s"Dual overrides") {
    FileSystemHelper.run(
      Map(
        "A.cls" -> "public virtual class A {private void foo() {} }",
        "B.cls" -> "public virtual class B extends A {private void foo() {} }",
        "C.cls" -> "public virtual class C extends A {private void foo() {} }"
      )
    ) { root: PathLike =>
      val org = createOrgWithOverride(root)
      assert(
        orgIssuesFor(org, root.join("A.cls"))
          == "Error: line 1 at 37-40: The overrides of this private method will fail in v61, see B, C\n"
      )
      assert(
        orgIssuesFor(org, root.join("B.cls"))
          == "Error: line 1 at 47-50: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
      assert(
        orgIssuesFor(org, root.join("C.cls"))
          == "Error: line 1 at 47-50: This override of a private method will fail in v61, see /A.cls: line 1 at 32-45\n"
      )
    }
  }

  test(s"Dual overrides (same file)") {
    FileSystemHelper.run(
      Map(
        "A.cls" ->
          """public virtual class A {private virtual void foo() {}
            | public virtual class B extends A {private override void foo() {} }
            | public virtual class C extends A {private override void foo() {} }
            |}
            |""".stripMargin
      )
    ) { root: PathLike =>
      val org = createOrgWithOverride(root)
      assert(
        orgIssuesFor(org, root.join("A.cls"))
          == "Error: line 1 at 45-48: The overrides of this private method will fail in v61, see A.B, A.C\n" +
          "Error: line 2 at 57-60: This override of a private method will fail in v61, see /A.cls: line 1 at 40-53\n" +
          "Error: line 3 at 57-60: This override of a private method will fail in v61, see /A.cls: line 1 at 40-53\n"
      )
    }
  }

}
