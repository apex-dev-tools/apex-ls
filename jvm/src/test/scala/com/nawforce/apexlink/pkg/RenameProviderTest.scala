package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class RenameProviderTest extends AnyFunSuite with TestHelper {
  test("Rename: Variable | Scope: Function | rename param var declaration") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod(String te${CURSOR}st) {
         |String fakeVar = test;
         |return test;
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(3, 7, 3, 11))
      assert(renames(0).edits(1) == Location(2, 17, 2, 21))
      assert(renames(0).edits(2) == Location(1, 51, 1, 55))
    }
  }

  test("Rename: Variable | Scope: Function | rename param var usage") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod(String test) {
         |String fakeVar = te${CURSOR}st;
         |return test;
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")
      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(3, 7, 3, 11))
      assert(renames(0).edits(1) == Location(2, 17, 2, 21))
      assert(renames(0).edits(2) == Location(1, 51, 1, 55))

    }
  }

  test("Rename: Variable | Scope: Function | rename function var declaration") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod() {
         |String te${CURSOR}st;
         |String fakeVar = test;
         |return test;
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")
      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(3, 17, 3, 21))
      assert(renames(0).edits(1) == Location(4, 7, 4, 11))
      assert(renames(0).edits(2) == Location(2, 7, 2, 11))
    }
  }

  test("Rename: Variable | Scope: Function | rename function var usage") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod() {
         |String test;
         |String fakeVar = te${CURSOR}st;
         |return test;
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")
      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(3, 17, 3, 21))
      assert(renames(0).edits(1) == Location(4, 7, 4, 11))
      assert(renames(0).edits(2) == Location(2, 7, 2, 11))
    }
  }

  test("Rename: Variable | Scope: Function | rename unused var") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod() {
         |String te${CURSOR}st;
         |String fakeVar = 'thing';
         |return fakeVar;
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")
      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(2, 7, 2, 11))
    }
  }

  test("Rename: Variable | Scope: Function | rename var declaration with dot chaining") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod() {
         |Map<String, String> m${CURSOR}1 = new Map<String, String>();
         |m1.put('Type', 'Fruit');
         |m1.put('Colour', 'Green');
         |return m1.get('Type');
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")
      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 4)
      assert(renames(0).edits(0) == Location(3, 0, 3, 2))
      assert(renames(0).edits(1) == Location(4, 0, 4, 2))
      assert(renames(0).edits(2) == Location(5, 7, 5, 9))
      assert(renames(0).edits(3) == Location(2, 20, 2, 22))
    }
  }

  test("Rename: Variable | Scope: Function | rename var usage with dot chaining") {
    val contentAndCursorPos = withCursorMultiLine(
      s"""public class Dummy { public void someMethod() {
         |Map<String, String> m1 = new Map<String, String>();
         |m${CURSOR}1.put('Type', 'Fruit');
         |m1.put('Colour', 'Green');
         |return m1.get('Type');
         |} }""".stripMargin
        .replaceAll("\r\n", "\n")
    )

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")
      val renames = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 4)
      assert(renames(0).edits(0) == Location(3, 0, 3, 2))
      assert(renames(0).edits(1) == Location(4, 0, 4, 2))
      assert(renames(0).edits(2) == Location(5, 7, 5, 9))
      assert(renames(0).edits(3) == Location(2, 20, 2, 22))
    }
  }

  test("Rename: Variable | Scope: Class | rename var declaration") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |String my${CURSOR}Field;
         |public void someMethod() {
         |String fakeVar = myField;
         |String test = myField;
         |}
         |public String methodA(){String test2 = myField;}
         |private String methodPrivate(){return myField;}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(5, 14, 5, 21))
      assert(renames(0).edits(1) == Location(4, 17, 4, 24))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(7, 39, 7, 46))
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(8, 38, 8, 45))
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 7, 2, 14))
    }
  }

  test("Rename: Variable | Scope: Class | rename var usage") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |String myField;
         |public void someMethod() {
         |String fakeVar = myField;
         |String test = my${CURSOR}Field;
         |}
         |public String methodA(){String test2 = myField;}
         |private String methodPrivate(){return myField;}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(5, 14, 5, 21))
      assert(renames(0).edits(1) == Location(4, 17, 4, 24))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(7, 39, 7, 46))
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(8, 38, 8, 45))
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 7, 2, 14))
    }
  }

  test("Rename: Variable | Scope: Class | unused class var") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String my${CURSOR}Field;
         |public void someMethod() {
         |String fakeVar = 'myField';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test(
    "Rename: Variable | Scope: Class | function var shadows class var | rename function var declaration"
  ) {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |public void someMethod() {
         |String myFi${CURSOR}eld = 'string value';
         |String newVar = myField;
         |return myField;
         |}
         |public String methodA(){String test2 = myField;}
         |private String methodPrivate(){return myField;}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(5, 16, 5, 23))
      assert(renames(0).edits(1) == Location(4, 7, 4, 14))
      assert(renames(0).edits(2) == Location(6, 7, 6, 14))
    }
  }

  test(
    "Rename: Variable | Scope: Class | function var shadows class var | rename function var usage"
  ) {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |public void someMethod() {
         |String myField = 'string value';
         |String newVar = my${CURSOR}Field;
         |return myField;
         |}
         |public String methodA(){String test2 = myField;}
         |private String methodPrivate(){return myField;}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(5, 16, 5, 23))
      assert(renames(0).edits(1) == Location(4, 7, 4, 14))
      assert(renames(0).edits(2) == Location(6, 7, 6, 14))
    }
  }

  test(
    "Rename: Variable | Scope: Class | function var shadows class var | rename class var declaration"
  ) {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String my${CURSOR}Field;
         |public void someMethod() {
         |String myField = 'string value';
         |String newVar = myField;
         |return myField;
         |}
         |public String methodA(){String test2 = myField;}
         |private String methodPrivate(){return myField;}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 3)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(8, 39, 8, 46))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(9, 38, 9, 45))
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test(
    "Rename: Variable | Scope: Class | function var shadows class var | rename class var usage"
  ) {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |public void someMethod() {
         |String myField = 'string value';
         |String newVar = myField;
         |return myField;
         |}
         |public String methodA(){String test2 = myField;}
         |private String methodPrivate(){return my${CURSOR}Field;}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 3)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(8, 39, 8, 46))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(9, 38, 9, 45))
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: Usage | rename across multiple blocks") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |private void privateMethod1(){
         |String newVar = my${CURSOR}Field;
         |}
         |private void privateMethod2(){
         |String newVar = myField;
         |}
         |private void privateMethod3(){
         |String newVar = myField;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(7, 16, 7, 23))
      assert(renames(2).path == "/Dummy.cls")
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(10, 16, 10, 23))
      assert(renames(3).path == "/Dummy.cls")
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: Declaration | rename across multiple blocks") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String my${CURSOR}Field;
         |private void privateMethod1(){
         |String newVar = myField;
         |}
         |private void privateMethod2(){
         |String newVar = myField;
         |}
         |private void privateMethod3(){
         |String newVar = myField;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(7, 16, 7, 23))
      assert(renames(2).path == "/Dummy.cls")
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(10, 16, 10, 23))
      assert(renames(3).path == "/Dummy.cls")
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: usage | Class object chain") {
    val dummyContent =
      """public class Dummy {
        |public String myField = 'test string';
        |}""".stripMargin.replaceAll("\r\n", "\n")
    val foo1ContentAndCursorPos = withCursorMultiLine(s"""public class Foo1 {
         |private void privateMethod(){
         |Dummy test = new Dummy();
         |test.my${CURSOR}Field = 'a new string';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(
      Map("Dummy.cls" -> dummyContent, "Foo1.cls" -> foo1ContentAndCursorPos._1)
    ) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Foo1.cls")

      val renames = {
        org.unmanaged
          .getRenameLocations(
            path,
            foo1ContentAndCursorPos._2.line,
            foo1ContentAndCursorPos._2.offset,
            Some(foo1ContentAndCursorPos._1)
          )
      }
      assert(renames.length == 2)
      assert(renames(0).path == "/Foo1.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 5, 4, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: Declaration | Class object chain") {
    val dummyContentAndCursorPos =
      withCursorMultiLine(s"""public class Dummy {
        |public String my${CURSOR}Field = 'test string';
        |}""".stripMargin.replaceAll("\r\n", "\n"))
    val foo1Content = """public class Foo1 {
         |private void privateMethod(){
         |Dummy test = new Dummy();
         |test.myField = 'a new string';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n")

    FileSystemHelper.run(
      Map("Dummy.cls" -> dummyContentAndCursorPos._1, "Foo1.cls" -> foo1Content)
    ) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames = {
        org.unmanaged
          .getRenameLocations(
            path,
            dummyContentAndCursorPos._2.line,
            dummyContentAndCursorPos._2.offset,
            Some(dummyContentAndCursorPos._1)
          )
      }
      assert(renames.length == 2)
      assert(renames(0).path == "/Foo1.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 5, 4, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: Usage | cross-file") {
    val dummyContent =
      """public class Dummy {
        |public static String myField = 'test string';
        |
        |private void privateMethod(){
        |myField = 'abc';
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")
    val foo1ContentAndCursorPos = withCursorMultiLine(s"""public class Foo1 {
         |private void privateMethod(){
         |Dummy.my${CURSOR}Field = 'a new string';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))
    val foo2Content =
      """public class Foo2 {
        |private void privateMethod(){
        |Dummy.myField = 'a new string but different';
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")

    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummyContent,
        "Foo1.cls"  -> foo1ContentAndCursorPos._1,
        "Foo2.cls"  -> foo2Content
      )
    ) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Foo1.cls")

      val renames = {
        org.unmanaged
          .getRenameLocations(
            path,
            foo1ContentAndCursorPos._2.line,
            foo1ContentAndCursorPos._2.offset,
            Some(foo1ContentAndCursorPos._1)
          )
      }
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(5, 0, 5, 7))
      assert(renames(1).path == "/Foo1.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(3, 6, 3, 13))
      assert(renames(2).path == "/Foo2.cls")
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(3, 6, 3, 13))
      assert(renames(3).path == "/Dummy.cls")
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 21, 2, 28))
    }
  }

  test("Rename: Variable | From: Declaration | cross-file") {
    val dummyContentAndCursorPos =
      withCursorMultiLine(s"""public class Dummy {
           |public static String my${CURSOR}Field = 'test string';
           |
           |private void privateMethod(){
           |myField = 'abc';
           |}
           |}""".stripMargin.replaceAll("\r\n", "\n"))
    val fooContent =
      """public class Foo {
        |private void privateMethod(){
        |Dummy.myField = 'a new string';
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")
    val thingContent =
      """public class Thing {
        |private void privateMethod(){
        |Dummy.myField = 'a new string but different';
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")

    FileSystemHelper.run(
      Map(
        "Dummy.cls" -> dummyContentAndCursorPos._1,
        "Foo.cls"   -> fooContent,
        "Thing.cls" -> thingContent
      )
    ) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames = {
        org.unmanaged
          .getRenameLocations(
            path,
            dummyContentAndCursorPos._2.line,
            dummyContentAndCursorPos._2.offset,
            Some(dummyContentAndCursorPos._1)
          )
      }
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(5, 0, 5, 7))
      // The dependencyHolders are not always returned in the same order which makes this test flakey.
      // Not checking for file names 'Foo' and 'Thing' because they alternate but the location in both files is the sane
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(3, 6, 3, 13))
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(3, 6, 3, 13))
      assert(renames(3).path == "/Dummy.cls")
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 21, 2, 28))
    }
  }

  test("Rename: Variable | Scope: Class | rename var from constructor") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |public Dummy() {
         |String newVar = my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | Scope: Class | rename var from initializer block") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |{
         |String newVar = my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: usage | Statement: LocalVariableDeclarationStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |private void privateMethod(){
         |String newVar = my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: usage | Statement: ReturnStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField;
         |private void privateMethod(){
         |return my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 7, 4, 14))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: usage | Statement: IfStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField = 'abc';
         |private String privateMethod(){
         |if(my${CURSOR}Field.length() > 0){
         |  myField = 'test';
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(5, 2, 5, 9))
      assert(renames(0).edits(1) == Location(4, 3, 4, 10))
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: usage | Statement: Basic ForStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 1;
         |private void privateMethod(){
         |for(Decimal i = my${CURSOR}Field; i < myField * 2; i += myField){
         |  myField = 1;
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 4)
      assert(renames(0).edits(0) == Location(4, 47, 4, 54))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))
      assert(renames(0).edits(2) == Location(4, 16, 4, 23))
      assert(renames(0).edits(3) == Location(4, 29, 4, 36))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: ForInit declaration | Statement: Basic ForStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |private void privateMethod(){
         |for(Decimal cou${CURSOR}nter = 5; counter < 10; counter ++){
         |  counter = counter + 2;
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 5)
      assert(renames(0).edits(0) == Location(3, 25, 3, 32))
      assert(renames(0).edits(1) == Location(3, 12, 3, 19))
      assert(renames(0).edits(2) == Location(4, 12, 4, 19))
      assert(renames(0).edits(3) == Location(4, 2, 4, 9))
      assert(renames(0).edits(4) == Location(3, 39, 3, 46))
    }
  }

  test("Rename: Variable | From: usage | Statement: Enhanced ForStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal[] myField = new List<Decimal>{1,2,3,4,5};
         |private void privateMethod(){
         |for(Decimal loopVar : my${CURSOR}Field){
         |  loopVar = 2;
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 22, 4, 29))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 17, 2, 24))

    }
  }

  test("Rename: Variable | From: EnhancedFor Declaration | Statement: Enhanced ForStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal[] myField = new List<Decimal>{1,2,3,4,5};
         |private void privateMethod(){
         |for(Decimal loop${CURSOR}Var : myField){
         |  loopVar = 2;
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 1)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(4, 12, 4, 19))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))

    }
  }

  test("Rename: Variable | From: usage | Statement: WhileStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |while (my${CURSOR}Field < 10) {
         |	myField = myField + 1;
         |	}
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(5, 11, 5, 18))
      assert(renames(0).edits(1) == Location(4, 7, 4, 14))
      assert(renames(0).edits(2) == Location(5, 1, 5, 8))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: DoWhileStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |do {
         |  myField = myField + 1;
         |} while (my${CURSOR}Field < 10) 
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(5, 2, 5, 9))
      assert(renames(0).edits(1) == Location(5, 12, 5, 19))
      assert(renames(0).edits(2) == Location(6, 9, 6, 16))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: TryStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |try{
         |  my${CURSOR}Field = 1 / 0;
         |  } catch(myField) {
         |  myField = 1;
         |  } finally {
         |  myField = 2;
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(5, 2, 5, 9))
      assert(renames(0).edits(1) == Location(9, 2, 9, 9))
      assert(renames(0).edits(2) == Location(7, 2, 7, 9))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: ThrowStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | throw my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 7, 4, 14))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: InsertStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | insert my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 8, 4, 15))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: UpdateStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | update my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 8, 4, 15))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: DeleteStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | delete my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 8, 4, 15))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: UndeleteStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | undelete my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 10, 4, 17))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: UpsertStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | upsert my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 8, 4, 15))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: MergeStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | merge my${CURSOR}Field myField;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(4, 7, 4, 14))
      assert(renames(0).edits(1) == Location(4, 15, 4, 22))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: RunAsStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | System.runAs(myField){
         |  my${CURSOR}Field;
         | }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(4, 14, 4, 21))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))

    }
  }

  test("Rename: Variable | From: usage | Statement: SOQL") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | List<SObject> things = [
         |  SELECT Id
         |	FROM Account
         |	WHERE Name = :my${CURSOR}Field
         | ]
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(7, 15, 7, 22))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: SOSL") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         | List<SObject> things = [
         |	FIND :my${CURSOR}Field
         |	IN ALL FIELDS
         |	RETURNING Account(Name)
         | ]
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(5, 7, 5, 14))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: ArrayExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |String arr = new List<String>{'a', 'b', 'c'}
         |Decimal newVar = arr[my${CURSOR}Field];
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(5, 21, 5, 28))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: CastExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Decimal newVar = (Decimal) my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 27, 4, 34))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: New set expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Set<Decimal> newSet = new Set<Decimal>{
         |  my${CURSOR}Field,
         |  myField
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(6, 2, 6, 9))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: New list expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |List<Decimal> newSet = new List<Decimal>{
         |  my${CURSOR}Field,
         |  myField
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(6, 2, 6, 9))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: New array expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Decimal[] newSet = new Decimal[]{
         |  my${CURSOR}Field,
         |  myField
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(6, 2, 6, 9))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: New map expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |new Map<Decimal, Decimal>{
         |  myField => my${CURSOR}Field
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(5, 13, 5, 20))
      assert(renames(0).edits(1) == Location(5, 2, 5, 9))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: New class expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |new PrivateClass(my${CURSOR}Field);
         |}
         |private class PrivateClass {
         |	PrivateClass(Decimal thing) {}
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 17, 4, 24))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: NegationExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String myField = 'abc';
         |private void privateMethod(){
         |if (!my${CURSOR}Field.isAllLowerCase()) {}
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 5, 4, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Rename: Variable | From: usage | Statement: SubExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Decimal thing = 1 * (my${CURSOR}Field + 5);
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 21, 4, 28))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: PostfixExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Decimal thing = my${CURSOR}Field++;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: PrefixExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Decimal thing = ++my${CURSOR}Field;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 18, 4, 25))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Variable | From: usage | Statement: InstanceOfExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal myField = 5;
         |private void privateMethod(){
         |Boolean thing = my${CURSOR}Field instanceOf System.Decimal;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(4, 16, 4, 23))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 22))
    }
  }

  test("Rename: Method | Declaration is cached") {
    val dummyContent =
      """public class Dummy {
        |public static String targetMethod() {
        |return 'A string value';
        |}
        |
        |private void privateMethod(){
        |targetMethod();
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")
    val fooContentAndCursorPos = withCursorMultiLine(s"""public class Foo {
         |private void privateMethod(){
         |Dummy.target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    withManualFlush {
      FileSystemHelper.run(
        Map("Dummy.cls" -> dummyContent, "Foo.cls" -> fooContentAndCursorPos._1)
      ) { root: PathLike =>
        val path = root.join("Foo.cls")
        val org  = createHappyOrg(root)
        org.flush()

        // Reload from cache
        createOrg(root)

        val newOrg = createOrg(root)
        newOrg.flush()

        val renames = {
          newOrg.unmanaged
            .getRenameLocations(
              path,
              fooContentAndCursorPos._2.line,
              fooContentAndCursorPos._2.offset,
              Some(fooContentAndCursorPos._1)
            )
        }
        assert(renames.length == 3)
        assert(renames(0).path == "/Dummy.cls")
        assert(renames(0).edits.length == 1)
        assert(renames(0).edits(0) == Location(7, 0, 7, 12))
        assert(renames(1).path == "/Foo.cls")
        assert(renames(1).edits.length == 1)
        assert(renames(1).edits(0) == Location(3, 6, 3, 18))
        assert(renames(2).path == "/Dummy.cls")
        assert(renames(2).edits.length == 1)
        assert(renames(2).edits(0) == Location(2, 21, 2, 33))
      }
    }
  }

  test("Rename: Method | cross-file") {
    val dummyContent = """public class Dummy {
         |public static String targetMethod() {
         |return 'A string value';
         |}
         |
         |private void privateMethod(){
         |targetMethod();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n")
    val fooContentAndCursorPos = withCursorMultiLine(s"""public class Foo {
         |private void privateMethod(){
         |Dummy.target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> dummyContent, "Foo.cls" -> fooContentAndCursorPos._1)) {
      root: PathLike =>
        val org  = createOrg(root)
        val path = root.join("Foo.cls")

        val renames = {
          org.unmanaged
            .getRenameLocations(
              path,
              fooContentAndCursorPos._2.line,
              fooContentAndCursorPos._2.offset,
              Some(fooContentAndCursorPos._1)
            )
        }
        assert(renames.length == 3)
        assert(renames(0).path == "/Dummy.cls")
        assert(renames(0).edits.length == 1)
        assert(renames(0).edits(0) == Location(7, 0, 7, 12))
        assert(renames(1).path == "/Foo.cls")
        assert(renames(1).edits.length == 1)
        assert(renames(1).edits(0) == Location(3, 6, 3, 18))
        assert(renames(2).path == "/Dummy.cls")
        assert(renames(2).edits.length == 1)
        assert(renames(2).edits(0) == Location(2, 21, 2, 33))
    }
  }

  test("Rename: Method | multiple different blocks") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'A string value';
         |}
         |private void privateMethod(){
         |target${CURSOR}Method();
         |}
         |private void privateMethod2(){
         |target${CURSOR}Method();
         |}
         |private void privateMethod3(){
         |target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 4)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 0, 6, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(9, 0, 9, 12))
      assert(renames(2).path == "/Dummy.cls")
      assert(renames(2).edits.length == 1)
      assert(renames(2).edits(0) == Location(12, 0, 12, 12))
      assert(renames(3).path == "/Dummy.cls")
      assert(renames(3).edits.length == 1)
      assert(renames(3).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | does not rename different methods with the same name") {
    val dummyContent =
      """public class Dummy {
        |public static String targetMethod() {
        |return 'A string value';
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")
    val dummy2Content =
      """public class Dummy2 {
        |public static String targetMethod() {
        |return 'A string value';
        |}
        |}""".stripMargin.replaceAll("\r\n", "\n")
    val fooContentAndCursorPos = withCursorMultiLine(s"""public class Foo {
         |private void privateMethod(){
         |Dummy.target${CURSOR}Method();
         |Dummy2.targetMethod();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(
      Map(
        "Dummy.cls"  -> dummyContent,
        "Foo.cls"    -> fooContentAndCursorPos._1,
        "Dummy2.cls" -> dummy2Content
      )
    ) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Foo.cls")

      val renames = {
        org.unmanaged
          .getRenameLocations(
            path,
            fooContentAndCursorPos._2.line,
            fooContentAndCursorPos._2.offset,
            Some(fooContentAndCursorPos._1)
          )
      }
      assert(renames.length == 2)
      assert(renames(0).path == "/Foo.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(3, 6, 3, 18))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 21, 2, 33))

    }
  }

  test("Rename: Method | From: Declaration") {
    val dummyContentAndCursorPos =
      withCursorMultiLine(s"""public class Dummy {
          |public static String target${CURSOR}Method() {
          |return 'A string value';
          |}
          |
          |private void privateMethod(){
          |targetMethod();
          |}
          |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> dummyContentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames = {
        org.unmanaged
          .getRenameLocations(
            path,
            dummyContentAndCursorPos._2.line,
            dummyContentAndCursorPos._2.offset,
            Some(dummyContentAndCursorPos._1)
          )
      }
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(7, 0, 7, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 21, 2, 33))
    }
  }

  test("Rename: Method | Call-Out Holder: Method Declaration") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'A string value';
         |}
         |private void privateMethod(){
         |target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 0, 6, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | Call-Out Holder: Constructor Declaration") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |Dummy(){
         |target${CURSOR}Method();
         |}
         |public String targetMethod() {
         |return 'A string value';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(3, 0, 3, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(5, 14, 5, 26))
    }
  }

  test("Rename: Method | Call-Out Holder: Field Declaration") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |static String newName = target${CURSOR}Method();
         |public static String targetMethod() {
         |return 'A string value';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(2, 24, 2, 36))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(3, 21, 3, 33))
    }
  }

  test("Rename: Method | Call-Out Holder: Initializer") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |{
         |target${CURSOR}Method();
         |}
         |public String targetMethod() {
         |return 'A string value';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(3, 0, 3, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(5, 14, 5, 26))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: ExpressionStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'A string value';
         |}
         |private void privateMethod(){
         |target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 0, 6, 12))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: LocalVariableDeclarationStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'A string value';
         |}
         |private void privateMethod(){
         |String testVar = target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 17, 6, 29))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: ReturnStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'A string value';
         |}
         |private String privateMethod(){
         |return target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 7, 6, 19))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: IfStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'A string value';
         |}
         |private String privateMethod(){
         |if(target${CURSOR}Method().length() > 0){
         |  targetMethod();
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(6, 3, 6, 15))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: Basic ForStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |for(Decimal i = targetMethod(); i < targetMethod() * 2; i += targetMethod()){
         |  target${CURSOR}Method();
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 4)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(6, 61, 6, 73))
      assert(renames(0).edits(2) == Location(6, 36, 6, 48))
      assert(renames(0).edits(3) == Location(6, 16, 6, 28))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))

    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: Enhanced ForStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return [1,2,3,4,5];
         |}
         |private void privateMethod(){
         |for(Decimal x : targetMethod()){
         |  target${CURSOR}Method();
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(6, 16, 6, 28))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))

    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: WhileStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |while (targetMethod() < 10) {
         |	target${CURSOR}Method();
         |	}
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(6, 7, 6, 19))
      assert(renames(0).edits(1) == Location(7, 1, 7, 13))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: DoWhileStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public Decimal targetMethod() {
           |return 5;
           |}
           |private void privateMethod(){
           |do {
           |	target${CURSOR}Method();
           |} while (targetMethod() < 10)
           |}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 1, 7, 13))
      assert(renames(0).edits(1) == Location(8, 9, 8, 21))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: TryStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |try{
         |  target${CURSOR}Method();
         |  } catch(Exception e) {
         |  targetMethod();
         |  } finally {
         |  targetMethod();
         |  }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 3)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(9, 2, 9, 14))
      assert(renames(0).edits(2) == Location(11, 2, 11, 14))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: ThrowStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | throw target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 7, 6, 19))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: InsertStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | insert target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 8, 6, 20))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: UpdateStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | update target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 8, 6, 20))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: DeleteStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | delete target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 8, 6, 20))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: UndeleteStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | undelete target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 10, 6, 22))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: UpsertStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | upsert target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 8, 6, 20))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: MergeStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | merge target${CURSOR}Method() targetMethod();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(6, 7, 6, 19))
      assert(renames(0).edits(1) == Location(6, 22, 6, 34))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))

    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: RunAsStatement") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | System.runAs(targetMethod()){
         |  target${CURSOR}Method();
         | }
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(6, 14, 6, 26))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))

    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: SOQL") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | List<SObject> things = [
         |  SELECT Id
         |	FROM Account
         |	WHERE Name = :target${CURSOR}Method()
         | ]
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(9, 15, 9, 27))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: SOSL") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | List<SObject> things = [
         |	FIND :target${CURSOR}Method()
         |	IN ALL FIELDS
         |	RETURNING Account(Name)
         | ]
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(7, 7, 7, 19))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: String concatenation") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         | String newStr = 'start ' + target${CURSOR}Method() + ' extra string text';
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 28, 6, 40))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: Function call parameter") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |anotherMethod(target${CURSOR}Method());
         |}
         |private void anotherMethod(String test){
         | test;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 14, 6, 26))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: ArrayExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Integer targetMethod() {
         |return 2;
         |}
         |private void privateMethod(){
         |String arr = new List<String>{'a', 'b', 'c'}
         |Decimal newVar = arr[target${CURSOR}Method()];
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(7, 21, 7, 33))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: CastExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Integer targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Decimal newVar = (Decimal) target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 27, 6, 39))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: New set expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Integer targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Set<Decimal> newSet = new Set<Decimal>{
         |  target${CURSOR}Method(),
         |  targetMethod()
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(8, 2, 8, 14))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: New list expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Integer targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |List<Decimal> newSet = new List<Decimal>{
         |  target${CURSOR}Method(),
         |  targetMethod()
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(8, 2, 8, 14))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: New array expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Decimal[] decArray = new Decimal[]{
         |  target${CURSOR}Method(),
         |  targetMethod()
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(8, 2, 8, 14))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: New map expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |new Map<Decimal, Decimal>{
         |  targetMethod() => target${CURSOR}Method()
         |  };
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 2)
      assert(renames(0).edits(0) == Location(7, 2, 7, 14))
      assert(renames(0).edits(1) == Location(7, 20, 7, 32))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: New class expression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |new PrivateClass(target${CURSOR}Method());
         |}
         |private class PrivateClass {
         |	PrivateClass(Decimal thing) {}
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 17, 6, 29))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: NegationExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public String targetMethod() {
         |return 'test';
         |}
         |private void privateMethod(){
         |if (!target${CURSOR}Method().isAllLowerCase()) {}
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 5, 6, 17))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 14, 2, 26))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: SubExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Decimal thing = 1 * (target${CURSOR}Method() + 5);
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 21, 6, 33))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: PostfixExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Decimal thing = target${CURSOR}Method()++;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 16, 6, 28))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: PrefixExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Decimal thing = ++target${CURSOR}Method();
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 18, 6, 30))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }

  test("Rename: Method | From: Call-out Statement | Statement: InstanceOfExpression") {
    val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
         |public Decimal targetMethod() {
         |return 5;
         |}
         |private void privateMethod(){
         |Boolean thing = target${CURSOR}Method() instanceOf System.Decimal;
         |}
         |}""".stripMargin.replaceAll("\r\n", "\n"))

    FileSystemHelper.run(Map("Dummy.cls" -> contentAndCursorPos._1)) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Dummy.cls")

      val renames =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(renames.length == 2)
      assert(renames(0).path == "/Dummy.cls")
      assert(renames(0).edits.length == 1)
      assert(renames(0).edits(0) == Location(6, 16, 6, 28))
      assert(renames(1).path == "/Dummy.cls")
      assert(renames(1).edits.length == 1)
      assert(renames(1).edits(0) == Location(2, 15, 2, 27))
    }
  }
}
