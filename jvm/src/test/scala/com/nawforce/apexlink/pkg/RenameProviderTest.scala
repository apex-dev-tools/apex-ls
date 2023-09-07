package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.TestHelper.CURSOR
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

class RenameProviderTest extends AnyFunSuite with TestHelper {
  test("Scope: Function - rename param var declaration") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod(String te${CURSOR}st) {
           |String fakeVar = test;
           |return test;
           |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(1, 51, 1, 55))
      assert(completions(0).edits(1) == Location(3, 7, 3, 11))
      assert(completions(0).edits(2) == Location(2, 17, 2, 21))
    }
  }

  test("Scope: Function - rename param var usage") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod(String test) {
          |String fakeVar = te${CURSOR}st;
          |return test;
          |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(2, 17, 2, 21))
      assert(completions(0).edits(1) == Location(3, 7, 3, 11))
      assert(completions(0).edits(2) == Location(1, 51, 1, 55))

    }
  }

  test("Scope: Function - rename function var declartion") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod() {
           |String te${CURSOR}st;
           |String fakeVar = test;
           |return test;
           |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(2, 7, 2, 11))
      assert(completions(0).edits(1) == Location(3, 17, 3, 21))
      assert(completions(0).edits(2) == Location(4, 7, 4, 11))
    }
  }

  test("Scope: Function - rename function var usage") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod() {
           |String test;
           |String fakeVar = te${CURSOR}st;
           |return test;
           |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(3, 17, 3, 21))
      assert(completions(0).edits(1) == Location(2, 7, 2, 11))
      assert(completions(0).edits(2) == Location(4, 7, 4, 11))
    }
  }

  test("Scope: Function - rename unused var") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod() {
           |String te${CURSOR}st;
           |String fakeVar = 'thing';
           |return fakeVar;
           |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 1)
      assert(completions(0).edits(0) == Location(2, 7, 2, 11))
    }
  }

  test("Scope: Function - rename var declaration with dot chaining") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod() {
           |Map<String, String> m${CURSOR}1 = new Map<String, String>();
           |m1.put('Type', 'Fruit');
           |m1.put('Colour', 'Green');
           |return m1.get('Type');
           |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 4)
      assert(completions(0).edits(0) == Location(2, 20, 2, 22))
      assert(completions(0).edits(1) == Location(5, 7, 5, 9))
      assert(completions(0).edits(2) == Location(3, 0, 3, 2))
      assert(completions(0).edits(3) == Location(4, 0, 4, 2))
    }
  }

  test("Scope: Function - rename var usage with dot chaining") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(
        s"""public class Dummy { public void someMethod() {
           |Map<String, String> m1 = new Map<String, String>();
           |m${CURSOR}1.put('Type', 'Fruit');
           |m1.put('Colour', 'Green');
           |return m1.get('Type');
           |} }""".stripMargin
          .replaceAll("\r\n", "\n")
      )
      val completions = org.unmanaged.getRenameLocations(
        path,
        contentAndCursorPos._2.line,
        contentAndCursorPos._2.offset,
        Some(contentAndCursorPos._1)
      )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 4)
      assert(completions(0).edits(0) == Location(3, 0, 3, 2))
      assert(completions(0).edits(1) == Location(5, 7, 5, 9))
      assert(completions(0).edits(2) == Location(2, 20, 2, 22))
      assert(completions(0).edits(3) == Location(4, 0, 4, 2))
    }
  }

  test("Scope: Class - rename var declaration") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String my${CURSOR}Field;
           |public void someMethod() {
           |String fakeVar = 'myField';
           |String test = myField;
           |}
           |public String methodA(){String test2 = myField;}
           |private String methodPrivate(){return myField;}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 4)
      assert(completions(0).edits(0) == Location(2, 14, 2, 21))
      assert(completions(0).edits(1) == Location(5, 14, 5, 21))
      assert(completions(0).edits(2) == Location(7, 39, 7, 46))
      assert(completions(0).edits(3) == Location(8, 38, 8, 45))
    }
  }

  test("Scope: Class - rename var usage") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String myField;
           |public void someMethod() {
           |String fakeVar = 'myField';
           |String test = my${CURSOR}Field;
           |}
           |public String methodA(){String test2 = myField;}
           |private String methodPrivate(){return myField;}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 4)
      assert(completions(0).edits(0) == Location(5, 14, 5, 21))
      assert(completions(0).edits(1) == Location(2, 14, 2, 21))
      assert(completions(0).edits(2) == Location(7, 39, 7, 46))
      assert(completions(0).edits(3) == Location(8, 38, 8, 45))
    }
  }

  test("Scope: Class - unused class var") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String my${CURSOR}Field;
           |public void someMethod() {
           |String fakeVar = 'myField';
           |}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 1)
      assert(completions(0).edits(0) == Location(2, 14, 2, 21))
    }
  }

  test("Scope: Class - function var shadows class var - rename function var declaration") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String myField;
           |public void someMethod() {
           |String myFi${CURSOR}eld = 'string value';
           |String newVar = myField
           |return myField
           |}
           |public String methodA(){String test2 = myField;}
           |private String methodPrivate(){return myField;}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(4, 7, 4, 14))
      assert(completions(0).edits(1) == Location(5, 16, 5, 23))
      assert(completions(0).edits(2) == Location(6, 7, 6, 14))
    }
  }

  test("Scope: Class - function var shadows class var - rename function var usage") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String myField;
           |public void someMethod() {
           |String myField = 'string value';
           |String newVar = my${CURSOR}Field
           |return myField
           |}
           |public String methodA(){String test2 = myField;}
           |private String methodPrivate(){return myField;}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(5, 16, 5, 23))
      assert(completions(0).edits(1) == Location(4, 7, 4, 14))
      assert(completions(0).edits(2) == Location(6, 7, 6, 14))
    }
  }

  test("Scope: Class - function var shadows class var - rename class var declaration") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String my${CURSOR}Field;
           |public void someMethod() {
           |String myField = 'string value';
           |String newVar = myField
           |return myField
           |}
           |public String methodA(){String test2 = myField;}
           |private String methodPrivate(){return myField;}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(2, 14, 2, 21))
      assert(completions(0).edits(1) == Location(8, 39, 8, 46))
      assert(completions(0).edits(2) == Location(9, 38, 9, 45))
    }
  }

  test("Scope: Class - function var shadows class var - rename class var usage") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      val org  = createOrg(root)
      val path = root.join("Completion.cls")
      val contentAndCursorPos = withCursorMultiLine(s"""public class Dummy {
           |public String myField;
           |public void someMethod() {
           |String myField = 'string value';
           |String newVar = myField
           |return myField
           |}
           |public String methodA(){String test2 = myField;}
           |private String methodPrivate(){return my${CURSOR}Field;}
           |}""".stripMargin.replaceAll("\r\n", "\n"))

      val completions =
        org.unmanaged.getRenameLocations(
          path,
          contentAndCursorPos._2.line,
          contentAndCursorPos._2.offset,
          Some(contentAndCursorPos._1)
        )
      assert(completions.length == 1)
      assert(completions(0).path == "/Completion.cls")
      assert(completions(0).edits.length == 3)
      assert(completions(0).edits(0) == Location(9, 38, 9, 45))
      assert(completions(0).edits(1) == Location(8, 39, 8, 46))
      assert(completions(0).edits(2) == Location(2, 14, 2, 21))
    }
  }
}
