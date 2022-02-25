package com.nawforce.runtime.sfparser

import com.financialforce.oparser.{
  ClassTypeDeclaration,
  EnumTypeDeclaration,
  InterfaceTypeDeclaration,
  OutlineParser,
  TypeDeclaration
}

import java.nio.file.{Files, Path, Paths}

object OutputComparisonTest {
  var exactlyEqual = 0
  var withWarnings = 0
  var errors       = 0
  var total        = 0

  def main(args: Array[String]): Unit = {

    if (args.isEmpty) {
      System.err.println(s"No workspace directory argument provided.")
      return
    }
    if (args.length > 1) {
      System.err.println(
        s"Multiple arguments provided, expected workspace directory, '${args.mkString(", ")}'}"
      )
      return
    }
    val absolutePath = Paths.get(Option(args.head).getOrElse("")).toAbsolutePath.normalize()

    val files: Seq[Path] = getFilesFromPath(absolutePath)

    files.foreach(f => {
      parseFiles(f)
    })
    println(s"""
        |Output Comparison Summary
        |Total files parsed: ${total}
        |Exactly Equal: ${exactlyEqual}
        |Files with comparison warnings: ${withWarnings}
        |Files with comparison errors: ${errors}
        |""".stripMargin)
  }

  private def parseFiles(path: Path): Unit = {
    val contentsBytes            = Files.readAllBytes(path)
    val contentsString: String   = new String(contentsBytes, "utf8")
    val (success, reason, opOut) = OutlineParser.parse(path.toString, contentsString)
    val sfOut                    = SFParser(path.toString, contentsString).parse
    total += 1
    if (!success) {
      System.err.println(s"PARSE FAILURE $path")
    }
    try {
      val warnings = compareTDs(opOut.get, sfOut.get)
      if (warnings.nonEmpty) {
        withWarnings += 1
//        println(s"Comparison Warnings for path ${path}")
//        warnings.foreach(println)
      } else {
        exactlyEqual += 1
      }
    } catch {
      case ex: Throwable =>
        errors += 1
        System.err.println(s"Failed output comparison: $path")
        System.err.println(ex.getMessage)
    }
  }

  private def compareTDs(td: TypeDeclaration, other: TypeDeclaration) = {
    SubsetCompare.clearWarnings()
    td match {
      case cls: ClassTypeDeclaration =>
        SubsetCompare.subsetOffClassDeclarations(cls, other.asInstanceOf[ClassTypeDeclaration])
      case int: InterfaceTypeDeclaration =>
        SubsetCompare.compareInterfaceTypeDeclarations(
          int,
          other.asInstanceOf[InterfaceTypeDeclaration]
        )
      case enm: EnumTypeDeclaration =>
        SubsetCompare.compareEnumTypeDeclarations(enm, other.asInstanceOf[EnumTypeDeclaration])
      case _ =>
    }
    SubsetCompare.getWarnings()
  }

  private def getFilesFromPath(absolutePath: Path) = {
    if (Files.isDirectory(absolutePath)) {
      println("Directory")
      val s = Files.walk(absolutePath)
      s.filter(file => !Files.isDirectory(file))
        .filter(file => file.getFileName.toString.toLowerCase.endsWith("cls"))
        .toArray
        .map(_.asInstanceOf[Path])
        .toIndexedSeq
    } else {
      println("Single file")
      Seq(absolutePath)
    }
  }
}
