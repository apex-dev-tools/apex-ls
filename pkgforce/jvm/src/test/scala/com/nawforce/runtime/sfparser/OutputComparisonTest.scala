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
  var parseFailure = 0

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
    def toPercentage(result: Int) = {
      (result / total.toFloat) * 100
    }
    println(f"""
         |Output Comparison Summary
         |Total cls files processed: $total
         |Parse Failures: $parseFailure (${toPercentage(parseFailure)}%.0f%%)
         |Exactly Equal: $exactlyEqual (${toPercentage(exactlyEqual)}%.0f%%)
         |Files with comparison warnings: $withWarnings (${toPercentage(withWarnings)}%.0f%%)
         |Files with comparison errors: $errors (${toPercentage(errors)}%.0f%%)
         |""".stripMargin)
  }

  private def parseFiles(path: Path): Unit = {
    val contentsBytes            = Files.readAllBytes(path)
    val contentsString: String   = new String(contentsBytes, "utf8")
    val (success, reason, opOut) = OutlineParser.parse(path.toString, contentsString)
    val sfOut                    = SFParser(path.toString, contentsString).parse
    total += 1
    if (!success || sfOut.isEmpty) {
      parseFailure += 1
      System.err.println(s"Parse Failure $path $reason")
      return
    }
    try {
      val warnings = compareTDs(opOut.get, sfOut.get)
      if (warnings.nonEmpty) {
        withWarnings += 1
        //TODO: Process warnings?
      } else {
        exactlyEqual += 1
      }
    } catch {
      case ex: Throwable =>
        errors += 1
        System.err.println(s"Failed output on ${path} due to ${ex.getMessage}")
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
    SubsetCompare.getWarnings
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
