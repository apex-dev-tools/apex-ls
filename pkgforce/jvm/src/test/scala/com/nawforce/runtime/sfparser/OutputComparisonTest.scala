/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser

import com.financialforce.oparser.{OutlineParser, TypeDeclaration}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.sfparser.compare.{SubsetComparator, TypeIdCollector}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer

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
    if (args.length < 2) {
      System.err.println(
        s"Not enough arguments provided, expected workspace directory and apex db path, '${args.mkString(", ")}'}"
      )
      return
    }

    val absolutePath = Paths.get(Option(args.head).getOrElse("")).toAbsolutePath.normalize()
    val dbpath = Paths.get(args.tail.headOption.getOrElse("")).toAbsolutePath.normalize()

    val files: Seq[Path] = getFilesFromPath(absolutePath)
    val sources: Map[String, String] = files
      .map(path => {
        path.toString -> getUTF8ContentsFromPath(path)
      })
      .toMap
    val sfParserOutput = SFParser(sources).parseClassWithSymbolProvider(SymbolProvider(dbpath))
    if (sfParserOutput._2.nonEmpty) {
      parseFailure = sfParserOutput._2.size
      System.err.println(
        s"Some files will not be compared due to parse failure: ${sfParserOutput._2.mkString(", ")}"
      )
    }
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      files
        .filterNot(x => sfParserOutput._2.contains(x.toAbsolutePath.toString))
        .foreach(f => {
          val sf = findSfParserOutput(f.toAbsolutePath, sfParserOutput)
          val op = index.rootModule.get.findExactTypeId(sf.get.getFullName)
          compareResolved(op, sf)
        })

    }

    def toPercentage(result: Int) = {
      (result / files.size.toFloat) * 100
    }

    println(
      f"""
         |Output Comparison Summary
         |Total cls files processed: ${files.size}
         |Total comparisons: $total
         |Parse Failures: $parseFailure (${toPercentage(parseFailure)}%.0f%%)
         |Exactly Equal: $exactlyEqual (${toPercentage(exactlyEqual)}%.0f%%)
         |Files with comparison warnings: $withWarnings (${toPercentage(withWarnings)}%.0f%%)
         |Files with comparison errors: $errors (${toPercentage(errors)}%.0f%%)
         |""".stripMargin)
  }

  private def getOutLineParserOutput(path: Path) = {
    val contentsString = getUTF8ContentsFromPath(path)
    val result = OutlineParser.parse(path.toString, contentsString)
    (result._1, result._2, result._3.map(_.asInstanceOf[IModuleTypeDeclaration]))
  }

  private def findSfParserOutput(
    path: Path,
    output: (ArrayBuffer[TypeDeclaration], ArrayBuffer[String])
  ) = {
    output._1.find(_.paths.head == path.toString)
  }

  private def compareResolved(
                               fromIndex: Option[IModuleTypeDeclaration],
                               fromSf: Option[TypeDeclaration]
  ): Unit = {
    val comparator = SubsetComparator(
      fromIndex.get,
      TypeIdCollector.fromIModuleTypeDecls(List.empty),
      TypeIdCollector.fromTypeDecls(List.empty)
    )
    comparator.subsetOf(fromSf.get)
  }

  private def compareOutputs(
    path: Path,
    sfOutput: (ArrayBuffer[TypeDeclaration], ArrayBuffer[String]),
    sfTypeIdResolver: TypeIdCollector
  ): Unit = {
    val (success, reason, opOut) = getOutLineParserOutput(path)
    val sfTd                     = findSfParserOutput(path, sfOutput)

    total += 1
    if (!success) {
      parseFailure += 1
      System.err.println(s"Parse Failure $path $reason")
      return
    }
    try {
      val opResolver = TypeIdCollector.fromIModuleTypeDecls(List(opOut.get))
      val comparator = SubsetComparator(opOut.get, opResolver, sfTypeIdResolver)
      comparator.subsetOf(sfTd.get)
      val warnings = comparator.getWarnings
      if (warnings.nonEmpty) {
        withWarnings += 1
        //TODO: Process warnings?
      } else {
        exactlyEqual += 1
      }
    } catch {
      case ex: Throwable =>
        errors += 1
        System.err.println(s"Failed output on $path due to ${ex.getMessage}")
    }
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

  private def getUTF8ContentsFromPath(absolutePath: Path): String = {
    val contentsBytes = Files.readAllBytes(absolutePath)
    new String(contentsBytes, "utf8")
  }
}
