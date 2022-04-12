/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser

import com.financialforce.oparser.UnresolvedTypeRef
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.workspace.IPM

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

    val files: Seq[Path] = getFilesFromPath(absolutePath)
    val sources: Map[String, String] = files
      .map(path => {
        path.toString -> getUTF8ContentsFromPath(path)
      })
      .toMap
    checkOPResolutions(sources)

    def toPercentage(result: Int) = {
      (result / files.size.toFloat) * 100
    }

    println(f"""
         |Output Comparison Summary
         |Total cls files processed: ${files.size}
         |Total comparisons: $total
         |Parse Failures: $parseFailure (${toPercentage(parseFailure)}%.0f%%)
         |Exactly Equal: $exactlyEqual (${toPercentage(exactlyEqual)}%.0f%%)
         |Files with comparison warnings: $withWarnings (${toPercentage(withWarnings)}%.0f%%)
         |Files with comparison errors: $errors (${toPercentage(errors)}%.0f%%)
         |""".stripMargin)
  }

  //Temp
  private def checkOPResolutions(sources: Map[String, String]): Unit = {
    FileSystemHelper.run(sources) { root: PathLike =>
      val index = new IPM.Index(root)
      sources.keys
        .filter(_.endsWith("cls"))
        .filterNot(_.contains(".sfdx/tools/"))
        .foreach(f => {
          val file = index.rootModule.get.findTypesByPath(f)
          if (file.nonEmpty) {
            val op = file.head
            val ex = op.extendsTypeRef match {
              case un: UnresolvedTypeRef => Some(un)
              case _                     => None
            }
            val impl = if (op.implementsTypeList != null) op.implementsTypeList.typeRefs collect {
              case un: UnresolvedTypeRef => un
            }
            else ArrayBuffer.empty
            val cons = op.constructors.filter(
              x =>
                x.formalParameterList.formalParameters
                  .flatMap(_.typeRef)
                  .collect({ case un: UnresolvedTypeRef => un })
                  .nonEmpty
            )
            val meths = op.methods.filter(
              x =>
                x.formalParameterList.formalParameters
                  .flatMap(_.typeRef)
                  .collect({ case un: UnresolvedTypeRef => un })
                  .nonEmpty
            )
            val props = op.properties.filter(x => x.typeRef.isInstanceOf[UnresolvedTypeRef])
            val fi    = op.fields.filter(x => x.typeRef.isInstanceOf[UnresolvedTypeRef])

            if (
              ex.nonEmpty ||
              impl.nonEmpty ||
              cons.nonEmpty ||
              meths.nonEmpty ||
              props.nonEmpty ||
              fi.nonEmpty
            ) {
              println(f)
              if (ex.nonEmpty) println(ex.map(_.getFullName))
              if (impl.nonEmpty) println(impl.map(_.getFullName))
              if (cons.nonEmpty)
                println(
                  cons
                    .map(
                      _.formalParameterList.formalParameters.flatMap(_.typeRef).map(_.getFullName)
                    )
                )
              if (meths.nonEmpty)
                println(
                  meths
                    .map(
                      _.formalParameterList.formalParameters.flatMap(_.typeRef).map(_.getFullName)
                    )
                )
              if (props.nonEmpty) println(props.map(_.typeRef.getFullName))
              if (fi.nonEmpty) println(fi.map(_.typeRef.getFullName))
              println(" ")

            }
          }
        })

    }
  }

  private def getFilesFromPath(absolutePath: Path) = {
    if (Files.isDirectory(absolutePath)) {
      println("Directory")
      val s = Files.walk(absolutePath)
      s.filter(file => !Files.isDirectory(file))
        .filter(
          file =>
            file.getFileName.toString.toLowerCase
              .endsWith("cls") || file.getFileName.toString.toLowerCase.endsWith("-meta.xml")
        )
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
