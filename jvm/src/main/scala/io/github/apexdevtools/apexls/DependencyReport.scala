/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package io.github.apexdevtools.apexls

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.runtime.platform.Path
import mainargs.{Flag, ParserForMethods, arg, main}

import scala.annotation.unused
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** Command line for running project analysis.
  *
  * Defaults to reporting issue but can also be used to report dependency information.
  */
object DependencyReport {
  private final val STATUS_OK: Int        = 0
  private final val STATUS_ARGS: Int      = 1
  private final val STATUS_EXCEPTION: Int = 3

  @unused
  @main(name = "io.github.apexdevtools.apexls.DependencyReport")
  def mainWithArgs(
    @arg(short = 'f', doc = "Output format text (default) or json")
    format: String = "text",
    @arg(short = 'l', doc = "Text output logging level, none (default), info or debug")
    logging: String = "none",
    @arg(short = 'n', doc = "Disable cache use")
    nocache: Flag,
    @arg(short = 'w', doc = "Workspace directory path, defaults to current directory")
    workspace: String = ""
  ): Unit = {
    System.exit(run(format, logging, nocache.value, workspace))
  }

  def main(args: Array[String]): Unit = {
    ParserForMethods(this).runOrExit(ArraySeq.unsafeWrapArray(args))
  }

  def run(format: String, logging: String, nocache: Boolean, directory: String): Int = {
    try {
      val workspace = Path(directory)
      val outputFormat = format match {
        case "text" | "json" => format
        case _ =>
          System.err.println(
            s"Unknown output format provided '$format', should be 'text' or 'json'"
          )
          return STATUS_ARGS
      }

      val loggingLevel =
        if (outputFormat != "text")
          "none"
        else
          logging match {
            case "none" | "info" | "debug" => logging
            case _ =>
              System.err.println(
                s"Unknown logging level provided '$logging', should be 'none', 'info' or 'debug'"
              )
              return STATUS_ARGS
          }

      val options = OpenOptions
        .default()
        .withParser("OutlineSingle")
        .withAutoFlush(enabled = false)
        .withLoggingLevel(loggingLevel)
        .withCache(!nocache)
        .withUnused(enabled = false)

      // Load org and flush to cache if we are using it
      val org = Org.newOrg(Path(workspace), options)
      if (!nocache) {
        org.flush()
      }

      // Output issues
      if (outputFormat == "json") {
        writeDependenciesAsJSON(org)
      } else {
        writeDependenciesAsCSV(org)
      }
      STATUS_OK

    } catch {
      case ex: Throwable =>
        ex.printStackTrace(System.err)
        STATUS_EXCEPTION
    }
  }

  private def writeDependenciesAsJSON(org: Org): Unit = {
    val buffer = new mutable.StringBuilder()
    var first  = true
    buffer ++= s"""{ "dependencies": [\n"""
    org.getDependencies.asScala.foreach(kv => {
      if (!first)
        buffer ++= ",\n"
      first = false

      buffer ++= s"""{ "name": "${kv._1}", "dependencies": ["""
      buffer ++= kv._2.map("\"" + _ + "\"").mkString(", ")
      buffer ++= s"]}"
    })
    buffer ++= "]}\n"
    print(buffer.mkString)
  }

  private def writeDependenciesAsCSV(org: Org): Unit = {
    org.getDependencies.asScala.foreach(kv => {
      println(s"${kv._1}, ${kv._2.mkString(", ")}")
    })
  }

}
