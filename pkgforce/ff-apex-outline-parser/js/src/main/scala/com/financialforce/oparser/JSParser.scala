/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import io.scalajs.nodejs.fs.Fs
import io.scalajs.nodejs.path.{Path => NodePath}
import io.scalajs.nodejs.process.Process

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object JSParser {
  def main(args: Array[String]): Unit = {
    Process.exit(Parser.run(Process.argv.tail.tail.toArray))
  }
}

object Parser {

  def run(args: Array[String]): Int = {

    println(args.length)

    if (args.length != 1) {
      System.err.println(
        s"Provide a single path for an Apex class or a directory to search for classes"
      )
      return 1
    }

    parse(display = false, args.head)

    println(s"Number of files: ${totalFileCount.longValue}")
    println(s"Total Length: ${totalFileLength.longValue()} bytes")
    println(s"Parse Time: ${totalParseTime.longValue() / 1e3}s")
    0
  }

  def parse(display: Boolean, inPath: String): Unit = {
    val path: String = Option(inPath).getOrElse(Process.cwd())
    val absolutePath = NodePath.resolve(path).pipe(NodePath.normalize)

    println(absolutePath + " JS")

    val stat = Fs.statSync(absolutePath)
    val files = if (stat.isDirectory()) {
      println("Directory")
      findClassFiles(absolutePath)
    } else {
      println("Single File")
      Array(absolutePath)
    }
    files.foreach(f => {
      val td = parseFile(f)
      if (display) {
        println("=====================")
        println(td.get)
      }
    })
  }

  private val totalParseTime = new AtomicLong(0)

  def addParseTime(t: Long): Unit = {
    totalParseTime.addAndGet(t)
  }

  private val totalFileCount = new AtomicLong(0)

  def addFileCount(t: Long): Unit = {
    totalFileCount.addAndGet(t)
  }

  private val totalFileLength = new AtomicLong(0)

  def addFileLength(t: Long): Unit = {
    totalFileLength.addAndGet(t)
  }

  private def parseFile(path: String): Option[TypeDeclaration] = {
    addFileCount(1)
    val contents: Array[Byte] = Fs.readFileSync(path).values().toIterator.map(_.toByte).toArray
    addFileLength(contents.length)

    val start = System.currentTimeMillis()
    val (success, reason, td) = OutlineParser.parse(path, new String(contents, "utf8"))
    if (!success)
      println(s"Error while parsing $path ${reason.get}")
    addParseTime(System.currentTimeMillis() - start)
    td
  }

  private def findClassFiles(directory: String): Array[String] = {

    Fs.readdirSync(directory)
      .foldLeft(mutable.ArrayBuffer.empty[String]) {
        case (acc: mutable.AbstractBuffer[String], entry: String) =>
          val path = NodePath.join(directory, entry)
          val exists = Fs.existsSync(path)
          if (!exists) acc
          else {
            val stat = Fs.lstatSync(path)
            if (stat.isDirectory()) acc ++ findClassFiles(path)
            else if (NodePath.extname(path).toLowerCase.equals(".cls")) acc ++ Seq(path)
            else acc
          }
      }
      .toArray
  }
}
