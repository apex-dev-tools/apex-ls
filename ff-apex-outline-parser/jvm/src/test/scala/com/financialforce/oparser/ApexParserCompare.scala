package com.financialforce.oparser

import com.financialforce.oparser.testutil.Antlr

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.StreamConverters._

object JVMParser {
  def main(args: Array[String]): Unit = {
    System.exit(Parser.run(args))
  }
}

object Parser {
  def run(args: Array[String]): Int = {
    val options    = Set("-seq", "-notest", "-display", "-antlr")
    val sequential = args.contains("-seq")
    val test       = !args.contains("-notest") && !args.contains("-antlr")
    val display    = args.contains("-display")
    val onlyANTLR  = args.contains("-antlr")
    val paths      = args.filterNot(options.contains)

    if (paths.isEmpty) {
      System.err.println(s"No paths provided to search for Apex classes")
      return 1
    }

    paths.foreach(path => {
      val result =
        if (sequential)
          parseSeq(display, test, onlyANTLR, path)
        else
          parsePar(display, test, onlyANTLR, path)
      if (result != 0)
        return result
    })

    // All good
    0
  }

  def parseSeq(display: Boolean, test: Boolean, onlyANTLR: Boolean, inPath: String): Int = {
    val absolutePath = Paths.get(Option(inPath).getOrElse("")).toAbsolutePath.normalize()
    println("SEQUENTIAL " + absolutePath.toString + " JVM")

    val start = System.currentTimeMillis()
    val files: Seq[Path] = if (Files.isDirectory(absolutePath)) {
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
    val timeForFiles = System.currentTimeMillis() - start

    files.foreach(f => {
      val status = parseFileWithStatus(display, test, onlyANTLR, f)
      if (status != 0)
        return status
    })

    println(s"Time to get file list ${timeForFiles / 1e3}s")
    println(s"Number of files: ${files.length}")
    println(s"Total Length: ${totalLength.longValue()} bytes")
    println(s"Read File Time: ${totalReadFileTime.longValue() / 1e3}s")
    println(s"Convert File Time: ${totalConvertFileTime.longValue() / 1e3}s")
    println(s"Parse Time: ${totalParseTime.longValue() / 1e3}s")
    println(s"Antlr Time: ${totalAntlrTime.longValue() / 1e3}s")
    println(s"Elapsed Time: ${(System.currentTimeMillis() - start) / 1e3}s")
    0
  }

  def parsePar(display: Boolean, test: Boolean, onlyANTLR: Boolean, inPath: String): Int = {
    val absolutePath = Paths.get(Option(inPath).getOrElse("")).toAbsolutePath.normalize()
    println("PARALLEL " + absolutePath.toString + " JVM")

    val start = System.currentTimeMillis()
    val futures: Seq[Future[Int]] = if (Files.isDirectory(absolutePath)) {
      println("Directory")
      val s = Files.walk(absolutePath)
      s.toScala(LazyList)
        .filter(file => !Files.isDirectory(file))
        .filter(file => file.getFileName.toString.toLowerCase.endsWith("cls"))
        .map(
          p =>
            Future {
              parseFileWithStatus(display, test, onlyANTLR, p)
            }
        )
    } else {
      println("Single file")
      Seq(Future {
        parseFileWithStatus(display, test, onlyANTLR, absolutePath)
      })
    }

    val all = Future
      .sequence(futures)
      .andThen(_ => {
        println(s"Number of files: ${futures.length}")
        println(s"Total Length: ${totalLength.longValue()} bytes")
        println(s"Read File Time: ${totalReadFileTime.longValue() / 1e3}s")
        println(s"Convert File Time: ${totalConvertFileTime.longValue() / 1e3}s")
        println(s"Parse Time: ${totalParseTime.longValue() / 1e3}s")
        println(s"Antlr Time: ${totalAntlrTime.longValue() / 1e3}s")
        println(s"Elapsed Time: ${(System.currentTimeMillis() - start) / 1e3}s")
      })

    val result = Await.result(all, 100.second)
    if (result.exists(_ != 0)) 2 else 0
  }

  private val totalLength = new AtomicLong(0)

  def addLength(l: Int): Unit = {
    totalLength.addAndGet(l)
  }

  private val totalParseTime = new AtomicLong(0)

  def addParseTime(t: Long): Unit = {
    totalParseTime.addAndGet(t)
  }

  private val totalAntlrTime = new AtomicLong(0)

  def addAntlrTime(t: Long): Unit = {
    totalAntlrTime.addAndGet(t)
  }

  private val totalReadFileTime = new AtomicLong(0)

  def addReadFileTime(t: Long): Unit = {
    totalReadFileTime.addAndGet(t)
  }

  private val totalConvertFileTime = new AtomicLong(0)

  def addConvertFileTime(t: Long): Unit = {
    totalConvertFileTime.addAndGet(t)
  }

  private def parseFileWithStatus(
    display: Boolean,
    test: Boolean,
    onlyANTLR: Boolean,
    path: Path
  ): Int = {
    try {
      parseFile(display, test, onlyANTLR, path)
      0
    } catch {
      case ex: Throwable =>
        System.err.println(s"Failed parsing: $path")
        ex.printStackTrace()
        2
    }
  }

  private def parseFile(display: Boolean, test: Boolean, onlyANTLR: Boolean, path: Path): Unit = {
    var start         = System.currentTimeMillis()
    val contentsBytes = Files.readAllBytes(path)
    addReadFileTime(System.currentTimeMillis() - start)
    start = System.currentTimeMillis()
    val contentsString: String = new String(contentsBytes, "utf8")
    addConvertFileTime(System.currentTimeMillis() - start)

    val td = if (test || !onlyANTLR) {
      start = System.currentTimeMillis()
      val (success, reason, decl) =
        OutlineParser.parse(path.toString, contentsString, TestClassFactory, null)
      if (!success)
        println(s"outline-parser failed: $path ${reason.get}")
      addParseTime(System.currentTimeMillis() - start)

      if (display) {
        println("=====================")
        println(decl.get)
      }
      decl
    } else None

    val antlrType = if (test || onlyANTLR) {
      try {
        start = System.currentTimeMillis()
        val decl = Antlr.parse(path.toString, contentsBytes)
        addAntlrTime(System.currentTimeMillis() - start)
        decl
      } catch {
        case ex: Exception =>
          println(s"apex-parser failed: $path $ex")
          None
      }
    } else None

    if (test && td.nonEmpty && antlrType.nonEmpty) {
      td.get match {
        case cls: TestClassTypeDeclaration =>
          Compare.compareClassTypeDeclarations(
            cls,
            antlrType.get.asInstanceOf[TestClassTypeDeclaration]
          )
        case int: TestInterfaceTypeDeclaration =>
          Compare.compareInterfaceTypeDeclarations(
            int,
            antlrType.get.asInstanceOf[TestInterfaceTypeDeclaration]
          )
        case enm: TestEnumTypeDeclaration =>
          Compare.compareEnumTypeDeclarations(
            enm,
            antlrType.get.asInstanceOf[TestEnumTypeDeclaration]
          )
        case _ =>
      }
    }
    addLength(contentsBytes.length)
  }
}
