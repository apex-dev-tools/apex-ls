/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved
 */

package com.nawforce.apexlink.deps

import com.nawforce.apexparser.ApexLexer
import com.nawforce.runtime.parsers.CodeParser
import com.nawforce.runtime.platform.Path
import org.antlr.v4.runtime.{CommonTokenStream, Token}

import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.{Failure, Success, Try}

object MaxDependencyCountParser {
  private final val maxCountMarker       = "MaxDependencyCount("
  private final val maxCountMarkerLength = maxCountMarker.length

  def parseMaxDependencyCount(path: String, default: Option[Int]): Either[Option[String], Int] = {
    val dependencyLimitParseExceptions = mutable.Queue[String]()

    def parseTokenToDependencyLimit(t: Token): Option[Int] = {
      getDependencyLimit(t) match {
        case Right(result) => Some(result)
        case Left(value) =>
          if (value.nonEmpty)
            dependencyLimitParseExceptions.enqueue(value.get)
          None
      }
    }

    val sourcePath = Path.safeApply(path)
    sourcePath.readSourceData() match {
      case Right(source) =>
        if (source.asString.indexOf(maxCountMarker) == -1)
          return if (default.isEmpty) Left(None) else Right(default.get)
        val parser      = CodeParser(sourcePath, source)
        val tokenStream = new CommonTokenStream(new ApexLexer(parser.cis))
        tokenStream.fill()

        val tokensR = tokenStream.getHiddenTokensToRight(0).asScala
        val countsR = tokensR.flatMap(t => parseTokenToDependencyLimit(t))
        val tokensL = tokenStream.getHiddenTokensToLeft(1).asScala
        val countsL = tokensL.flatMap(t => parseTokenToDependencyLimit(t))

        val counts = countsL ++ countsR
        if (counts.isEmpty && dependencyLimitParseExceptions.isEmpty) Left(None)
        else if (dependencyLimitParseExceptions.nonEmpty)
          Left(Some(dependencyLimitParseExceptions.last))
        else Right(counts.max)
      case Left(err) => Left(Some(err))
    }
  }

  private def getDependencyLimit(token: Token): Either[Option[String], Int] = {
    val tokenText = token.getText.filterNot((x: Char) => x.isWhitespace)
    val start     = tokenText.indexOf(maxCountMarker)
    if (start == -1) return Left(None)
    val end = tokenText.indexOf(")", start + maxCountMarkerLength)
    if (end == -1) return Left(None)
    val text = tokenText.substring(start + maxCountMarkerLength, end)

    Try(text.toInt) match {
      case Success(value) if value >= 0 => Right(value)
      case Success(_)                   => Left(Some(s"'$text' must be >=0"))
      case Failure(_)                   => Left(Some(s"'$text' is not an integer value"))
    }
  }
}
