/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.sfdx

import com.nawforce.pkgforce.diagnostics.Duplicates.IterableOps
import com.nawforce.pkgforce.diagnostics.{
  DiagnosticCategory,
  ERROR_CATEGORY,
  IssueExclusion,
  MISSING_CATEGORY,
  SYNTAX_CATEGORY,
  UNUSED_CATEGORY,
  WARNING_CATEGORY
}
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLike
import ujson.Value

import scala.util.{Failure, Success, Try}

case class ApexConfig(
  private val projectPath: PathLike,
  private val config: ValueWithPositions,
  private val configSource: Map[String, Value.Value]
) {
  // For test compatibility - package private access to raw plugins map
  private[sfdx] val plugins: Map[String, Value.Value] = configSource

  val dependencies: Seq[PackageDependent]       = computeDependencies()
  val unpackagedMetadata: Seq[PackageDirectory] = computeUnpackagedMetadata()
  val additionalNamespaces: Array[Option[Name]] = computeAdditionalNamespaces()
  val maxDependencyCount: Option[Int]           = computeMaxDependencyCount()
  val dependencyCountAliases: Map[String, Int]  = computeDependencyCountAliases()
  val isLibrary: Boolean                        = computeIsLibrary()
  val externalMetadata: Seq[String]             = computeExternalMetadata()
  val options: Map[String, String]              = computeOptions()
  val exclusions: Seq[IssueExclusion]           = computeExclusions()

  private def computeExclusions(): Seq[IssueExclusion] = {
    configSource.getOrElse("exclude", ujson.Arr()) match {
      case value: ujson.Arr => value.value.toSeq.zipWithIndex.map(parseExclusion)
      case value            => throwConfigError(value, "'exclude' should be an array")
    }
  }

  private def parseExclusion(entry: (ujson.Value, Int)): IssueExclusion = {
    val (value, index) = entry
    value match {
      case obj: ujson.Obj =>
        val errorPath = s"exclude[$index]"
        val supported = Set("path", "severity", "id")
        obj.value.keys.find(key => !supported.contains(key)).foreach { unknown =>
          throwConfigError(obj(unknown), s"'$errorPath.$unknown' is not supported")
        }
        if (obj.value.isEmpty)
          throwConfigError(value, s"'$errorPath' should contain path, severity, or id")

        val path = obj.value.get("path").map(parseString(_, s"$errorPath.path"))
        val severity = obj.value
          .get("severity")
          .map(parseSeverity(_, s"$errorPath.severity"))
        val id = obj.value.get("id").map(parseNonEmptyString(_, s"$errorPath.id"))

        val pathMatcher = path.map { pattern =>
          val rules = IgnoreRuleV2.parseRules(pattern)
          if (rules.length != 1)
            throwConfigError(obj("path"), s"'$errorPath.path' should be one forceignore pattern")
          val matcher = new ForceIgnoreV2(projectPath, rules)
          (path: PathLike) => matcher.synchronized(!matcher.includeFile(path))
        }
        IssueExclusion(pathMatcher, severity, id)
      case _ => throwConfigError(value, s"'exclude[$index]' should be an object")
    }
  }

  private def parseSeverity(value: ujson.Value, errorPath: String): DiagnosticCategory = {
    val severity = parseString(value, errorPath)
    Map(
      SYNTAX_CATEGORY.name  -> SYNTAX_CATEGORY,
      ERROR_CATEGORY.name   -> ERROR_CATEGORY,
      MISSING_CATEGORY.name -> MISSING_CATEGORY,
      WARNING_CATEGORY.name -> WARNING_CATEGORY,
      UNUSED_CATEGORY.name  -> UNUSED_CATEGORY
    ).getOrElse(
      severity,
      throwConfigError(
        value,
        s"'$errorPath' should be one of Syntax, Error, Missing, Warning, or Unused"
      )
    )
  }

  private def parseNonEmptyString(value: ujson.Value, errorPath: String): String = {
    val result = parseString(value, errorPath)
    if (result.isEmpty)
      throwConfigError(value, s"'$errorPath' should be a non-empty string")
    result
  }

  private def parseString(value: ujson.Value, errorPath: String): String = value match {
    case ujson.Str(result) => result
    case _                 => throwConfigError(value, s"'$errorPath' should be a string")
  }

  private def throwConfigError(value: ujson.Value, message: String): Nothing = {
    val lineAndOffset = config.lineAndOffsetOf(value).getOrElse((0, 0))
    throw SFDXProjectError(lineAndOffset, message)
  }

  private def computeDependencies(): Seq[PackageDependent] = {
    configSource.getOrElse("dependencies", ujson.Arr()) match {
      case value: ujson.Arr =>
        value.value.toSeq.zipWithIndex.map(dp => PackageDependent(projectPath, config, dp._1))
      case value =>
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset => {
            throw SFDXProjectError(lineAndOffset, "'dependencies' should be an array")
          })
          .getOrElse(Seq.empty)
    }
  }

  private def computeUnpackagedMetadata(): Seq[PackageDirectory] = {
    configSource.getOrElse("unpackagedMetadata", ujson.Arr()) match {
      case value: ujson.Arr =>
        value.value.toSeq.map(value =>
          PackageDirectory.fromUnpackagedMetadata(projectPath, config, value)
        )
      case value =>
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset => {
            throw SFDXProjectError(lineAndOffset, "'unpackagedMetadata' should be an array")
          })
          .getOrElse(Seq.empty)
    }
  }

  private def computeAdditionalNamespaces(): Array[Option[Name]] = {
    val rawValue            = configSource.getOrElse("additionalNamespaces", ujson.Arr())
    val namespacesArray     = parseNamespaceArray(rawValue)
    val validatedStrings    = validateNamespaceEntries(namespacesArray)
    val convertedNamespaces = convertToNamespaces(validatedStrings)
    validateNoDuplicates(convertedNamespaces, rawValue)
    convertedNamespaces.toArray
  }

  private def parseNamespaceArray(value: ujson.Value): ujson.Arr = {
    value match {
      case arr: ujson.Arr => arr
      case _ =>
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset => {
            throw SFDXProjectError(lineAndOffset, "'additionalNamespaces' should be an array")
          })
          .getOrElse(throw new RuntimeException("'additionalNamespaces' should be an array"))
    }
  }

  private def validateNamespaceEntries(arr: ujson.Arr): Seq[String] = {
    arr.value.toSeq.flatMap {
      case ujson.Str(value) => Some(value)
      case invalidValue =>
        config
          .lineAndOffsetOf(invalidValue)
          .map(lineAndOffset =>
            throw SFDXProjectError(
              lineAndOffset,
              "'additionalNamespaces' entries should all be strings"
            )
          )
        None
    }
  }

  private def convertToNamespaces(strings: Seq[String]): Seq[Option[Name]] = {
    strings.map {
      case "unmanaged" => None
      case ns          => Some(Name(ns))
    }
  }

  private def validateNoDuplicates(
    namespaces: Seq[Option[Name]],
    originalValue: ujson.Value
  ): Unit = {
    val dups = namespaces.duplicates(_.getOrElse("unmanaged"))
    if (dups.nonEmpty) {
      config
        .lineAndOffsetOf(originalValue)
        .map(lineAndOffset =>
          throw SFDXProjectError(
            lineAndOffset,
            s"namespace '${dups.head._1.getOrElse("unmanaged")}' is duplicated in additionalNamespaces'"
          )
        )
    }
  }

  private def computeMaxDependencyCount(): Option[Int] = {
    configSource.get("maxDependencyCount") match {
      case None        => None
      case Some(value) => parseAndValidateCount(value)
    }
  }

  private def parseAndValidateCount(value: ujson.Value): Option[Int] = {
    value match {
      case num: ujson.Num if num.toString.matches("[0-9]+") =>
        Try(num.toString().toInt) match {
          case Success(parsedValue) => Some(parsedValue)
          case Failure(_) =>
            throwCountError(
              value,
              s"'maxDependencyCount' value '${num.toString}' is not an integer"
            )
        }
      case _ =>
        throwCountError(
          value,
          s"'maxDependencyCount' value '${value.toString}' should be a positive integer"
        )
    }
  }

  private def throwCountError(value: ujson.Value, message: String): Option[Int] = {
    config
      .lineAndOffsetOf(value)
      .map(lineAndOffset => {
        throw SFDXProjectError(lineAndOffset, message)
      })
      .getOrElse(None)
  }

  private def computeDependencyCountAliases(): Map[String, Int] = {
    configSource.get("dependencyCountAliases") match {
      case None => Map.empty
      case Some(value: ujson.Obj) =>
        flattenDependencyCountAliases("dependencyCountAliases", "", value)
      case Some(value) =>
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset => {
            throw SFDXProjectError(lineAndOffset, "'dependencyCountAliases' should be an object")
          })
          .getOrElse(Map.empty)
    }
  }

  private def flattenDependencyCountAliases(
    errorPath: String,
    keyPrefix: String,
    obj: ujson.Obj
  ): Map[String, Int] = {
    obj.value.toSeq.flatMap { case (key, value) =>
      val fullKey       = if (keyPrefix.isEmpty) key else s"$keyPrefix.$key"
      val fullErrorPath = s"$errorPath.$key"
      value match {
        case nested: ujson.Obj =>
          flattenDependencyCountAliases(fullErrorPath, fullKey, nested).toSeq
        case _ =>
          parseAliasCount(fullErrorPath, value).map(fullKey -> _).toSeq
      }
    }.toMap
  }

  private def parseAliasCount(errorPath: String, value: ujson.Value): Option[Int] = {
    value match {
      case num: ujson.Num if num.toString.matches("[0-9]+") =>
        Try(num.toString().toInt) match {
          case Success(parsedValue) => Some(parsedValue)
          case Failure(_) =>
            throwCountError(value, s"'$errorPath' value '${num.toString}' is not an integer")
        }
      case _ =>
        throwCountError(
          value,
          s"'$errorPath' value '${value.toString}' should be a positive integer or nested object"
        )
    }
  }

  private def computeIsLibrary(): Boolean = {
    configSource.get("library") match {
      case Some(ujson.Bool(value)) => value
      case Some(value) =>
        config
          .lineAndOffsetOf(value)
          .foreach(lineAndOffset =>
            throw SFDXProjectError(lineAndOffset, "'library' should be a boolean")
          )
        false
      case None => false
    }
  }

  private def computeExternalMetadata(): Seq[String] = {
    configSource.getOrElse("externalMetadata", ujson.Arr()) match {
      case value: ujson.Arr =>
        value.value.toSeq.flatMap(value => {
          value match {
            case ujson.Str(dir) =>
              validateExternalMetadataPath(dir) match {
                case Some(error) =>
                  config
                    .lineAndOffsetOf(value)
                    .map(lineAndOffset => throw SFDXProjectError(lineAndOffset, error))
                  None
                case None => Some(dir)
              }
            case _ =>
              config
                .lineAndOffsetOf(value)
                .map(lineAndOffset =>
                  throw SFDXProjectError(
                    lineAndOffset,
                    "'externalMetadata' entries should be strings"
                  )
                )
              None
          }
        })
      case value =>
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset =>
            throw SFDXProjectError(lineAndOffset, "'externalMetadata' should be an array")
          )
          .getOrElse(Seq.empty)
    }
  }

  private def validateExternalMetadataPath(dir: String): Option[String] = {
    if (dir.startsWith("/") || dir.startsWith("\\") || dir.contains("..")) {
      Some(s"External metadata path '$dir' must be a relative path within the project")
    } else {
      None
    }
  }

  private def computeOptions(): Map[String, String] = {
    val rawValue = configSource.getOrElse("options", ujson.Obj())
    parseOptionsObject(rawValue) match {
      case Right(options) => options
      case Left(error)    => throw error
    }
  }

  private def parseOptionsObject(
    value: ujson.Value
  ): Either[SFDXProjectError, Map[String, String]] = {
    value match {
      case obj: ujson.Obj =>
        try {
          val optionsMap = scala.collection.mutable.Map[String, String]()
          obj.value.foreach {
            case (key, ujson.Str(strValue)) =>
              optionsMap.put(key, strValue)
            case (key, nonStringValue) =>
              config
                .lineAndOffsetOf(nonStringValue)
                .map(lineAndOffset => {
                  throw SFDXProjectError(lineAndOffset, s"'options.$key' should be a string value")
                })
                .getOrElse(throw new RuntimeException(s"'options.$key' should be a string value"))
          }
          Right(optionsMap.toMap)
        } catch {
          case e: SFDXProjectError => Left(e)
        }
      case _ =>
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset => {
            Left(SFDXProjectError(lineAndOffset, "'options' should be an object"))
          })
          .getOrElse(Left(new SFDXProjectError(0, 0, "'options' should be an object")))
    }
  }
}

object ApexConfig {
  def fromConfig(projectPath: PathLike, config: ValueWithPositions): ApexConfig = {
    val pluginsRoot = extractPluginsRoot(config)

    // Check if apex-ls key exists within plugins
    val configSource = pluginsRoot.get("apex-ls") match {
      case Some(apexLsValue: ujson.Obj) =>
        // Use apex-ls configuration
        apexLsValue.value.toMap
      case Some(value: Value.Value) =>
        // apex-ls exists but is not an object
        config
          .lineAndOffsetOf(value)
          .map(lineAndOffset => {
            throw SFDXProjectError(lineAndOffset, "'plugins.apex-ls' should be an object")
          })
          .getOrElse(Map.empty[String, Value.Value])
      case None =>
        // Fall back to legacy plugins configuration
        pluginsRoot
    }

    ApexConfig(projectPath, config, configSource)
  }

  private def extractPluginsRoot(config: ValueWithPositions): Map[String, Value.Value] = {
    try {
      config.root("plugins") match {
        case value: ujson.Obj => value.value.toMap
        case value =>
          config
            .lineAndOffsetOf(value)
            .map(lineAndOffset => {
              throw SFDXProjectError(lineAndOffset, "'plugins' should be an object")
            })
            .getOrElse(Map.empty[String, Value.Value])
      }
    } catch {
      case _: NoSuchElementException => Map.empty[String, Value.Value]
    }
  }
}
