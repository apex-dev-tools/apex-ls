package com.nawforce.pkgforce

import com.nawforce.runtime.platform.{Environment, Path}

import java.nio.file.Paths

object PathInterpolator {

  private val currentDrive =
    if (Environment.isWindows) Path("").toString.substring(0, 1) + ":\\" else ""

  implicit class PathInterpolator(val sc: StringContext) extends AnyVal {

    /* String interpolator for handling UNIX style -> C:\ Windows style path conversions */
    def path(args: Any*): String = {
      transformPaths("C:\\", sc.parts.iterator, args.iterator)
    }

    /* String interpolator for handling UNIX style -> current drive Windows style path conversions */
    def cpath(args: Any*): String = {
      transformPaths(currentDrive, sc.parts.iterator, args.iterator)
    }

  }

  def transformPaths(
    drive: String,
    strings: Iterator[String],
    expressions: Iterator[Any]
  ): String = {
    val sb = new StringBuilder(transform(drive, strings.next().trim()))
    while (strings.hasNext) {
      sb.append(expressions.next().toString)
      sb.append(transform(drive, strings.next()))
    }
    sb.toString()
  }

  private def transform(drive: String, path: String): String = {
    if (Environment.isWindows) {
      val paths = "/[^\\s']*".r
      paths.replaceAllIn(
        path,
        m => {
          drive + m.group(0).replace("/", "\\\\")
        }
      )
    } else {
      path
    }

  }
}
