package com.nawforce.pkgforce

import com.nawforce.runtime.platform.Environment

object PathInterpolator {

  implicit class PathInterpolator(val sc: StringContext) extends AnyVal {
    def path(args: Any*): String = {
      val strings: Iterator[String]  = sc.parts.iterator
      val expressions: Iterator[Any] = args.iterator

      val sb = new StringBuilder(transform(strings.next().trim()))
      while (strings.hasNext) {
        sb.append(expressions.next().toString)
        sb.append(transform(strings.next()))
      }
      sb.toString()
    }
  }

  private def transform(path: String): String = {
    if (Environment.isWindows) {
      val paths = "/[^\\s']*".r
      paths.replaceAllIn(
        path,
        m => {
          "C:\\" + m.group(0).replace("/", "\\\\")
        }
      )
    } else {
      path
    }

  }
}
