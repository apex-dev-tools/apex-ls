/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.path

import java.nio.charset.StandardCharsets
import java.nio.file.Files

case class Path(nativePath: java.nio.file.Path) extends PathLike {

  override lazy val basename: String = Option(nativePath.getFileName).map(_.toString).getOrElse("")
  override lazy val parent: Path = join("..")
  override lazy val absolute: Path = Path(nativePath.toAbsolutePath)

  override def toString: String = nativePath.toString

  override lazy val nature: PathNature = {
    if (!Files.exists(nativePath)) DOES_NOT_EXIST
    else if (Files.isDirectory(nativePath)) DIRECTORY
    else if (Files.isRegularFile(nativePath)) {
      if (Files.size(nativePath) == 0) EMPTY_FILE else NONEMPTY_FILE
    }
    else UNKNOWN
  }

  override def join(arg: String): Path = {
    Path(nativePath.resolve(arg).normalize())
  }

  override def createFile(name: String, data: String): Either[String, Path] = {
    val created = join(name)
    created.write(data) match {
      case None => Right(created)
      case Some(err) => Left(err)
    }
  }

  override def read(): Either[String, String] = {
    try {
      Right(new String(Files.readAllBytes(nativePath), StandardCharsets.UTF_8))
    } catch {
      case ex: java.io.IOException => Left(ex.toString)
    }
  }

  override def write(data: String): Option[String] = {
    try {
      Files.write(nativePath, data.getBytes(StandardCharsets.UTF_8))
      None
    } catch {
      case ex: java.io.IOException => Some(ex.toString)
    }
  }

  override def delete(): Option[String] = {
    try {
      Files.delete(nativePath)
      None
    } catch {
      case ex: java.io.IOException => Some(ex.toString)
    }
  }

  override def createDirectory(name: String): Either[String, PathLike] = {
    val dir = join(name)
    if (dir.nature == DOES_NOT_EXIST) {
      try {
        Files.createDirectory(dir.nativePath)
        Right(Path(dir.nativePath))
      } catch {
        case ex: java.io.IOException => Left(ex.toString)
      }
    } else if (dir.nature == DIRECTORY) {
      Right(dir)
    } else {
      Left(s"Can not create directory '$dir', file already exists")
    }
  }

  override def directoryList(): Either[String, Seq[String]] = {
    if (nature == DIRECTORY) {
      val f = nativePath.toFile
      Right(Option(f.listFiles()).getOrElse(Array()).map(_.getName))
    } else {
      Left(s"Path '$nativePath' is not a directory'")
    }
  }
}

object Path {
  def apply(path: String): Path = Path(java.nio.file.Paths.get(path))
}
