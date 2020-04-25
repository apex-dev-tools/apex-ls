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
package com.nawforce.common.sfdx

import com.nawforce.common.documents.{ForceIgnore, LineLocationImpl}
import com.nawforce.common.names.Name
import com.nawforce.common.org.OrgImpl
import com.nawforce.common.path.PathLike

trait Workspace {
  val rootPaths: Seq[PathLike]
  val namespace: Either[String, Option[Name]]
  val paths: Seq[PathLike]
  val ignorePath: Option[PathLike] = None

  lazy val forceIgnore: Option[ForceIgnore] = {
    if (ignorePath.nonEmpty && ignorePath.get.isFile) {
      ForceIgnore(ignorePath.get) match {
        case Left(err) =>
          OrgImpl.logError(LineLocationImpl(ignorePath.get.toString, 0), s"Could not read .forceignore, error: $err")
          None
        case Right(forceIgnore) =>
          Some(forceIgnore)
      }
    } else {
      None
    }
  }

  /** Determine if a path is a file that could be included in index. */
  def isVisibleFile(path: PathLike): Boolean = {
    forceIgnore.forall(_.includeFile(path)) && isVisiblePath(path.parent)
  }

  // Check a directory path would be included in index
  @scala.annotation.tailrec
  private def isVisiblePath(path: PathLike): Boolean = {
    if (paths.contains(path)) return true
    if (!forceIgnore.forall(_.includeDirectory(path))) return false

    val parent = path.parent
    if (parent != path)
      isVisiblePath(parent)
    else
      false
  }
}

class MDAPIWorkspace(_namespace: Option[Name], val paths: Seq[PathLike]) extends Workspace {
  override lazy val rootPaths: Seq[PathLike] = paths

  override val namespace: Either[String, Option[Name]] = Right(_namespace)

  override def toString: String = {
    s"MDAPIWorkspace(namespace=${namespace.getOrElse("")}, paths=${paths.map(_.toString).mkString(", ")})"
  }
}

class SFDXWorkspace(_namespace: Option[Name], val rootPath: PathLike, project: Project) extends Workspace {
  override val namespace: Either[String, Option[Name]] = {
    _namespace.map(ns => Right(Some(ns))).getOrElse(project.namespace)
  }

  override lazy val rootPaths: Seq[PathLike] = Seq(rootPath)

  override lazy val paths: Seq[PathLike] = {
    val errors = project.paths.filter(_.isLeft)
    if (errors.nonEmpty)
      OrgImpl.logError(LineLocationImpl(rootPath.join("sfdx-project.json").toString,0), errors.head.left.get)
    project.paths.filter(_.isRight).map(_.right.get).map(p => rootPath.join(p))
  }

  override val ignorePath: Option[PathLike] = Some(rootPath.join(".forceignore"))

  override def toString: String = {
    s"SFDXWorkspace(namespace=${namespace.getOrElse("")}, paths=${paths.map(_.toString).mkString(", ")})"
  }
}

object Workspace {
  def apply(namespace: Option[Name], paths: Seq[PathLike]): Either[String, Workspace] = {
    val missing = paths.filterNot(_.isDirectory)
    if (missing.nonEmpty)
      return Left(s"Workspace '${missing.head}' is not a directory")

    if (paths.size == 1) {
      Project(paths.head) match {
        case Left(err) => Left(err)
        case Right(project) =>
          if (project.nonEmpty)
            Right(new SFDXWorkspace(namespace, paths.head, project.get))
          else
            Right(new MDAPIWorkspace(namespace, paths))
      }
    } else {
      Right(new MDAPIWorkspace(namespace, paths))
    }
  }
}
