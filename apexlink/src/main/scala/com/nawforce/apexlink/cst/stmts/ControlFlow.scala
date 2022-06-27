/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst.stmts

import com.nawforce.apexlink.cst.{BlockVerifyContext, CST, OuterBlockVerifyContext}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.pkgforce.diagnostics.{ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.path.PathLocation

import scala.collection.mutable

trait ControlFlowContext {
  this: BlockVerifyContext =>

  private lazy val paths               = mutable.ArrayBuffer[ControlPath]()
  private var root: ControlFlowContext = this
  private var branching: Boolean       = false

  def parentFlowContext: Option[BlockVerifyContext] =
    parent().flatMap {
      case a: BlockVerifyContext with ControlFlowContext => Some(a)
      case _                                             => None
    }

  def getPaths: Array[ControlPath] = root.paths.toArray
  def addPath(path: ControlPath): ControlPath = {
    root.paths.addOne(path)
    path
  }

  def getControlRoot(): ControlFlowContext = root
  def setControlRoot(context: BlockVerifyContext with ControlFlowContext): Unit = {
    root = context
  }

  //TODO find alternative
  def hasBranchingControl(): Boolean = root.branching
  def withBranchingControl(): BlockVerifyContext = {
    root.branching = true
    this
  }

  def logUnreachableIssues(): Unit = {
    // localised to a single block
    getPaths
      .filter(_.unreachable)
      .flatMap(_.location)
      .foreach(
        pl => log(Issue(pl.path, ERROR_CATEGORY, pl.location, s"Unreachable block or statement"))
      )
  }
}

trait OuterControlFlowContext {
  this: OuterBlockVerifyContext =>

  def logControlFlowIssues(): Unit = {
    // Failed path refs climb to top level
    getPaths
      .collectFirst {
        case method: BlockPath if !method.returns && returnType != TypeNames.Void =>
          method
      }
      .map {
        case outer if outer.failedPaths.isEmpty => Array[ControlPath](outer)
        case inner                              => inner.failedPaths
      }
      .foreach(_.flatMap(_.location).foreach(logPathIssue))
  }

  private def logPathIssue(pl: PathLocation): Unit = {
    log(Issue(pl.path, ERROR_CATEGORY, pl.location, s"Code path does not return a value"))
  }
}

trait ControlFlow {
  this: CST =>

  def verifyControlPath(
    context: BlockVerifyContext,
    controlPattern: ControlPattern = NoControlPattern
  ): Unit = {
    val path = controlPattern.addControlPath(context, this)

    path match {
      // if this block itself is unreachable, issue logging is deferred to parent block
      case BlockPath(_, unreachable, _, _) if !unreachable => context.logUnreachableIssues()
      case _                                               =>
    }
  }

}
