/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst.stmts

import com.nawforce.apexlink.cst.{ScopeVerifyContext, CST, ExprContext}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.pkgforce.path.PathLocation

trait ControlPath {
  val returns: Boolean
  val unreachable: Boolean
  val location: Option[PathLocation]
}

sealed case class UnknownPath(unreachable: Boolean, location: Option[PathLocation])
    extends ControlPath {
  override val returns = false
}
sealed case class StatementPath(
  returns: Boolean,
  unreachable: Boolean,
  location: Option[PathLocation],
  exitsBlock: Boolean
) extends ControlPath
sealed case class BlockPath(
  returns: Boolean,
  unreachable: Boolean,
  location: Option[PathLocation],
  failedPaths: Array[ControlPath],
  enters: Boolean
) extends ControlPath

sealed trait ControlPattern {

  def addControlPath(context: ScopeVerifyContext, cst: CST): ControlPath

  protected def nextPathUnreachable(context: ScopeVerifyContext): Boolean = {
    val paths   = context.getPaths
    val nonVoid = context.returnType != TypeNames.Void
    !context.hasBranchingControl && (paths.lastOption.exists(_.unreachable) || paths.exists {
      // some block returns depend on entry to the block
      // we cannot be certain in all cases
      case s: StatementPath => s.returns || s.exitsBlock
      case b: BlockPath     => nonVoid && b.enters && b.returns
      case _                => false // unknown stmts/blocks
    })
  }

}

// default
object NoControlPattern extends ControlPattern {
  def addControlPath(context: ScopeVerifyContext, cst: CST): ControlPath = {
    context.addPath(UnknownPath(nextPathUnreachable(context), cst.safeLocation))
  }
}

// continue, break / return, throw
class ExitControlPattern(method: Boolean, block: Boolean) extends ControlPattern {
  def addControlPath(context: ScopeVerifyContext, cst: CST): ControlPath = {
    context.addPath(StatementPath(method, nextPathUnreachable(context), cst.safeLocation, block))
  }
}

object ExitControlPattern {
  def apply(exitsMethod: Boolean, exitsBlock: Boolean): ExitControlPattern = {
    new ExitControlPattern(exitsMethod, exitsBlock)
  }
}

// default blocks (methods etc.)
class BlockControlPattern extends ControlPattern {
  override def addControlPath(context: ScopeVerifyContext, cst: CST): ControlPath = {
    val paths            = context.getPaths
    val parent           = context.parentFlowContext
    val blockUnreachable = parent.exists(p => nextPathUnreachable(p))

    val path = if (paths.length == 0 || blockUnreachable) {
      BlockPath(returns = false, blockUnreachable, cst.safeLocation, Array(), enters = false)
    } else {
      val failedPaths = Option
        .unless(context.returnType == TypeNames.Void) { collectFailedPaths(context, paths) }
        .getOrElse(Array())

      BlockPath(
        willBlockReturn(paths, failedPaths),
        blockUnreachable,
        cst.safeLocation,
        failedPaths,
        willEnterBlock(context)
      )
    }

    parent.foreach(_.addPath(path))
    path
  }

  protected def collectFailedPaths(
    context: ScopeVerifyContext,
    paths: Array[ControlPath]
  ): Array[ControlPath] = {
    val path = paths
      .filterNot(_.unreachable)
      .last

    if (path.returns) {
      // block is valid
      Array[ControlPath]()
    } else {
      handlePathFailure(path)
    }
  }

  protected def willEnterBlock(context: ScopeVerifyContext): Boolean = false

  protected def willBlockReturn(
    paths: Array[ControlPath],
    failedPaths: Array[ControlPath]
  ): Boolean =
    failedPaths.isEmpty

  protected def handlePathFailure(path: ControlPath): Array[ControlPath] = {
    // failed inner paths take precedence
    path match {
      case BlockPath(_, _, _, failed, _) if failed.length > 0 => failed
      case _                                                  => Array(path)
    }
  }
}

object BlockControlPattern {
  def apply(): BlockControlPattern = {
    new BlockControlPattern()
  }
}

// if/else, try/catch
class BranchControlPattern(requiredBranches: Array[Boolean], exclusive: Boolean)
    extends BlockControlPattern {
  override protected def collectFailedPaths(
    context: ScopeVerifyContext,
    paths: Array[ControlPath]
  ): Array[ControlPath] = {
    if (paths.length != requiredBranches.length) {
      // missing branches
      Array()
    } else {
      paths
        .zip(requiredBranches)
        .collect {
          case (path, required) if required && !path.returns => path
        }
        .flatMap(handlePathFailure)
    }
  }

  override protected def willEnterBlock(context: ScopeVerifyContext): Boolean = exclusive

  override protected def willBlockReturn(
    paths: Array[ControlPath],
    failedPaths: Array[ControlPath]
  ): Boolean = {
    // make sure empty failed paths is genuine
    super.willBlockReturn(paths, failedPaths) && paths.length == requiredBranches.length
  }
}

object BranchControlPattern {
  def apply(requiredBranches: Array[Boolean]): BranchControlPattern = {
    new BranchControlPattern(requiredBranches, false)
  }
  def apply(expr: Option[ExprContext], requiredBranches: Int): BranchControlPattern = {
    new BranchControlPattern(Array.fill(requiredBranches)(true), true)
  }
}
