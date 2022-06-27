/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.apexlink.cst.stmts

import com.nawforce.apexlink.cst.{BlockVerifyContext, CST, ExprContext}
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
  failedPaths: Array[ControlPath]
) extends ControlPath

sealed trait ControlPattern {

  def addControlPath(context: BlockVerifyContext, cst: CST): ControlPath

  protected def nextPathUnreachable(context: BlockVerifyContext): Boolean = {
    val paths = context.getPaths
    !context.hasBranchingControl() && (paths.lastOption.exists(_.unreachable) || paths.exists {
      case s: StatementPath => s.returns || s.exitsBlock
      //TODO if block with branching that returns
      case _ => false // limitation: block returns depend on entry to the block
    })
  }

}

// default
object NoControlPattern extends ControlPattern {
  def addControlPath(context: BlockVerifyContext, cst: CST): ControlPath = {
    context.addPath(UnknownPath(nextPathUnreachable(context), cst.safeLocation))
  }
}

// continue, break / return, throw
class ExitControlPattern(method: Boolean, block: Boolean) extends ControlPattern {
  def addControlPath(context: BlockVerifyContext, cst: CST): ControlPath = {
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
  override def addControlPath(context: BlockVerifyContext, cst: CST): ControlPath = {
    val paths            = context.getPaths
    val parent           = context.parentFlowContext
    val blockUnreachable = parent.exists(p => nextPathUnreachable(p))

    val path = if (paths.length == 0 || blockUnreachable) {
      BlockPath(returns = false, blockUnreachable, cst.safeLocation, Array())
    } else {
      val failedPaths = Option
        .unless(context.returnType == TypeNames.Void) { collectFailedPaths(context, paths) }
        .getOrElse(Array())

      BlockPath(blockReturns(paths, failedPaths), blockUnreachable, cst.safeLocation, failedPaths)
    }

    parent.foreach(_.addPath(path))
    path
  }

  def collectFailedPaths(
    context: BlockVerifyContext,
    paths: Array[ControlPath]
  ): Array[ControlPath] = {
    if (context.returnType == TypeNames.Void) {
      Array()
    } else {
      val path = paths
        .filterNot(_.unreachable)
        .last

      if (path.returns) {
        // block is valid
        Array[ControlPath]()
      } else {
        // failed inner paths take precedence
        path match {
          case BlockPath(_, _, _, failed) if failed.length > 0 => failed
          case _                                               => Array(path)
        }
        //TODO
        // change failedPaths to location + errorType
        // if statement "Expected return"
        // if block with failed paths "Path does not return"
        // if block without failed paths "Missing path with return" or something

      }
    }
  }

  def blockReturns(paths: Array[ControlPath], failedPaths: Array[ControlPath]): Boolean =
    failedPaths.isEmpty
}

object BlockControlPattern {
  def apply(): BlockControlPattern = {
    new BlockControlPattern()
  }
}

// if/else, try/catch
class BranchControlPattern(expr: Option[ExprContext], requiredBranches: Array[Boolean])
    extends BlockControlPattern {
  override def collectFailedPaths(
    context: BlockVerifyContext,
    paths: Array[ControlPath]
  ): Array[ControlPath] = {
    if (paths.length != requiredBranches.length) {
      // missing branches
      Array()
    } else {
      paths
        .zip(requiredBranches)
        .filter {
          case (path, required) => required && !path.returns
        }
        .flatMap {
          case (BlockPath(_, _, _, failed), _) if failed.length > 0 => failed
          case (p, _)                                               => Array(p)
        }
    }
  }

  override def blockReturns(paths: Array[ControlPath], failedPaths: Array[ControlPath]): Boolean = {
    // make sure empty failed paths is genuine
    super.blockReturns(paths, failedPaths) && paths.length == requiredBranches.length

    //TODO check edge case
    // if expression always true
    // doesn't matter if other paths are missing / don't return
  }
}

object BranchControlPattern {
  def apply(expr: Option[ExprContext], requiredBranches: Array[Boolean]): BranchControlPattern = {
    new BranchControlPattern(expr, requiredBranches)
  }
}

class LoopControlPattern(expr: Option[ExprContext]) extends BlockControlPattern {
  //TODO test expression
  // may not enter loop
  // therefore must assume returns = false
  // if always enters loop - returns can't be false
}

object LoopControlPattern {
  def apply(expr: Option[ExprContext]): LoopControlPattern = {
    new LoopControlPattern(expr)
  }
}
