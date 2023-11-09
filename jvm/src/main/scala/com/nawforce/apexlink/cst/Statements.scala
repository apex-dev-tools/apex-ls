/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.cst

import com.financialforce.types.base.{Location => OPLocation}
import com.nawforce.apexlink.cst.AssignableSupport.isAssignableDeclaration
import com.nawforce.apexlink.cst.stmts._
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.org.OrgInfo
import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.apexparser.ApexParser._
import com.nawforce.pkgforce.diagnostics.{ERROR_CATEGORY, Issue, LoggerOps}
import com.nawforce.pkgforce.modifiers.{ApexModifiers, FINAL_MODIFIER, ModifierResults}
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.runtime.parsers.{CodeParser, Source}

import java.lang.ref.WeakReference
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/** Base class for all types of Statement */
abstract class Statement extends CST with ControlFlow {
  def verify(context: BlockVerifyContext): Unit
}

/** Block of statements, nesting uses a Block as a Statement.
  *
  * There are two types of Block, an Outer which can use lazy loading and an Inner which does not. The OuterBlock
  * helps significantly reduce memory needs at the cost of needing to re-parse the block contents. As re-parsing
  * will parse all nested blocks we use an InnerBlock for these just to reduce the number of WeakReferences we
  * need to use.
  */
abstract class Block extends Statement {
  def statements(): Seq[Statement]
}

object Block {
  val empty: Block = InnerBlock(Seq())

  /** Create an OuterBlock from ANTLR parser output.
    *
    * @param parser the parser used
    * @param blockContext the ANTLR block context
    * @param isTrigger set if outer block of a trigger
    */
  def constructOuterFromANTLR(
    parser: CodeParser,
    blockContext: BlockContext,
    isTrigger: Boolean = false
  ): Block = {
    OuterBlock(parser.extractSource(blockContext), isTrigger, new WeakReference(blockContext))
      .withContext(blockContext)
  }

  /** Create an OuterBlock from Outline parser output.
    *
    * @param blockSource  location and cached source of the block
    */
  def constructOuterFromOutline(blockSource: Source, location: OPLocation): Block = {
    val block = OuterBlock(blockSource, isTrigger = false, null)
    block.setLocation(
      blockSource.path,
      location.startLine,
      location.startLineOffset,
      location.endLine,
      location.endLineOffset
    )
    block
  }

  /** Create an InnerBlock from ANTLR parser output.
    *
    * @param parser       the parser used
    * @param blockContext the ANTLR block context
    */
  def constructInner(parser: CodeParser, blockContext: BlockContext): Block = {
    InnerBlock(
      Statement.construct(parser, CodeParser.toScala(blockContext.statement()), isTrigger = false)
    ).withContext(blockContext)
  }
}

/** Outer block, holds weak reference to statements, will re-parse as needed
  *
  * @param source location of the block with cached source
  * @param isTrigger is the the outer block of a trigger
  * @param blockContextRef ANTLR BlockContext if one is available
  */
private final case class OuterBlock(
  source: Source,
  isTrigger: Boolean,
  var blockContextRef: WeakReference[BlockContext] = null
) extends Block {
  private var statementsRef: WeakReference[Seq[Statement]] = _
  private var reParsed                                     = false

  override def verify(context: BlockVerifyContext): Unit = {
    val blockContext = new InnerBlockVerifyContext(context)
    statements().foreach(_.verify(blockContext))
    verifyControlPath(blockContext, BlockControlPattern())
    context.typePlugin.foreach(_.onBlockValidated(this, context.isStatic, blockContext))
  }

  override def statements(): Seq[Statement] = {
    var statements = Option(statementsRef).map(_.get).orNull

    // If the statement WeakRef has gone stale we need to re-build them
    if (statements == null) {
      // If the block AST WeakRef has gone stale as well we need to re-parse first
      var statementContext = if (blockContextRef != null) blockContextRef.get else null
      if (statementContext == null) {
        val parser = new CodeParser(source)
        val result = parser.parseBlock()
        result.issues.foreach(OrgInfo.log)
        statementContext = result.value
        blockContextRef = new WeakReference(statementContext)
        reParsed = true
      }

      // Now rebuild, making sure we put correct source in scope for CST to use
      val parsedSource = if (reParsed) source else source.outer.get
      CST.sourceContext.withValue(Some(parsedSource)) {
        withContext(statementContext)
        val parser = new CodeParser(parsedSource)
        statementsRef = createStatements(statementContext, parser, isTrigger)
        statements = statementsRef.get
      }
    }
    statements
  }

  // Construct statements from ANTLR AST
  private def createStatements(
    context: BlockContext,
    parser: CodeParser,
    isTrigger: Boolean
  ): WeakReference[Seq[Statement]] = {
    val statementContexts = CodeParser.toScala(context.statement())
    val statements        = Some(Statement.construct(parser, statementContexts, isTrigger))
    new WeakReference(statements.get)
  }
}

/** Inner block, a container for statements. Used for blocks nested in an OuterBlock as re-parsing can be expensive.
  * @param statements the statements in the block
  */
private final case class InnerBlock(statements: Seq[Statement]) extends Block {
  override def verify(context: BlockVerifyContext): Unit = {
    val blockContext = new InnerBlockVerifyContext(context)
    statements.foreach(_.verify(blockContext))
    verifyControlPath(blockContext, BlockControlPattern())
  }
}

final case class LocalVariableDeclarationStatement(
  localVariableDeclaration: LocalVariableDeclaration
) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    localVariableDeclaration.verify(context)
    verifyControlPath(context)
  }
}

object LocalVariableDeclarationStatement {
  def construct(
    parser: CodeParser,
    from: LocalVariableDeclarationStatementContext,
    isTrigger: Boolean
  ): LocalVariableDeclarationStatement = {
    LocalVariableDeclarationStatement(
      LocalVariableDeclaration.construct(parser, from.localVariableDeclaration(), isTrigger)
    ).withContext(from)
  }
}

final case class IfStatement(expression: Expression, statements: Seq[Statement]) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    val exprResult =
      expression.verifyIs(context, Set(TypeNames.Boolean), isStatic = false, "If")

    // This is replicating a feature where non-block statements can pass declarations forward
    val stmtRootContext = new InnerBlockVerifyContext(context).withBranchingControl()
    var stmtContext     = stmtRootContext
    statements.foreach(stmt => {
      val isBlock = stmt.isInstanceOf[Block]
      if (isBlock) {
        stmtContext = new InnerBlockVerifyContext(stmtContext).setControlRoot(stmtRootContext)
      }
      stmt.verify(stmtContext)
      if (isBlock)
        context.typePlugin.foreach(
          _.onBlockValidated(stmt.asInstanceOf[Block], context.isStatic, stmtContext)
        )
    })

    verifyControlPath(stmtRootContext, BranchControlPattern(Some(exprResult._2), 2))
  }
}

object IfStatement {
  def construct(parser: CodeParser, ifStatement: IfStatementContext): IfStatement = {
    val statements = CodeParser.toScala(ifStatement.statement())
    IfStatement(
      Expression.construct(ifStatement.parExpression().expression()),
      Statement.construct(parser, statements.toList, isTrigger = false)
    ).withContext(ifStatement)
  }
}

final case class ForStatement(control: Option[ForControl], statement: Option[Statement])
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    control.foreach(control => {
      val forContext = new InnerBlockVerifyContext(context)
      control.verify(forContext)

      val loopContext = new InnerBlockVerifyContext(forContext).setControlRoot(forContext)
      control.addVars(loopContext)
      statement.foreach(_.verify(loopContext))
      verifyControlPath(forContext, BlockControlPattern())
    })
  }
}

object ForStatement {
  def construct(parser: CodeParser, statement: ForStatementContext): ForStatement = {
    ForStatement(
      CodeParser.toScala(statement.forControl()).map(fc => ForControl.construct(parser, fc)),
      CodeParser
        .toScala(statement.statement())
        .flatMap(stmt => Statement.construct(parser, stmt, isTrigger = false))
    ).withContext(statement)
  }
}

/** base for the two types of 'for' loop, Base style e.g. for(Integer i=0; i<10; i++) or enhanced
  * style e.g. for(String a: new List<String>{'x', 'y', 'z'}
  */
sealed abstract class ForControl extends CST {
  def verify(context: BlockVerifyContext): Unit
  def addVars(context: BlockVerifyContext): Unit
}

object ForControl {
  def construct(parser: CodeParser, from: ForControlContext): ForControl = {
    val cst =
      CodeParser
        .toScala(from.enhancedForControl())
        .map(efc => EnhancedForControl.construct(efc))
        .getOrElse(BasicForControl.construct(parser, from))
    cst.withContext(from)
  }
}

/** for-each iteration, e.g. for(String a: new List<String>{'x', 'y', 'z'}
  * @param typeName loop variable type
  * @param id loop variable identifier
  * @param expression iteration expression
  */
final case class EnhancedForControl(typeName: TypeName, id: Id, expression: Expression)
    extends ForControl {

  /** Add vars introduced by the control to a context */
  override def addVars(context: BlockVerifyContext): Unit = {
    context.addVar(id.name, this, isReadOnly = false, typeName)
  }

  override def verify(context: BlockVerifyContext): Unit = {
    id.validate()

    // Check the loop var type is available
    var varTd = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (varTd.isEmpty) {
      context.missingType(id.location, typeName)
      return
    }

    var varTypeName = varTd.get.typeName
    val exprContext = expression.verify(context)
    if (exprContext.isDefined) {

      // Unwrap varTypeName If using grouping query via list loop var,
      // e.g. for(List<Account> a : [Select Id from Account]){..}
      if (varTypeName.isList && exprContext.typeName.isRecordSet) {
        varTypeName = varTypeName.params.head
        varTd = context.getTypeAndAddDependency(varTypeName, context.thisType).toOption
      }

      // Check we are trying to iterate over something iterable
      val iterationTd   = exprContext.typeDeclaration
      val iterationType = getIterationType(iterationTd)
      if (iterationType.isEmpty) {
        context.log(
          Issue(
            ERROR_CATEGORY,
            this.location,
            s"For loop can only have iterable types, not '${iterationTd.typeName}'"
          )
        )
      } else {
        // Check we can assign the iterable type to loop var type
        if (!AssignableSupport.isAssignable(varTypeName, iterationType.get, context)) {
          context.log(
            Issue(
              ERROR_CATEGORY,
              id.location,
              s"Incompatible types in assignment, from '${iterationType.get}' to '$varTypeName'"
            )
          )
        } else {
          // All good, setup save context for definition resolution on loop var, we have do this manually
          // as the loop var is not in scope yet
          context.saveResult(id, id.location.location) {
            ExprContext(Some(false), varTd, varTd.get)
          }
        }
      }
    }
  }

  private def getIterationType(iterableType: TypeDeclaration): Option[TypeName] = {
    val itTypeName = iterableType.typeName
    if (itTypeName.isList || itTypeName.isSet || itTypeName.isIterable || itTypeName.isRecordSet) {
      itTypeName.params.headOption
    } else {
      iterableType.findImplementedTypeParams(TypeNames.Iterable).flatMap(_.headOption)
    }
  }
}

object EnhancedForControl {

  def construct(from: EnhancedForControlContext): EnhancedForControl = {
    EnhancedForControl(
      TypeReference.construct(from.typeRef()),
      Id.construct(from.id()),
      Expression.construct(from.expression())
    ).withContext(from)
  }
}

/** for loop, e.g. for(Integer i; i<10; i++}
  * @param forInit initialization statement
  * @param expression continuation condition
  * @param forUpdate increment statement
  */
final case class BasicForControl(
  forInit: Option[ForInit],
  expression: Option[Expression],
  forUpdate: Option[ForUpdate]
) extends ForControl {
  override def verify(context: BlockVerifyContext): Unit = {
    forInit.foreach(_.verify(context))
    expression.foreach(
      _.verifyIs(context, Set(TypeNames.Boolean), isStatic = false, "For condition")
    )
    forUpdate.foreach(_.verify(context))
  }

  def addVars(context: BlockVerifyContext): Unit = {
    // Not needed, handled by forInit verify
  }
}

object BasicForControl {
  def construct(parser: CodeParser, from: ForControlContext): BasicForControl = {
    val forInit =
      CodeParser
        .toScala(from.forInit())
        .map(fi => ForInit.construct(parser, fi))
    val expression =
      CodeParser
        .toScala(from.expression())
        .map(e => Expression.construct(e))
    val forUpdate =
      CodeParser
        .toScala(from.forUpdate())
        .map(u => ForUpdate.construct(u))
    BasicForControl(forInit, expression, forUpdate).withContext(from)
  }
}

sealed abstract class ForInit extends CST {
  def verify(context: BlockVerifyContext): Unit
  def addVars(context: BlockVerifyContext): Unit
}

final case class LocalVariableForInit(variable: LocalVariableDeclaration) extends ForInit {
  override def verify(context: BlockVerifyContext): Unit = {
    variable.verify(context)
  }

  override def addVars(context: BlockVerifyContext): Unit = {
    variable.addVars(context)
  }
}

final case class ExpressionListForInit(expressions: ArraySeq[Expression]) extends ForInit {
  override def verify(context: BlockVerifyContext): Unit = {
    expressions.foreach(_.verify(context))
  }

  override def addVars(context: BlockVerifyContext): Unit = {}
}

object ForInit {
  def construct(parser: CodeParser, from: ForInitContext): ForInit = {
    CodeParser
      .toScala(from.localVariableDeclaration())
      .map(lvd =>
        LocalVariableForInit(LocalVariableDeclaration.construct(parser, lvd, isTrigger = false))
      )
      .getOrElse({
        val expressions =
          CodeParser.toScala(CodeParser.toScala(from.expressionList()).get.expression())
        ExpressionListForInit(Expression.construct(expressions))
      })
      .withContext(from)
  }
}

final case class ForUpdate(expressions: ArraySeq[Expression]) extends CST {
  def verify(context: BlockVerifyContext): Unit = {
    expressions.foreach(_.verify(context))
  }
}

object ForUpdate {
  def construct(from: ForUpdateContext): ForUpdate = {
    val expressions = CodeParser.toScala(from.expressionList().expression())
    ForUpdate(Expression.construct(expressions)).withContext(from)
  }
}

final case class WhileStatement(expression: Expression, statement: Option[Statement])
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIs(context, Set(TypeNames.Boolean), isStatic = false, "While")
    statement.foreach(_.verify(context))
  }
}

object WhileStatement {
  def construct(parser: CodeParser, statement: WhileStatementContext): WhileStatement = {
    WhileStatement(
      Expression.construct(statement.parExpression().expression()),
      Statement.construct(parser, statement.statement(), isTrigger = false)
    ).withContext(statement)
  }
}

final case class DoWhileStatement(block: Block, expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIs(context, Set(TypeNames.Boolean), isStatic = false, "While")
    block.verify(context)
  }
}

object DoWhileStatement {
  def construct(parser: CodeParser, statement: DoWhileStatementContext): DoWhileStatement = {
    DoWhileStatement(
      Block.constructInner(parser, statement.block()),
      Expression.construct(statement.parExpression.expression())
    ).withContext(statement)
  }
}

final case class TryStatement(block: Block, catches: Seq[CatchClause], finallyBlock: Option[Block])
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    val tryContext = new InnerBlockVerifyContext(context).withBranchingControl()

    block.verify(tryContext)
    catches.foreach(_.verify(tryContext))
    finallyBlock.foreach(_.verify(tryContext))

    val a = mutable
      .ArrayBuffer(true)
      .addAll(catches.map(_ => true))
    finallyBlock.foreach(_ => a.addOne(false))
    verifyControlPath(tryContext, BranchControlPattern(a.toArray))
  }
}

object TryStatement {
  def construct(parser: CodeParser, from: TryStatementContext): TryStatement = {
    val catches = CodeParser.toScala(from.catchClause())
    val finallyBlock =
      CodeParser
        .toScala(from.finallyBlock())
        .map(fb => Block.constructInner(parser, fb.block()))
    TryStatement(
      Block.constructInner(parser, from.block()),
      CatchClause.construct(parser, catches),
      finallyBlock
    ).withContext(from)
  }
}

final case class CatchClause(
  modifiers: ModifierResults,
  qname: QualifiedName,
  id: String,
  block: Option[Block]
) extends CST {
  def verify(context: BlockVerifyContext): Unit = {
    modifiers.issues.foreach(context.log)

    block.foreach(block => {
      val blockContext      = new InnerBlockVerifyContext(context).setControlRoot(context)
      val exceptionTypeName = qname.asTypeName()
      val exceptionType =
        blockContext.getTypeAndAddDependency(exceptionTypeName, context.thisType) match {
          case Left(_) =>
            context.missingType(qname.location, exceptionTypeName)
            context.module.any
          case Right(td) =>
            if (exceptionTypeName.name.endsWith(Names.Exception)) {
              td
            } else {
              context.log(
                Issue(
                  ERROR_CATEGORY,
                  qname.location,
                  s"Catch clause should catch an Exception instance, not a '$exceptionTypeName' instance"
                )
              )
              context.module.any
            }
        }
      // definition = None disables issues like 'Unused' for exceptions
      blockContext.addVar(
        Name(id),
        None,
        modifiers.modifiers.contains(FINAL_MODIFIER),
        exceptionType
      )
      block.verify(blockContext)
      context.typePlugin.foreach(_.onBlockValidated(block, context.isStatic, blockContext))
    })
  }
}

object CatchClause {
  def construct(parser: CodeParser, clauses: Seq[CatchClauseContext]): Seq[CatchClause] = {
    if (clauses != null)
      clauses.flatMap(x => CatchClause.construct(parser, x))
    else
      Seq()
  }

  def construct(parser: CodeParser, from: CatchClauseContext): Option[CatchClause] = {
    QualifiedName
      .construct(from.qualifiedName())
      .map(qualifiedName => {
        CatchClause(
          ApexModifiers.catchModifiers(parser, CodeParser.toScala(from.modifier()), from),
          qualifiedName,
          CodeParser.getText(from.id()),
          CodeParser
            .toScala(from.block())
            .map(block => Block.constructInner(parser, block))
        ).withContext(from)
      })
  }
}

final case class ReturnStatement(expression: Option[Expression]) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    assertReturnType(context, expression.map(_.verify(context)))
      .foreach(msg => context.log(Issue(ERROR_CATEGORY, location, msg)))
    verifyControlPath(context, ExitControlPattern(exitsMethod = true, exitsBlock = true))
  }

  private def assertReturnType(
    context: BlockVerifyContext,
    expr: Option[ExprContext]
  ): Option[String] = {
    val expectedType = context.returnType

    if (expr.isEmpty && expectedType != TypeNames.Void) {
      Some(s"Missing return value of type '$expectedType'")
    } else if (expr.nonEmpty && expectedType == TypeNames.Void) {
      Some(s"Void method can not return a value")
    } else {
      expr.flatMap(e => {
        if (e.isDefined && !isAssignableDeclaration(expectedType, e.typeDeclaration, context))
          Some(s"Incompatible return type, '${e.typeName}' is not assignable to '$expectedType'")
        else
          None
      })
    }
  }
}

object ReturnStatement {
  def construct(statement: ReturnStatementContext): ReturnStatement = {
    ReturnStatement(
      CodeParser
        .toScala(statement.expression())
        .map(e => Expression.construct(e))
    ).withContext(statement)
  }
}

final case class ThrowStatement(expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIsExceptionInstance(context, "Throw")
    verifyControlPath(context, ExitControlPattern(exitsMethod = true, exitsBlock = true))
  }
}

object ThrowStatement {
  def construct(statement: ThrowStatementContext): ThrowStatement = {
    ThrowStatement(Expression.construct(statement.expression())).withContext(statement)
  }
}

final case class BreakStatement() extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    verifyControlPath(context, ExitControlPattern(exitsMethod = false, exitsBlock = true))
  }
}

object BreakStatement {
  def construct(statement: BreakStatementContext): BreakStatement = {
    BreakStatement().withContext(statement)
  }
}

final case class ContinueStatement() extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    verifyControlPath(context, ExitControlPattern(exitsMethod = false, exitsBlock = true))
  }
}

object ContinueStatement {
  def construct(statement: ContinueStatementContext): ContinueStatement = {
    ContinueStatement().withContext(statement)
  }
}

final case class InsertStatement(expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIsSObjectOrSObjectList(context, "Insert")
    verifyControlPath(context)
  }
}

object InsertStatement {
  def construct(statement: InsertStatementContext): InsertStatement = {
    InsertStatement(Expression.construct(statement.expression())).withContext(statement)
  }
}

final case class UpdateStatement(expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIsSObjectOrSObjectList(context, "Update")
    verifyControlPath(context)
  }
}

object UpdateStatement {
  def construct(statement: UpdateStatementContext): UpdateStatement = {
    UpdateStatement(Expression.construct(statement.expression())).withContext(statement)
  }
}

final case class DeleteStatement(expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIsSObjectOrSObjectList(context, "Delete")
    verifyControlPath(context)
  }
}

object DeleteStatement {
  def construct(statement: DeleteStatementContext): DeleteStatement = {
    DeleteStatement(Expression.construct(statement.expression())).withContext(statement)
  }
}

final case class UndeleteStatement(expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIsSObjectOrSObjectList(context, "Undelete")
    verifyControlPath(context)
  }
}

object UndeleteStatement {
  def construct(statement: UndeleteStatementContext): UndeleteStatement = {
    UndeleteStatement(Expression.construct(statement.expression())).withContext(statement)
  }
}

final case class UpsertStatement(expression: Expression, field: Option[QualifiedName])
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verifyIsSObjectOrSObjectList(context, "Upsert")
    // We don't attempt to verify the field here as we don't have the means to determine
    // if standard fields are external ids, and I can't see this being important enough
    // to justify that we work out how to do that.
    verifyControlPath(context)
  }
}

object UpsertStatement {
  def construct(statement: UpsertStatementContext): UpsertStatement = {
    val expression = Expression.construct(statement.expression())
    val qualifiedName = CodeParser
      .toScala(statement.qualifiedName())
      .flatMap(qualifiedName => QualifiedName.construct(qualifiedName))
    UpsertStatement(expression, qualifiedName).withContext(statement)
  }
}

final case class MergeStatement(expression1: Expression, expression2: Expression)
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    val masterTypeName = expression1.verifyIsMergeableSObject(context, "Merge")
    val mergeTypesName = expression2.verifyIsMergeableSObjectOrSObjectList(context, "Merge")
    if (masterTypeName.nonEmpty && mergeTypesName.nonEmpty && masterTypeName != mergeTypesName) {
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"Incompatible types used in merge, '${masterTypeName.get}' and '${mergeTypesName.get}'"
        )
      )
    }
    verifyControlPath(context)
  }
}

object MergeStatement {
  def construct(statement: MergeStatementContext): MergeStatement = {
    val expressions = CodeParser.toScala(statement.expression())
    MergeStatement(Expression.construct(expressions.head), Expression.construct(expressions(1)))
      .withContext(statement)
  }
}

final case class RunAsStatement(expressions: ArraySeq[Expression], block: Option[Block])
    extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    if (expressions.size != 1) {
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"System.runAs must be provided a User or Version argument, not ${expressions.size} arguments"
        )
      )
    } else {
      expressions.head.verifyIs(
        context,
        Set(TypeNames.UserSObject, TypeNames.Version),
        isStatic = false,
        "System.runAs"
      )
    }
    block.foreach(_.verify(context))
    verifyControlPath(context)
  }
}

object RunAsStatement {
  def construct(parser: CodeParser, statement: RunAsStatementContext): RunAsStatement = {
    val expressions: ArraySeq[Expression] =
      CodeParser
        .toScala(statement.expressionList())
        .map(el => Expression.construct(CodeParser.toScala(el.expression())))
        .getOrElse(Expression.emptyExpressions)
    val block =
      CodeParser
        .toScala(statement.block())
        .map(b => Block.constructInner(parser, b))
    RunAsStatement(expressions, block).withContext(statement)
  }
}

final case class ExpressionStatement(var expression: Expression) extends Statement {
  override def verify(context: BlockVerifyContext): Unit = {
    expression.verify(context)
    if (!allowableExpression(expression)) {
      context.log(
        Issue(
          ERROR_CATEGORY,
          expression.location,
          "Only assignment, new & method call expressions can be used as statements"
        )
      )
    }

    verifyControlPath(context)
  }

  @tailrec
  private def allowableExpression(expression: Expression): Boolean = {
    expression match {
      case e: SubExpression           => allowableExpression(e.expression)
      case e: BinaryExpression        => e.isAssignmentOperation
      case e: PrefixExpression        => e.isAssignmentOperation
      case _: PostfixExpression       => true
      case _: MethodCallCtor          => true
      case _: MethodCallWithId        => true
      case _: DotExpressionWithMethod => true
      case _: NewExpression           => true

      case _ =>
        false
    }
  }
}

object ExpressionStatement {
  def construct(statement: ExpressionStatementContext): ExpressionStatement = {
    ExpressionStatement(Expression.construct(statement.expression())).withContext(statement)
  }
}

object Statement {

  /** Create CST statements from ANTLR tree
    *
    * @param parser ANTLR parser, used to extract block source
    * @param statements ANTLR statement contexts
    * @param isTrigger construction is for a trigger
    */
  def construct(
    parser: CodeParser,
    statements: Seq[StatementContext],
    isTrigger: Boolean
  ): Seq[Statement] = {
    statements.flatMap(s => Statement.construct(parser, s, isTrigger))
  }

  /** Create CST statement from ANTLR tree
    *
    * @param parser ANTLR parser, used to extract block source
    * @param statement ANTLR statement context
    * @param isTrigger construction is for a trigger
    */
  def construct(
    parser: CodeParser,
    statement: StatementContext,
    isTrigger: Boolean
  ): Option[Statement] = {
    val typedStatement = CodeParser.toScala(statement.getChild(0))
    if (typedStatement.isEmpty) {
      // Log here just in case
      LoggerOps.info(s"Apex Statement found without content in ${parser.source.path}")
    }
    try {
      typedStatement.map {
        case stmt: BlockContext =>
          Block.constructInner(parser, stmt)
        case stmt: LocalVariableDeclarationStatementContext =>
          LocalVariableDeclarationStatement.construct(parser, stmt, isTrigger)
        case stmt: IfStatementContext =>
          IfStatement.construct(parser, stmt)
        case stmt: SwitchStatementContext =>
          SwitchStatement.construct(parser, stmt)
        case stmt: ForStatementContext =>
          ForStatement.construct(parser, stmt)
        case stmt: WhileStatementContext =>
          WhileStatement.construct(parser, stmt)
        case stmt: DoWhileStatementContext =>
          DoWhileStatement.construct(parser, stmt)
        case stmt: TryStatementContext =>
          TryStatement.construct(parser, stmt)
        case stmt: ReturnStatementContext =>
          ReturnStatement.construct(stmt)
        case stmt: ThrowStatementContext =>
          ThrowStatement.construct(stmt)
        case stmt: BreakStatementContext =>
          BreakStatement.construct(stmt)
        case stmt: ContinueStatementContext =>
          ContinueStatement.construct(stmt)
        case stmt: InsertStatementContext =>
          InsertStatement.construct(stmt)
        case stmt: UpdateStatementContext =>
          UpdateStatement.construct(stmt)
        case stmt: DeleteStatementContext =>
          DeleteStatement.construct(stmt)
        case stmt: UndeleteStatementContext =>
          UndeleteStatement.construct(stmt)
        case stmt: UpsertStatementContext =>
          UpsertStatement.construct(stmt)
        case stmt: MergeStatementContext =>
          MergeStatement.construct(stmt)
        case stmt: RunAsStatementContext =>
          RunAsStatement.construct(parser, stmt)
        case stmt: ExpressionStatementContext =>
          ExpressionStatement.construct(stmt)
      }
    } catch {
      case _: MatchError =>
        // Log here just in case
        LoggerOps.info(s"Unexpected Apex Statement type found in ${parser.source.path}")
        None
    }
  }
}
