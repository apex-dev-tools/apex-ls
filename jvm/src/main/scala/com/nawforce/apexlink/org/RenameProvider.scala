/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.cst.stmts.SwitchStatement
import com.nawforce.apexlink.rpc.Rename
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, SummaryMethod}
import com.nawforce.apexlink.types.core.DependencyHolder
import com.nawforce.pkgforce.path.{Locatable, Location, PathLike}

import scala.collection.mutable

trait RenameProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getRenameLocations(
    path: PathLike,
    line: Int,
    offset: Int,
    content: Option[String]
  ): Array[Rename] = {
    val sourceAndType = loadFullSourceAndType(path, None).getOrElse(return Array.empty)

    val validation = locateFromValidation(sourceAndType._2, line, offset)

    val declaration = getClassBodyDeclaration(sourceAndType._2, validation, line, offset)

    declaration match {
      case Some(cbd: ClassBodyDeclaration)  => getSymbolLocations(cbd)
      case Some(varDec: VariableDeclarator) => getLocalVarSymbolLocations(varDec.id, validation)
      case Some(id: Id)                     => getLocalVarSymbolLocations(id, validation)
      case _                                => Array.empty
    }
  }

  /** Retrieves the declaration for the symbol that the rename event was fired on.
    *
    * If the declaration is at the class level, then the Locatable returned will be a ClassBodyDeclaration.
    * Otherwise, expect either an Id or a VariableDeclarator object to be returned for local declarations.
    */
  private def getClassBodyDeclaration(
    classDeclaration: ApexFullDeclaration,
    validation: (Map[Location, ValidationResult], Option[Location]),
    requestLine: Int,
    requestOffset: Int
  ): Option[Locatable] = {
    validation._2 match {
      case Some(location) =>
        val vr = validation._1(location)
        vr.cst match {
          case methodCall: MethodCallWithId =>
            methodCall.cachedMethod match {
              case Some(amd: ApexMethodDeclaration) =>
                // refresh require for correct dependencyHolders on declaration
                refresh(amd.location.path, highPriority = true)
                loadTypeFromModule(amd.location.path) match {
                  case Some(reloadedClassDec: ClassDeclaration) =>
                    reloadedClassDec.bodyDeclarations
                      .find {
                        case reloadedMethodDec: ApexMethodDeclaration =>
                          reloadedMethodDec.idPathLocation == amd.idPathLocation
                        case _ => false
                      }
                  case _ => None
                }
              case Some(sm: SummaryMethod) =>
                reValidate(Set(sm.thisTypeId) ++ sm.getDependencyHolders.collect {
                  case dh if dh.thisTypeIdOpt.isDefined => dh.thisTypeIdOpt.get
                })
                loadTypeFromModule(sm.location.path) match {
                  case Some(reloadedClassDec: ClassDeclaration) =>
                    reloadedClassDec.bodyDeclarations
                      .find {
                        case amd: ApexMethodDeclaration =>
                          amd.idPathLocation == sm.idPathLocation
                        case _ => false
                      }
                  case _ => None
                }
              case _ => None
            }

          case primaryExpression: PrimaryExpression =>
            primaryExpression.primary match {
              case _: IdPrimary =>
                vr.result.locatable match {
                  case Some(enhancedForControl: EnhancedForControl) =>
                    Some(enhancedForControl.id)
                  case _ => vr.result.locatable
                }
              case _ => None
            }

          case id: Id =>
            Some(id)

          case _ => None
        }

      case None =>
        classDeclaration match {
          case cd: ClassDeclaration =>
            cd.bodyDeclarations
              .foreach {
                case cbd: ClassBodyDeclaration =>
                  if (cbd.idLocation.contains(requestLine, requestOffset)) {
                    return Some(cbd)
                  }
                  if (cbd.location.location.contains(requestLine, requestOffset)) {
                    cbd match {
                      case methodDec: ApexMethodDeclaration =>
                        methodDec.parameters.foreach(parameter =>
                          if (parameter.id.location.location.contains(requestLine, requestOffset)) {
                            return Some(parameter.id)
                          }
                        )

                        methodDec.block match {
                          case Some(block: Block) =>
                            block
                              .statements()
                              .foreach(statement => {
                                val localVarDec =
                                  getLocalVariableDeclaration(statement, requestLine, requestOffset)
                                if (localVarDec.isDefined) return localVarDec
                              })
                          case _ => None
                        }
                      case constructorDec: ApexConstructorDeclaration =>
                        constructorDec.parameters.foreach(parameter =>
                          if (parameter.id.location.location.contains(requestLine, requestOffset)) {
                            return Some(parameter.id)
                          }
                        )

                        constructorDec.block match {
                          case block: Block =>
                            block
                              .statements()
                              .foreach(statement => {
                                val localVarDec =
                                  getLocalVariableDeclaration(statement, requestLine, requestOffset)
                                if (localVarDec.isDefined) return localVarDec
                              })
                          case _ => None
                        }
                      case apd: ApexPropertyDeclaration =>
                        apd.getter match {
                          case Some(getterBlock) if getterBlock.block.isDefined =>
                            getterBlock.block.get
                              .statements()
                              .foreach(statement => {
                                val localVarDec =
                                  getLocalVariableDeclaration(statement, requestLine, requestOffset)
                                if (localVarDec.isDefined) return localVarDec
                              })
                          case _ =>
                        }
                        apd.setter match {
                          case Some(setterBlock) if setterBlock.block.isDefined =>
                            setterBlock.block.get
                              .statements()
                              .foreach(statement => {
                                val localVarDec =
                                  getLocalVariableDeclaration(statement, requestLine, requestOffset)
                                if (localVarDec.isDefined) return localVarDec
                              })
                          case _ =>
                        }

                      case initializerBlock: ApexInitializerBlock =>
                        initializerBlock.block match {
                          case block: Block =>
                            block
                              .statements()
                              .foreach(statement => {
                                val localVarDec =
                                  getLocalVariableDeclaration(statement, requestLine, requestOffset)
                                if (localVarDec.isDefined) return localVarDec
                              })
                          case _ => None
                        }
                      case _ =>
                    }
                  }
                case _ =>
              }
          case _ =>
        }
        None
    }
  }

  private def getLocalVariableDeclaration(
    statement: Statement,
    line: Int,
    offset: Int
  ): Option[VariableDeclarator] = {
    statement match {
      case varDecStatement: LocalVariableDeclarationStatement =>
        varDecStatement.localVariableDeclaration.variableDeclarators.declarators.foreach(
          varDeclarator =>
            if (varDeclarator.location.location.contains(line, offset)) {
              return Some(varDeclarator)
            }
        )

      case ifStatement: IfStatement =>
        ifStatement.statements.foreach {
          case block: Block =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case forStatement: ForStatement =>
        forStatement.control match {
          case Some(control: BasicForControl) =>
            control.forInit match {
              case Some(forInit: LocalVariableForInit) =>
                forInit.variable.variableDeclarators.declarators.foreach(varDeclarator =>
                  if (varDeclarator.location.location.contains(line, offset)) {
                    return Some(varDeclarator)
                  }
                )
              case _ =>
            }

          case _ =>
        }

        forStatement.statement match {
          case Some(block: Block) =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case whileStatement: WhileStatement =>
        whileStatement.statement match {
          case Some(block: Block) =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case doWhileStatement: DoWhileStatement =>
        doWhileStatement.statement match {
          case Some(block: Block) =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case tryStatement: TryStatement =>
        tryStatement.block match {
          case block: Block =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset)
                if (varDec.isDefined) return varDec
              })
        }

        tryStatement.catches.foreach(catchStatement =>
          catchStatement.block match {
            case Some(block: Block) =>
              block
                .statements()
                .foreach(statement => {
                  val varDec = getLocalVariableDeclaration(statement, line, offset)
                  if (varDec.isDefined) return varDec
                })
            case _ =>
          }
        )

        tryStatement.finallyBlock match {
          case Some(block: Block) =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case switchStatement: SwitchStatement =>
        switchStatement.whenControls.foreach(whenControl =>
          whenControl.block
            .statements()
            .foreach(statement => {
              val varDec = getLocalVariableDeclaration(statement, line, offset)
              if (varDec.isDefined) return varDec
            })
        )

      case _ =>
    }
    None
  }

  private def getLocalVarSymbolLocations(
    varDecId: Id,
    validation: (Map[Location, ValidationResult], Option[Location])
  ): Array[Rename] = {
    val locations: mutable.Set[Location] = mutable.Set(varDecId.location.location)
    validation._1.foreach(vr =>
      vr._2.result.locatable match {
        case Some(currentSymbolVarDec: VariableDeclarator) =>
          if (currentSymbolVarDec.id eq varDecId) {
            locations.add(vr._1)
          }
        case Some(enhancedForControl: EnhancedForControl) =>
          if (enhancedForControl.id eq varDecId) {
            locations.add(vr._1)
          }
        case Some(id: Id) =>
          if (id eq varDecId) {
            locations.add(vr._1)
          }
        case _ =>
      }
    )

    Array(Rename(varDecId.location.path.toString, locations.toArray))
  }

  private def getSymbolLocations(cbd: ClassBodyDeclaration): Array[Rename] = {
    val declarationDependencyHolders = getDependencyHolders(cbd).getOrElse(
      return Array(Rename(cbd.location.path.toString, Array(cbd.idLocation)))
    )

    val calloutLocations = declarationDependencyHolders.collect {
      case holdingMethod: ApexMethodDeclaration =>
        val currentClassPath = holdingMethod.location.path
        val methodRenameLocations: mutable.Set[Location] = holdingMethod.block match {
          case Some(block: Block) => getLocationsFromStatements(block.statements(), cbd)
          case _                  => mutable.Set.empty
        }

        Rename(currentClassPath.toString, methodRenameLocations.toArray)

      case holdingConstructor: ApexConstructorDeclaration =>
        val currentClassPath = holdingConstructor.location.path
        val methodRenameLocations: mutable.Set[Location] =
          getLocationsFromStatements(holdingConstructor.block.statements(), cbd)

        Rename(currentClassPath.toString, methodRenameLocations.toArray)

      case fieldDeclaration: ApexFieldDeclaration =>
        val currentClassPath = fieldDeclaration.location.path
        val methodRenameLocations: mutable.Set[Location] =
          fieldDeclaration.variableDeclarator.init match {
            case Some(exp) => getMethodLocationsFromExpression(exp, cbd)
            case None      => mutable.Set.empty
          }

        Rename(currentClassPath.toString, methodRenameLocations.toArray)

      case propertyDeclaration: ApexPropertyDeclaration =>
        val currentClassPath                             = propertyDeclaration.location.path
        val methodRenameLocations: mutable.Set[Location] = mutable.Set.empty
        propertyDeclaration.getter match {
          case Some(getterBlock) if getterBlock.block.isDefined =>
            methodRenameLocations.addAll(
              getLocationsFromStatements(getterBlock.block.get.statements(), cbd)
            )
          case _ =>
        }
        propertyDeclaration.setter match {
          case Some(setterBlock) if setterBlock.block.isDefined =>
            methodRenameLocations.addAll(
              getLocationsFromStatements(setterBlock.block.get.statements(), cbd)
            )
          case _ =>
        }

        Rename(currentClassPath.toString, methodRenameLocations.toArray)

      case holdingInitializerBlock: ApexInitializerBlock =>
        val currentClassPath = holdingInitializerBlock.location.path
        val methodRenameLocations: mutable.Set[Location] =
          getLocationsFromStatements(holdingInitializerBlock.block.statements(), cbd)

        Rename(currentClassPath.toString, methodRenameLocations.toArray)
    }.toArray

    calloutLocations :+ Rename(cbd.location.path.toString, Array(cbd.idLocation))
  }

  private def getDependencyHolders(cbd: ClassBodyDeclaration): Option[Set[DependencyHolder]] = {
    cbd match {
      case md: ApexMethodDeclaration => Some(md.getDependencyHolders)
      case fd: ApexFieldDeclaration  => Some(fd.getDependencyHolders)
      case _                         => None
    }
  }

  private def getLocationsFromStatements(
    statements: Seq[Statement],
    cbd: ClassBodyDeclaration
  ): mutable.Set[Location] = {
    val methodRenameLocations: mutable.Set[Location] =
      mutable.Set.empty

    statements.foreach {
      case expressionStatement: ExpressionStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(expressionStatement.expression, cbd)
        )

      case varDecStatement: LocalVariableDeclarationStatement =>
        varDecStatement.localVariableDeclaration.variableDeclarators.declarators.foreach(
          varDeclarator =>
            varDeclarator.init match {
              case Some(exp: Expression) =>
                methodRenameLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
              case _ =>
            }
        )

      case returnStatement: ReturnStatement =>
        returnStatement.expression match {
          case Some(exp: Expression) =>
            methodRenameLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
          case _ =>
        }

      case ifStatement: IfStatement =>
        methodRenameLocations.addAll(getMethodLocationsFromExpression(ifStatement.expression, cbd))

        ifStatement.statements.foreach {
          case block: Block =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case forStatement: ForStatement =>
        forStatement.control match {
          case Some(control: BasicForControl) =>
            control.forInit match {
              case Some(forInit: LocalVariableForInit) =>
                forInit.variable.variableDeclarators.declarators.foreach(varDeclarator =>
                  varDeclarator.init match {
                    case Some(exp: Expression) =>
                      methodRenameLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
                    case _ =>
                  }
                )
              case Some(forInit: ExpressionListForInit) =>
                forInit.expressions.foreach(expression =>
                  methodRenameLocations.addAll(getMethodLocationsFromExpression(expression, cbd))
                )
              case _ =>
            }

            control.expression match {
              case Some(expression: Expression) =>
                methodRenameLocations.addAll(getMethodLocationsFromExpression(expression, cbd))
              case _ =>
            }

            control.forUpdate match {
              case Some(forUpdate: ForUpdate) =>
                forUpdate.expressions.foreach(expression =>
                  methodRenameLocations.addAll(getMethodLocationsFromExpression(expression, cbd))
                )
              case _ =>
            }

          case Some(control: EnhancedForControl) =>
            methodRenameLocations.addAll(getMethodLocationsFromExpression(control.expression, cbd))

          case _ =>
        }

        forStatement.statement match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case whileStatement: WhileStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(whileStatement.expression, cbd)
        )

        whileStatement.statement match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case doWhileStatement: DoWhileStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(doWhileStatement.expression, cbd)
        )

        doWhileStatement.statement match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case tryStatement: TryStatement =>
        tryStatement.block match {
          case block: Block =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
        }

        tryStatement.catches.foreach(catchStatement =>
          catchStatement.block match {
            case Some(block: Block) =>
              methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
            case _ =>
          }
        )

        tryStatement.finallyBlock match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case throwStatement: ThrowStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(throwStatement.expression, cbd)
        )

      case insertStatement: InsertStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(insertStatement.expression, cbd)
        )

      case updateStatement: UpdateStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(updateStatement.expression, cbd)
        )

      case deleteStatement: DeleteStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(deleteStatement.expression, cbd)
        )

      case undeleteStatement: UndeleteStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(undeleteStatement.expression, cbd)
        )

      case upsertStatement: UpsertStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(upsertStatement.expression, cbd)
        )

      case mergeStatement: MergeStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(mergeStatement.expression1, cbd)
        )
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(mergeStatement.expression2, cbd)
        )

      case runAsStatement: RunAsStatement =>
        runAsStatement.expressions.foreach(exp =>
          methodRenameLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
        )

        runAsStatement.block match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case switchStatement: SwitchStatement =>
        methodRenameLocations.addAll(
          getMethodLocationsFromExpression(switchStatement.expression, cbd)
        )

        switchStatement.whenControls.foreach(whenControl =>
          methodRenameLocations.addAll(
            getLocationsFromStatements(whenControl.block.statements(), cbd)
          )
        )

      case _ =>
    }

    methodRenameLocations
  }

  private def validateExpression(expression: Expression): Option[Expression] = {
    val sourceAndType = loadFullSourceAndType(expression.location.path, None)
    val validation = locateFromValidation(
      sourceAndType.get._2,
      expression.location.location.startLine,
      expression.location.location.startPosition
    )

    validation
      ._1(validation._2.getOrElse(return None))
      .cst match {
      case validatedMethodCall: Expression => Some(validatedMethodCall)
      case _                               => None
    }
  }

  private def getLocationFromMethodCall(
    methodCall: MethodCallWithId,
    md: ApexMethodDeclaration
  ): Option[Location] = {
    if (methodCall.cachedMethod.isEmpty) {
      val validatedMethodCall = validateExpression(methodCall)
      validatedMethodCall match {
        case Some(methodCall: MethodCallWithId) =>
          methodCall.getTargetLocationForMethodCallOut(md)
        case _ => None
      }
    } else {
      methodCall.getTargetLocationForMethodCallOut(md)
    }
  }

  private def getLocationFromPrimaryExp(
    primaryExpression: PrimaryExpression,
    fd: ApexFieldDeclaration
  ): Option[Location] = {
    primaryExpression.primary match {
      case id: IdPrimary =>
        if (id.isCachedFieldEmpty) {
          val validatedPrimaryExpression = validateExpression(primaryExpression)
          validatedPrimaryExpression match {
            case Some(primaryExpression: PrimaryExpression) =>
              primaryExpression.primary
                .asInstanceOf[IdPrimary]
                .getLocationForClassFieldUsage(fd)
            case _ => None
          }
        } else {
          id.getLocationForClassFieldUsage(fd)
        }
      case _ => None
    }
  }

  private def getMethodLocationsFromExpression(
    expression: Expression,
    cbd: ClassBodyDeclaration
  ): mutable.Set[Location] = {
    val methodCallLocations: mutable.Set[Location] = mutable.Set.empty
    expression match {
      case methodCall: MethodCallWithId =>
        methodCall.arguments.foreach(exp =>
          methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
        )

        cbd match {
          case md: ApexMethodDeclaration =>
            getLocationFromMethodCall(methodCall, md) match {
              case Some(l) => methodCallLocations.add(l)
              case _       =>
            }
          case _ =>
        }

      case constructorCall: MethodCallCtor =>
        constructorCall.arguments.foreach(exp =>
          methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
        )

      case dotExpression: DotExpressionWithMethod =>
        dotExpression.target match {
          case Some(exp: Expression) =>
            methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
          case _ =>
        }
        methodCallLocations.addAll(getMethodLocationsFromExpression(dotExpression.expression, cbd))

      case binaryExpression: BinaryExpression =>
        binaryExpression.rhs match {
          case exp: Expression =>
            methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
          case _ =>
        }
        binaryExpression.lhs match {
          case exp: Expression =>
            methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
          case _ =>
        }

      case primaryExpression: PrimaryExpression =>
        primaryExpression.primary match {
          case soql: SOQL =>
            soql.boundExpressions.foreach(exp =>
              methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
            )
          case sosl: SOSL =>
            sosl.boundExpressions.foreach(exp =>
              methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
            )
          case _ =>
        }

        cbd match {
          case fd: ApexFieldDeclaration =>
            getLocationFromPrimaryExp(primaryExpression, fd) match {
              case Some(l) => methodCallLocations.add(l)
              case _       =>
            }
          case _ =>
        }

      case arrayExpression: ArrayExpression =>
        methodCallLocations.addAll(
          getMethodLocationsFromExpression(arrayExpression.expression, cbd)
        )
        methodCallLocations.addAll(
          getMethodLocationsFromExpression(arrayExpression.arrayExpression, cbd)
        )

      case castExpression: CastExpression =>
        methodCallLocations.addAll(getMethodLocationsFromExpression(castExpression.expression, cbd))

      case newExpression: NewExpression =>
        newExpression.creator.creatorRest match {
          case Some(cr: SetOrListCreatorRest) =>
            cr.parts.foreach(exp =>
              methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
            )
          case Some(cr: ArrayCreatorRest) =>
            cr.indexExpression match {
              case Some(exp: Expression) =>
                methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
              case _ =>
            }
            cr.arrayInitializer match {
              case Some(initialiser) =>
                initialiser.expressions.foreach(exp =>
                  methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
                )
              case _ =>
            }
          case Some(cr: MapCreatorRest) =>
            cr.pairs.foreach(pair => {
              methodCallLocations.addAll(getMethodLocationsFromExpression(pair.from, cbd))
              methodCallLocations.addAll(getMethodLocationsFromExpression(pair.to, cbd))
            })
          case Some(cr: ClassCreatorRest) =>
            cr.arguments.foreach(exp =>
              methodCallLocations.addAll(getMethodLocationsFromExpression(exp, cbd))
            )
          case _ =>
        }

      case negationExpression: NegationExpression =>
        methodCallLocations.addAll(
          getMethodLocationsFromExpression(negationExpression.expression, cbd)
        )

      case subExpression: SubExpression =>
        methodCallLocations.addAll(getMethodLocationsFromExpression(subExpression.expression, cbd))

      case prefixExpression: PrefixExpression =>
        methodCallLocations.addAll(
          getMethodLocationsFromExpression(prefixExpression.expression, cbd)
        )

      case postfixExpression: PostfixExpression =>
        methodCallLocations.addAll(
          getMethodLocationsFromExpression(postfixExpression.expression, cbd)
        )

      case instanceOfExpression: InstanceOfExpression =>
        methodCallLocations.addAll(
          getMethodLocationsFromExpression(instanceOfExpression.expression, cbd)
        )

      case _ =>
    }
    methodCallLocations
  }

}
