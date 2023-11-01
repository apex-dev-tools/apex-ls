/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.stmts.SwitchStatement
import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.rpc.Rename
import com.nawforce.apexlink.types.apex.{ApexFullDeclaration, SummaryDeclaration, SummaryMethod}
import com.nawforce.apexlink.types.core.{DependencyHolder, TypeDeclaration}
import com.nawforce.pkgforce.names.TypeName
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
      case Some(cd: ClassDeclaration)       => getSymbolLocations(cd)
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
      // if symbol at location is usage/call-out
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

          case dotExpression: DotExpressionWithId =>
            dotExpression.expression match {
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
              case _ => None
            }

          case newExpression: NewExpression
              if newExpression.creator.createdName.location.location
                .contains(requestLine, requestOffset) =>
            vr.result.declaration match {
              case Some(cd: ClassDeclaration) => Some(cd)
              case _                          => None
            }

          case id: Id =>
            Some(id)

          case _ => None
        }

      // if symbol location is the declaration
      case None =>
        classDeclaration match {
          case cd: ClassDeclaration =>
            if (cd.idLocation.contains(requestLine, requestOffset)) {
              // refresh for correct dependency holders
              refresh(classDeclaration.location.path, highPriority = true)
              val reloadedClassDec =
                loadFullSourceAndType(classDeclaration.location.path, None)
                  .getOrElse(return None)
                  ._2
              return Some(reloadedClassDec)
            }

            cd.getSuperClassSymbolLocation() match {
              case Some(location) if location.contains(requestLine, requestOffset) =>
                return cd.superClassDeclaration.map(td => td.asInstanceOf[ClassDeclaration])
              case _ =>
            }

            cd.bodyDeclarations
              .foreach {
                case cbd: ClassBodyDeclaration =>
                  if (cbd.idLocation.contains(requestLine, requestOffset)) {
                    // need to refresh for class level to ensure variables set during verify are correct
                    refresh(classDeclaration.location.path, highPriority = true)
                    val reloadedClassDec =
                      loadFullSourceAndType(classDeclaration.location.path, None)
                        .getOrElse(return None)
                        ._2

                    cbd match {
                      // if the declaration on the cursor is a constructor then the declaration is the class itself
                      case _: ApexConstructorDeclaration =>
                        return Some(reloadedClassDec.asInstanceOf[ClassDeclaration])
                      case _ =>
                    }

                    return reloadedClassDec
                      .asInstanceOf[ClassDeclaration]
                      .bodyDeclarations
                      .find(bodyDec => bodyDec.idLocation == cbd.idLocation)
                  }

                  if (cbd.location.location.contains(requestLine, requestOffset)) {
                    cbd match {
                      // if the cursor is on a field declaration data type then a class is the declaration
                      case fieldDec: ApexFieldDeclaration =>
                        if (fieldDec.returnTypeNameLocation.contains(requestLine, requestOffset)) {
                          return resolveClassName(fieldDec.typeName, cd)
                        }
                      case methodDec: ApexMethodDeclaration =>
                        // if the cursor is on the return data type then a class is the declaration
                        if (methodDec.returnTypeNameLocation.contains(requestLine, requestOffset)) {
                          return resolveClassName(methodDec.typeName, cd)
                        }

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
                                  getLocalVariableDeclaration(
                                    statement,
                                    requestLine,
                                    requestOffset,
                                    cd
                                  )
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
                                  getLocalVariableDeclaration(
                                    statement,
                                    requestLine,
                                    requestOffset,
                                    cd
                                  )
                                if (localVarDec.isDefined) return localVarDec
                              })
                          case _ => None
                        }
                      case apd: ApexPropertyDeclaration =>
                        // check return type for renaming classes
                        if (apd.returnTypeNameLocation.contains(requestLine, requestOffset)) {
                          return resolveClassName(apd.typeName, cd)
                        }

                        apd.getter match {
                          case Some(getterBlock) if getterBlock.block.isDefined =>
                            getterBlock.block.get
                              .statements()
                              .foreach(statement => {
                                val localVarDec =
                                  getLocalVariableDeclaration(
                                    statement,
                                    requestLine,
                                    requestOffset,
                                    cd
                                  )
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
                                  getLocalVariableDeclaration(
                                    statement,
                                    requestLine,
                                    requestOffset,
                                    cd
                                  )
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
                                  getLocalVariableDeclaration(
                                    statement,
                                    requestLine,
                                    requestOffset,
                                    cd
                                  )
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

  private def resolveClassName(
    typeName: TypeName,
    cd: ClassDeclaration
  ): Option[ClassDeclaration] = {
    // if the cursor is on the type declaration, return the class declaration
    TypeResolver(typeName, cd).toOption match {
      case Some(declarationClass: ClassDeclaration) => Some(declarationClass)
      case Some(summaryDec: SummaryDeclaration) =>
        reValidate(Set(summaryDec.typeId) ++ summaryDec.getTypeDependencyHolders.toSet)

        loadTypeFromModule(summaryDec.location.path) match {
          case Some(td: TypeDeclaration) =>
            Some(td.asInstanceOf[ClassDeclaration])
          case _ => None
        }
      case _ => None
    }
  }

  private def getLocalVariableDeclaration(
    statement: Statement,
    line: Int,
    offset: Int,
    cd: ClassDeclaration
  ): Option[Locatable] = {
    statement match {
      case varDecStatement: LocalVariableDeclarationStatement =>
        if (
          varDecStatement.localVariableDeclaration.returnTypeNameLocation.contains(line, offset)
        ) {
          return resolveClassName(varDecStatement.localVariableDeclaration.typeName, cd)
        }

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
                val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
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
                val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
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
                val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case doWhileStatement: DoWhileStatement =>
        doWhileStatement.block
          .statements()
          .foreach(statement => {
            val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
            if (varDec.isDefined) return varDec
          })

      case tryStatement: TryStatement =>
        tryStatement.block match {
          case block: Block =>
            block
              .statements()
              .foreach(statement => {
                val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
                if (varDec.isDefined) return varDec
              })
        }

        tryStatement.catches.foreach(catchStatement =>
          catchStatement.block match {
            case Some(block: Block) =>
              block
                .statements()
                .foreach(statement => {
                  val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
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
                val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
                if (varDec.isDefined) return varDec
              })
          case _ =>
        }

      case switchStatement: SwitchStatement =>
        switchStatement.whenControls.foreach(whenControl =>
          whenControl.block
            .statements()
            .foreach(statement => {
              val varDec = getLocalVariableDeclaration(statement, line, offset, cd)
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

    var calloutLocations = declarationDependencyHolders.collect {
      case holdingMethod: ApexMethodDeclaration =>
        val currentClassPath = holdingMethod.location.path
        val methodRenameLocations: mutable.Set[Location] = holdingMethod.block match {
          case Some(block: Block) => getLocationsFromStatements(block.statements(), cbd)
          case _                  => mutable.Set.empty
        }

        cbd match {
          case cd: ClassDeclaration =>
            getDeclarationTypeLocation(holdingMethod, cd) match {
              case Some(location) => methodRenameLocations.add(location)
              case None           =>
            }
          case _ =>
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
            case Some(exp) => getLocationsFromExpression(exp, cbd)
            case None      => mutable.Set.empty
          }

        cbd match {
          case cd: ClassDeclaration =>
            getDeclarationTypeLocation(fieldDeclaration, cd) match {
              case Some(location) => methodRenameLocations.add(location)
              case None           =>
            }
          case _ =>
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

        cbd match {
          case cd: ClassDeclaration =>
            getDeclarationTypeLocation(propertyDeclaration, cd) match {
              case Some(location) => methodRenameLocations.add(location)
              case None           =>
            }
          case _ =>
        }

        Rename(currentClassPath.toString, methodRenameLocations.toArray)

      case holdingInitializerBlock: ApexInitializerBlock =>
        val currentClassPath = holdingInitializerBlock.location.path
        val methodRenameLocations: mutable.Set[Location] =
          getLocationsFromStatements(holdingInitializerBlock.block.statements(), cbd)

        Rename(currentClassPath.toString, methodRenameLocations.toArray)

      case childClass: ClassDeclaration =>
        val currentClassPath = childClass.location.path
        val parentClassSymbolLocation: Array[Location] =
          childClass.getSuperClassSymbolLocation() match {
            case Some(location) => Array(location)
            case None           => Array.empty
          }

        Rename(currentClassPath.toString, parentClassSymbolLocation)

    }.toArray

    cbd match {
      case cd: ClassDeclaration =>
        getConstructorDeclarationLocations(cd) match {
          case Some(rename) => calloutLocations = calloutLocations :+ rename
          case None         =>
        }
        calloutLocations :+ Rename(
          cd.location.path.toString,
          Array(cd.idLocation),
          renameFile = cd.outerTypeName.isEmpty // don't rename file for inner classes
        )
      case _ => calloutLocations :+ Rename(cbd.location.path.toString, Array(cbd.idLocation))
    }

  }

  private def getDeclarationTypeLocation(
    cbd: ClassBodyDeclaration,
    cd: ClassDeclaration
  ): Option[Location] = {
    cbd match {
      case md: ApexMethodDeclaration if md.typeName.name == cd.name =>
        Some(md.returnTypeNameLocation)

      case fd: ApexFieldDeclaration if fd.typeName.name == cd.name =>
        Some(fd.returnTypeNameLocation)

      case pd: ApexPropertyDeclaration if pd.typeName.name == cd.name =>
        Some(pd.returnTypeNameLocation)

      case _ => None
    }
  }

  private def getConstructorDeclarationLocations(cd: ClassDeclaration): Option[Rename] = {
    val constructorLocations =
      cd.localConstructors.map(localConstructor => localConstructor.idLocation)
    if (constructorLocations.nonEmpty) {
      Some(Rename(cd.location.path.toString, constructorLocations.toArray))
    } else {
      None
    }
  }

  private def getDependencyHolders(cbd: ClassBodyDeclaration): Option[Set[DependencyHolder]] = {
    cbd match {
      case md: ApexMethodDeclaration => Some(md.getDependencyHolders)
      case fd: ApexFieldDeclaration  => Some(fd.getDependencyHolders)
      case cd: ClassDeclaration      => Some(cd.getDependencyHolders)
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
          getLocationsFromExpression(expressionStatement.expression, cbd)
        )

      case varDecStatement: LocalVariableDeclarationStatement =>
        cbd match {
          case cd: ClassDeclaration =>
            getTypeLocationForVarDec(varDecStatement, cd) match {
              case Some(location) => methodRenameLocations.add(location)
              case None           =>
            }
          case _ =>
        }
        varDecStatement.localVariableDeclaration.variableDeclarators.declarators.foreach(
          varDeclarator =>
            varDeclarator.init match {
              case Some(exp: Expression) =>
                methodRenameLocations.addAll(getLocationsFromExpression(exp, cbd))
              case _ =>
            }
        )

      case returnStatement: ReturnStatement =>
        returnStatement.expression match {
          case Some(exp: Expression) =>
            methodRenameLocations.addAll(getLocationsFromExpression(exp, cbd))
          case _ =>
        }

      case ifStatement: IfStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(ifStatement.expression, cbd))

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
                      methodRenameLocations.addAll(getLocationsFromExpression(exp, cbd))
                    case _ =>
                  }
                )
              case Some(forInit: ExpressionListForInit) =>
                forInit.expressions.foreach(expression =>
                  methodRenameLocations.addAll(getLocationsFromExpression(expression, cbd))
                )
              case _ =>
            }

            control.expression match {
              case Some(expression: Expression) =>
                methodRenameLocations.addAll(getLocationsFromExpression(expression, cbd))
              case _ =>
            }

            control.forUpdate match {
              case Some(forUpdate: ForUpdate) =>
                forUpdate.expressions.foreach(expression =>
                  methodRenameLocations.addAll(getLocationsFromExpression(expression, cbd))
                )
              case _ =>
            }

          case Some(control: EnhancedForControl) =>
            methodRenameLocations.addAll(getLocationsFromExpression(control.expression, cbd))

          case _ =>
        }

        forStatement.statement match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case whileStatement: WhileStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(whileStatement.expression, cbd))

        whileStatement.statement match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case doWhileStatement: DoWhileStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(doWhileStatement.expression, cbd))

        methodRenameLocations.addAll(
          getLocationsFromStatements(doWhileStatement.block.statements(), cbd)
        )

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
        methodRenameLocations.addAll(getLocationsFromExpression(throwStatement.expression, cbd))

      case insertStatement: InsertStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(insertStatement.expression, cbd))

      case updateStatement: UpdateStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(updateStatement.expression, cbd))

      case deleteStatement: DeleteStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(deleteStatement.expression, cbd))

      case undeleteStatement: UndeleteStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(undeleteStatement.expression, cbd))

      case upsertStatement: UpsertStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(upsertStatement.expression, cbd))

      case mergeStatement: MergeStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(mergeStatement.expression1, cbd))
        methodRenameLocations.addAll(getLocationsFromExpression(mergeStatement.expression2, cbd))

      case runAsStatement: RunAsStatement =>
        runAsStatement.expressions.foreach(exp =>
          methodRenameLocations.addAll(getLocationsFromExpression(exp, cbd))
        )

        runAsStatement.block match {
          case Some(block: Block) =>
            methodRenameLocations.addAll(getLocationsFromStatements(block.statements(), cbd))
          case _ =>
        }

      case switchStatement: SwitchStatement =>
        methodRenameLocations.addAll(getLocationsFromExpression(switchStatement.expression, cbd))

        switchStatement.whenControls.foreach(whenControl =>
          methodRenameLocations.addAll(
            getLocationsFromStatements(whenControl.block.statements(), cbd)
          )
        )

      case _ =>
    }

    methodRenameLocations
  }

  private def getTypeLocationForVarDec(
    statement: LocalVariableDeclarationStatement,
    cd: ClassDeclaration
  ): Option[Location] = {
    if (statement.localVariableDeclaration.typeName.name == cd.name) {
      val varDec = statement.localVariableDeclaration.variableDeclarators.declarators.head

      // variable name location minus the length of the type dec -1 (for the space before)
      val startPosition = varDec.location.location.startPosition - cd.name.value.length - 1
      // variable name start pos -1 (for the space) to get the end of the type dec location.
      val endPosition = varDec.location.location.startPosition - 1
      val startLine   = varDec.location.location.startLine
      val endLine     = varDec.location.location.endLine

      Some(Location(startLine, startPosition, endLine, endPosition))
    } else {
      None
    }
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

  private def getVarLocationFromPrimaryExp(
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

  private def getLocationsFromExpression(
    expression: Expression,
    cbd: ClassBodyDeclaration
  ): mutable.Set[Location] = {
    val methodCallLocations: mutable.Set[Location] = mutable.Set.empty
    expression match {
      case methodCall: MethodCallWithId =>
        methodCall.arguments.foreach(exp =>
          methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
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
          methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
        )

      case dotExpression: DotExpressionWithMethod =>
        dotExpression.target match {
          case Some(exp: Expression) =>
            methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
          case _ =>
        }
        methodCallLocations.addAll(getLocationsFromExpression(dotExpression.expression, cbd))

      case dotExpression: DotExpressionWithId =>
        methodCallLocations.addAll(getLocationsFromExpression(dotExpression.expression, cbd))
        cbd match {
          case fd: ApexFieldDeclaration =>
            getVarLocationFromDotExpression(dotExpression, fd) match {
              case Some(location) => methodCallLocations.add(location)
              case _              =>
            }
          case _ =>
        }

      case binaryExpression: BinaryExpression =>
        binaryExpression.rhs match {
          case exp: Expression =>
            methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
          case _ =>
        }
        binaryExpression.lhs match {
          case exp: Expression =>
            methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
          case _ =>
        }

      case primaryExpression: PrimaryExpression =>
        primaryExpression.primary match {
          case soql: SOQL =>
            soql.boundExpressions.foreach(exp =>
              methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
            )
          case sosl: SOSL =>
            sosl.boundExpressions.foreach(exp =>
              methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
            )
          case _ =>
        }

        cbd match {
          case fd: ApexFieldDeclaration =>
            getVarLocationFromPrimaryExp(primaryExpression, fd) match {
              case Some(l) => methodCallLocations.add(l)
              case _       =>
            }
          case cd: ClassDeclaration =>
            getClassNameLocationFromPrimaryExp(primaryExpression, cd) match {
              case Some(location) => methodCallLocations.add(location)
              case _              =>
            }
          case _ =>
        }

      case arrayExpression: ArrayExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(arrayExpression.expression, cbd))
        methodCallLocations.addAll(getLocationsFromExpression(arrayExpression.arrayExpression, cbd))

      case castExpression: CastExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(castExpression.expression, cbd))

      case newExpression: NewExpression =>
        newExpression.creator.creatorRest match {
          case Some(cr: SetOrListCreatorRest) =>
            cr.parts.foreach(exp =>
              methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
            )
          case Some(cr: ArrayCreatorRest) =>
            cr.indexExpression match {
              case Some(exp: Expression) =>
                methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
              case _ =>
            }
            cr.arrayInitializer match {
              case Some(initializer) =>
                initializer.expressions.foreach(exp =>
                  methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
                )
              case _ =>
            }
          case Some(cr: MapCreatorRest) =>
            cr.pairs.foreach(pair => {
              methodCallLocations.addAll(getLocationsFromExpression(pair.from, cbd))
              methodCallLocations.addAll(getLocationsFromExpression(pair.to, cbd))
            })
          case Some(cr: ClassCreatorRest) =>
            cr.arguments.foreach(exp =>
              methodCallLocations.addAll(getLocationsFromExpression(exp, cbd))
            )
          case _ =>
        }

        cbd match {
          case cd: ClassDeclaration =>
            getConstructorLocationFromExp(newExpression, cd) match {
              case Some(l) => methodCallLocations.add(l)
              case _       =>
            }
          case _ =>
        }

      case negationExpression: NegationExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(negationExpression.expression, cbd))

      case subExpression: SubExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(subExpression.expression, cbd))

      case prefixExpression: PrefixExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(prefixExpression.expression, cbd))

      case postfixExpression: PostfixExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(postfixExpression.expression, cbd))

      case instanceOfExpression: InstanceOfExpression =>
        methodCallLocations.addAll(getLocationsFromExpression(instanceOfExpression.expression, cbd))

      case _ =>
    }
    methodCallLocations
  }

  private def getVarLocationFromDotExpression(
    dotExpression: DotExpressionWithId,
    fd: ApexFieldDeclaration
  ): Option[Location] = {
    dotExpression.expression match {
      case primaryExpression: PrimaryExpression if dotExpression.target == fd.id =>
        primaryExpression.primary match {
          case idPrimary: IdPrimary =>
            idPrimary.typeName match {
              // when called off an object
              case Some(typeName) if typeName == fd.thisTypeId.typeName =>
                Some(dotExpression.target.location.location)

              // when called off a class
              case None if idPrimary.id.name == fd.thisTypeId.typeName.name =>
                Some(dotExpression.target.location.location)

              case _ => None
            }
          case _ => None
        }

      case _ => None
    }
  }

  private def getConstructorLocationFromExp(
    newExpression: NewExpression,
    cd: ClassDeclaration
  ): Option[Location] = {
    if (newExpression.creator.createdName.typeName.name == cd.name) {
      Some(newExpression.creator.createdName.location.location)
    } else {
      None
    }
  }

  private def getClassNameLocationFromPrimaryExp(
    primaryExp: PrimaryExpression,
    cd: ClassDeclaration
  ): Option[Location] = {
    primaryExp.primary match {
      case idPrimary: IdPrimary if idPrimary.id == cd.id => Some(idPrimary.location.location)
      case _                                             => None
    }
  }

}
