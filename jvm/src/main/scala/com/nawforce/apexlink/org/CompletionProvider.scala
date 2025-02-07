/*
 Copyright (c) 2021 Kevin Jones & FinancialForce, All rights reserved.
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
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.org.CompletionProvider._
import com.nawforce.apexlink.org.TextOps.TestOpsUtils
import com.nawforce.apexlink.rpc.CompletionItemLink
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexFullDeclaration}
import com.nawforce.apexlink.types.core._
import com.nawforce.pkgforce.documents.{ApexClassDocument, ApexTriggerDocument, MetadataDocument}
import com.nawforce.pkgforce.modifiers.{PRIVATE_MODIFIER, PROTECTED_MODIFIER, PUBLIC_MODIFIER}
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.path.PathLike
import com.vmware.antlr4c3.CodeCompletionCore
import io.github.apexdevtools.apexparser.{ApexLexer, ApexParser}
import org.antlr.v4.runtime.Token

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

trait CompletionProvider {
  this: OPM.PackageImpl =>

  def getCompletionItems(
    path: PathLike,
    line: Int,
    offset: Int,
    content: String
  ): Array[CompletionItemLink] = {

    // Get basic context of what we are looking at
    val module            = getPackageModule(path)
    val terminatedContent = injectStatementTerminator(line, offset, content)
    val classDetails = MetadataDocument(path)
      .collect {
        case _: ApexClassDocument   => loadClass(path, terminatedContent._1)
        case _: ApexTriggerDocument => loadTrigger(path, terminatedContent._1)
      }
      .getOrElse((None, None))

    if (classDetails._1.isEmpty)
      /* Bail if we did not at least parse the content */
      return emptyCompletions
    val parserAndCU     = classDetails._1.get
    val fromDeclaration = classDetails._2
    val adjustedOffset  = terminatedContent._2

    // Attempt to find a searchTerm for dealing with dot expressions
    lazy val searchTerm =
      content.extractDotTermExclusive(() => new IdentifierAndMethodLimiter, line, offset)

    // Setup to lazy validate the Class block to recover the validationResult for the cursor position
    lazy val validationResult: Option[ValidationResult] = {
      if (fromDeclaration.nonEmpty && searchTerm.nonEmpty) {
        val searchEnd     = searchTerm.get.location.endPosition
        val resultMap     = fromDeclaration.get.getValidationMap(line, searchEnd)
        val exprLocations = resultMap.keys.filter(_.contains(line, searchEnd))
        val targetExpression =
          exprLocations.find(exprLocation => exprLocations.forall(_.contains(exprLocation)))
        targetExpression.map(targetExpression => resultMap(targetExpression))
      } else {
        None
      }
    }

    // Lazy extract local vars in scope at cursor position from the validation
    lazy val localVars = validationResult.flatMap(_.vars).getOrElse(mutable.Map())

    // Run C3 to get our keywords & rule matches
    val tokenAndIndex =
      findTokenAndIndex(parserAndCU._1, line, adjustedOffset, offset != adjustedOffset)
    val core = new CodeCompletionCore(parserAndCU._1, preferredRules.asJava, ignoredTokens.asJava)
    val candidates = core.collectCandidates(tokenAndIndex._2, parserAndCU._2, MAX_STATES)

    // Generate a list of possible keyword matches
    val keywords = candidates.tokens.asScala
      .filter(_._1 >= 1)
      .map(kv => parserAndCU._1.getVocabulary.getDisplayName(kv._1))
      .map(keyword => stripQuotes(keyword))
      .map(keyword => CompletionItemLink(keyword, "Keyword"))
      .toArray

    val creatorCompletions =
      if (fromDeclaration.nonEmpty && keywords.map(_.label).contains("new")) {
        getEmptyCreatorCompletionItems(fromDeclaration.get, terminatedContent._3)
      } else {
        emptyCompletions
      }

    // Find completions for a dot expression, we use the safe navigation operator here as a trigger as it is unique to
    // dot expressions. We can't use rule matching for these as often the expression before the '.' is complete and
    // we have to remove the '.' so the parser constructs a statement, see injectStatementTerminator().
    val dotCompletions =
      if (keywords.exists(_.label == "?.") && searchTerm.nonEmpty) {
        validationResult
          .filter(_.result.declaration.nonEmpty)
          .map(validationResult =>
            getAllCompletionItems(
              fromDeclaration,
              validationResult.result.declaration.get,
              validationResult.result.isStatic,
              searchTerm.get.residualExpr
            )
          )
          .getOrElse(emptyCompletions)
      } else {
        emptyCompletions
      }

    /* Now for rule matches. These are not distinct cases, they might combine to give the correct result. */
    var haveTypes = false
    val rules = candidates.rules.asScala
      .map(_._1.toInt)
      .flatMap { id =>
        id match {
          /* TypeRefs appear in lots of places, e.g. inside Primaries but we just handle once for simplicity. */
          case ApexParser.RULE_typeRef =>
            if (haveTypes) Array[CompletionItemLink]()
            else {
              haveTypes = true
              module.map(_.matchTypeName(terminatedContent._3, offset)).getOrElse(Array())
            }

          /* Primary will appear at the start of an expression (recursively) but this just handles the first primary as
           * dotCompletions covers over cases. At the point it is being handled it is indistinguishable from a MethodCall
           * so we handle both cases. */
          case ApexParser.RULE_primary =>
            /* Also handles start of methodCall */
            searchTerm
              .map(searchTerm => {
                // Local vars can only be on initial primary
                if (searchTerm.prefixExpr.isEmpty) {
                  localVars.keys
                    .filter(
                      _.value.take(1).toLowerCase == searchTerm.residualExpr.take(1).toLowerCase
                    )
                    .map(name =>
                      CompletionItemLink(
                        name.value,
                        "Variable",
                        localVars(name).declaration.typeName.toString()
                      )
                    )
                    .toArray ++
                    classDetails._2
                      .map(td => getAllCompletionItems(Some(td), td, None, searchTerm.residualExpr))
                      .getOrElse(Array()) ++
                    (if (haveTypes) Array[CompletionItemLink]()
                     else {
                       haveTypes = true
                       module
                         .map(_.matchTypeName(terminatedContent._3, offset))
                         .getOrElse(Array())
                     })
                } else {
                  emptyCompletions
                }
              })
              .getOrElse(emptyCompletions)
          case ApexParser.RULE_creator =>
            module
              .map(m => m.matchTdsForModule(terminatedContent._3, offset))
              .map(_.flatMap(td => getAllCreatorCompletionItems(td, classDetails._2)))
              .getOrElse(emptyCompletions)

          case _ => emptyCompletions
        }
      }
      .toArray

    (if (creatorCompletions.nonEmpty)
       creatorCompletions
     else
       keywords.filterNot(_.label == "?.")) ++ dotCompletions ++ rules
  }

  private def findTokenAndIndex(
    parser: ApexParser,
    line: Int,
    offset: Int,
    dotRemoved: Boolean
  ): (Token, Int) = {
    val tokenStream = parser.getInputStream
    tokenStream.seek(0)
    var i     = 0
    var token = tokenStream.get(i)
    while (token.getType != -1 && !tokenContains(token, line, offset)) {
      i += 1
      token = tokenStream.get(i)
    }

    // Adjust cursor, see c3 README for details of cursor positioning
    val idx =
      if (dotRemoved || (i > 1 && tokenStream.get(i - 2).getText == ".")) {
        i
      } else {
        Math.max(0, i - 1)
      }
    (tokenStream.get(idx), idx)
  }

  private def tokenContains(token: Token, line: Int, offset: Int): Boolean = {
    token.getLine == line &&
    token.getCharPositionInLine <= offset &&
    token.getCharPositionInLine + token.getText.length > offset
  }

  private def stripQuotes(keyword: String): String = {
    if (keyword.length > 2)
      keyword.substring(1, keyword.length - 1)
    else
      keyword
  }

  private def injectStatementTerminator(
    line: Int,
    offset: Int,
    content: String
  ): (String, Int, String) = {
    val lines          = content.splitLines
    val result         = new mutable.StringBuilder()
    var adjustedOffset = offset
    var activeLine     = ""

    for (i <- lines.indices) {
      val currentLine = lines(i)
      if (i == line - 1) {
        activeLine = currentLine.substring(0, offset)
        result.append(activeLine)
        // Erase trailing dot so we have a legal expression that ANTLR will construct
        if (result.last == '.') {
          result.deleteCharAt(result.length() - 1)
          adjustedOffset -= 1
        }
        appendTerminators(result)
        result.append(currentLine.substring(offset))
        result.append('\n')
      } else {
        result.append(currentLine)
        result.append('\n')
      }
    }
    (result.toString(), adjustedOffset, activeLine)
  }

  private def appendTerminators(buffer: mutable.StringBuilder): Unit = {
    def appendStack(buffer: mutable.StringBuilder, stack: List[Char]): Unit = {
      stack match {
        case '(' :: tl =>
          appendStack(buffer, tl)
          buffer.append(')')
        case '[' :: tl =>
          appendStack(buffer, tl)
          buffer.append(']')
        case _ => ()
      }
    }

    var at                = buffer.length - 1
    var stack: List[Char] = Nil
    while (at > 0 && buffer.charAt(at) != '{' && buffer.charAt(at) != ';') {
      val char = buffer(at)
      if (char == ')' || char == ']')
        stack = char :: stack
      else if (char == '(') {
        if (stack.headOption.contains(')'))
          stack = stack.tail
        else
          stack = char :: stack
      } else if (char == '[') {
        if (stack.headOption.contains(']'))
          stack = stack.tail
        else
          stack = char :: stack
      }
      at -= 1
    }
    appendStack(buffer, stack)
    buffer.append(";")
  }

  /** Return a list of completion items in targetDeclaration that can be accessed from fromDeclaration.
    * The fromDeclaration is optional as we may not be able to construct due to parsing errors.
    */
  private def getAllCompletionItems(
    fromDeclaration: Option[TypeDeclaration],
    targetDeclaration: TypeDeclaration,
    isStatic: Option[Boolean],
    filterBy: String
  ): Array[CompletionItemLink] = {
    var items = Array[CompletionItemLink]()

    val minimumVisibility =
      if (fromDeclaration.contains(targetDeclaration))
        PRIVATE_MODIFIER.order
      else if (fromDeclaration.exists(_.extendsOrImplements(targetDeclaration.typeName)))
        PROTECTED_MODIFIER.order
      else PUBLIC_MODIFIER.order

    items = items ++ targetDeclaration.methods
      .filter(isStatic.isEmpty || _.isStatic == isStatic.get)
      .filter(_.visibility.map(_.order).getOrElse(0) >= minimumVisibility)
      .map(method => CompletionItemLink(method))

    items = items ++ targetDeclaration.fields
      .filter(isStatic.isEmpty || _.isStatic == isStatic.get)
      .filter(_.visibility.map(_.order).getOrElse(0) >= minimumVisibility)
      .map(field => CompletionItemLink(field))

    if (isStatic.isEmpty || isStatic.contains(true)) {
      items = items ++ targetDeclaration.nestedTypes
        .filter(_.visibility.map(_.order).getOrElse(0) >= minimumVisibility)
        .flatMap(nested => CompletionItemLink(nested))
    }
    if (isStatic.isEmpty) {
      val superConstructors = targetDeclaration.superClassDeclaration
        .map(superClass => {
          superClass.constructors
            .filter(constructor =>
              ConstructorMap.isCtorAccessible(
                constructor,
                targetDeclaration,
                targetDeclaration.superClassDeclaration
              )
            )
            .map(constructor =>
              (
                "super(" + constructor.parameters.map(_.name.toString()).mkString(", ") + ")",
                constructor.toString
              )
            )
            .map(labelDetail => CompletionItemLink(labelDetail._1, "Constructor", labelDetail._2))
            .toArray
        })
        .getOrElse(emptyCompletions)

      val thisConstructors = targetDeclaration.constructors
        .filter(constructor =>
          ConstructorMap.isCtorAccessible(
            constructor,
            targetDeclaration,
            targetDeclaration.superClassDeclaration
          )
        )
        .map(constructor =>
          (
            "this(" + constructor.parameters.map(_.name.toString()).mkString(", ") + ")",
            constructor.toString
          )
        )
        .map(labelDetail => CompletionItemLink(labelDetail._1, "Constructor", labelDetail._2))

      items = items ++ thisConstructors ++ superConstructors
    }

    if (filterBy.nonEmpty)
      items.filter(x => x.label.take(1).toLowerCase == filterBy.take(1).toLowerCase)
    else
      items
  }

  private def getAllCreatorCompletionItems(
    itemsFor: ApexClassDeclaration,
    callingFrom: Option[ApexFullDeclaration]
  ): Array[CompletionItemLink] = {
    callingFrom.map(td => (td, td.superClassDeclaration)) match {
      case Some((thisType, superType)) =>
        itemsFor.constructors
          .filter(constructor => ConstructorMap.isCtorAccessible(constructor, thisType, superType))
          .map(constructor => CompletionItemLink(itemsFor.name, constructor))
          .toArray
      case None => emptyCompletions
    }
  }

  private def getEmptyCreatorCompletionItems(
    td: ApexFullDeclaration,
    line: String
  ): Array[CompletionItemLink] = {
    // Strip 'id = n' from '... typeName id = new '
    val trimmed = idAssignPattern.replaceFirstIn(line, "")
    if (trimmed == line)
      return emptyCompletions

    // Map valid typeName to creation completion
    findTrailingTypeName(trimmed)
      .flatMap(typeName => {
        TypeResolver(typeName, td).toOption
          .filter(td => td.isSObject || td.constructors.exists(_.parameters.isEmpty))
          .map(_ => new CompletionItemLink(s"new $typeName();", "Constructor", ""))
      })
      .toArray
  }

  private def findTrailingTypeName(text: String): Option[TypeName] = {
    if (text.isEmpty)
      None
    else
      TypeName(text).toOption.orElse(findTrailingTypeName(text.substring(1)))
  }
}

object CompletionProvider {
  /* This limits how many states can be traversed during code completion, it provides a safeguard against run away
   * analysis but needs to be large enough for long files. */
  private final val MAX_STATES: Int = 10000000

  /* Match trailing 'id = n' as part of field/var creator pattern */
  private final val idAssignPattern: Regex = "\\s*[0-9a-zA-Z_]+\\s*=\\s*n\\s*$".r

  private final val emptyCompletions: Array[CompletionItemLink] = Array[CompletionItemLink]()

  final val ignoredTokens: Set[Integer] = Set[Integer](
    ApexLexer.LPAREN,
    ApexLexer.RPAREN,
    ApexLexer.LBRACE,
    ApexLexer.LBRACE,
    ApexLexer.RBRACE,
    ApexLexer.LBRACK,
    ApexLexer.RBRACK,
    ApexLexer.SEMI,
    ApexLexer.COMMA,
    ApexLexer.DOT,
    ApexLexer.ASSIGN,
    ApexLexer.GT,
    ApexLexer.LT,
    ApexLexer.BANG,
    ApexLexer.TILDE,
    /* ApexLexer.QUESTIONDOT, - Needed for dotCompletion handling */
    ApexLexer.QUESTION,
    ApexLexer.COLON,
    ApexLexer.EQUAL,
    ApexLexer.TRIPLEEQUAL,
    ApexLexer.NOTEQUAL,
    ApexLexer.LESSANDGREATER,
    ApexLexer.TRIPLENOTEQUAL,
    ApexLexer.AND,
    ApexLexer.OR,
    ApexLexer.COAL,
    ApexLexer.INC,
    ApexLexer.DEC,
    ApexLexer.ADD,
    ApexLexer.SUB,
    ApexLexer.MUL,
    ApexLexer.DIV,
    ApexLexer.BITAND,
    ApexLexer.BITOR,
    ApexLexer.CARET,
    ApexLexer.MAPTO,
    ApexLexer.ADD_ASSIGN,
    ApexLexer.SUB_ASSIGN,
    ApexLexer.MUL_ASSIGN,
    ApexLexer.DIV_ASSIGN,
    ApexLexer.AND_ASSIGN,
    ApexLexer.OR_ASSIGN,
    ApexLexer.XOR_ASSIGN,
    ApexLexer.LSHIFT_ASSIGN,
    ApexLexer.RSHIFT_ASSIGN,
    ApexLexer.URSHIFT_ASSIGN,
    ApexLexer.ATSIGN,
    ApexLexer.INSTANCEOF
  )
  final val preferredRules: Set[Integer] =
    Set[Integer](ApexParser.RULE_typeRef, ApexParser.RULE_primary, ApexParser.RULE_creator)
}
