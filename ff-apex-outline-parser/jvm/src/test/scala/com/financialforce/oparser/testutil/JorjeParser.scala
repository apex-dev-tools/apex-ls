/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser.testutil

import apex.jorje.data.ast.TypeRefs.ArrayTypeRef
import apex.jorje.semantic.ast.AstNode
import apex.jorje.semantic.ast.compilation._
import apex.jorje.semantic.ast.member.{Method, Parameter}
import apex.jorje.semantic.ast.modifier.ModifierGroup
import apex.jorje.semantic.ast.statement.BlockStatement
import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor}
import apex.jorje.semantic.compiler.parser.ParserEngine
import apex.jorje.semantic.compiler.sfdc.SymbolProvider
import apex.jorje.semantic.compiler.{CodeUnit, SourceFile}
import apex.jorje.semantic.exception.Errors
import apex.jorje.semantic.symbol.`type`.TypeInfo
import apex.jorje.semantic.symbol.member.Member
import apex.jorje.semantic.symbol.member.method.MethodInfo
import apex.jorje.semantic.symbol.member.variable.FieldInfo
import com.financialforce.oparser._
import com.financialforce.types.base.{
  Annotation,
  Location,
  Modifier,
  QualifiedName,
  TypeNameSegment,
  TypeRef,
  UnresolvedTypeRef
}
import org.apache.commons.lang3.reflect.FieldUtils

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.OptionConverters.RichOptional
import scala.language.postfixOps

class JorjeParser(source: Map[String, String]) {
  private val typeDeclarations: ArrayBuffer[TestTypeDeclaration] = ArrayBuffer()
  private val parseFailures: ArrayBuffer[String]                 = ArrayBuffer()

  def parse: (ArrayBuffer[TestTypeDeclaration], ArrayBuffer[String]) = {
    parse()
  }

  def parseBlock(): (ArrayBuffer[TestTypeDeclaration], ArrayBuffer[String]) = {
    parse(ParserEngine.Type.ANONYMOUS)
  }

  def parseClassWithSymbolProvider(
    symbolProvider: SymbolProvider
  ): (ArrayBuffer[TestTypeDeclaration], ArrayBuffer[String]) = {
    parse(ParserEngine.Type.NAMED, symbolProvider)
  }

  private def parse(
    parserEngineType: ParserEngine.Type = ParserEngine.Type.NAMED,
    symbolProvider: SymbolProvider = EmptySymbolProvider()
  ): (ArrayBuffer[TestTypeDeclaration], ArrayBuffer[String]) = {
    val (_, cu) =
      CompilerService.compile(toSourceFiles, parserEngineType, symbolProvider)
    source.keys.foreach(path => {
      getTypeDeclaration(path, cu) match {
        case Some(value) => typeDeclarations.append(value)
        case None        => parseFailures.append(path)
      }
    })
    (typeDeclarations, parseFailures)
  }

  private def toSourceFiles: List[SourceFile] = {
    source.map {
      case (path, contents) => SourceFile.builder().setKnownName(path).setBody(contents).build()
    }.toList
  }

  private def getTypeDeclaration(path: String, cu: List[CodeUnit]): Option[TestTypeDeclaration] = {
    cu.find(_.getSourceFile.getKnownName == path) match {
      case Some(cu) =>
        if (cu != null) {
          getTypeDeclaration(cu.getNode, path, enclosing = null)
        } else {
          throw new RuntimeException(s"No code unit found for type $path")
        }
      case _ => None
    }
  }

  private def getTypeDeclaration(
    root: Compilation,
    path: String,
    enclosing: IMutableTestTypeDeclaration
  ): Option[TestTypeDeclaration] = {
    def getMembers[T](root: Compilation): T = {
      //This is seems very hacky but its the easiest way to expose innerTypes and initializer blocks and unresolved methods
      FieldUtils
        .readDeclaredField(root, "members", true)
        .asInstanceOf[T]
    }

    val userClass     = classOf[UserClass]
    val userInterface = classOf[UserInterface]
    val userEnum      = classOf[UserEnum]
    val typeInfo      = root.getDefiningType
    //TODO: figure out td location? Check if we can use root.bodyLoc
    root.getClass match {
      case `userClass` =>
        val ctd = constructClassTypeDeclaration(
          path,
          typeInfo,
          getMembers[UserClassMembers](root),
          enclosing
        )
        return Some(ctd)
      case `userInterface` =>
        val itd =
          constructInterfaceTypeDeclaration(
            path,
            typeInfo,
            getMembers[UserInterfaceMembers](root),
            enclosing
          )
        return Some(itd)
      case `userEnum` =>
        val etd = constructEnumTypeDeclaration(path, typeInfo, enclosing)
        return Some(etd)
      case _ =>
    }
    None
  }

  private def getInitBlocks(members: UserClassMembers) = {
    def toInitializer(x: AstNode, isStatic: Boolean) = {
      val init = Initializer(isStatic)
      val loc  = toLoc(x.getLoc, 0, 0)
      init.setLocation(loc.startPosition, loc.endPosition)
      init
    }

    val block: ArrayBuffer[Initializer] = ArrayBuffer()
    members.getStatements.asScala
      .filter(_.isInstanceOf[BlockStatement])
      .map(toInitializer(_, isStatic = false))
      .foreach(block.append)

    val clinit =
      members.getMethods.asScala.find(_.getMethodInfo.getCanonicalName.equals("<clinit>"))

    clinit match {
      case Some(cli) =>
        val visitor = new BlockStatementVisitor()
        cli.traverse(visitor, new AdditionalPassScope(Errors.createErrors()))
        visitor.statements.foreach(s => block.append(toInitializer(s, isStatic = true)))
      case None =>
    }
    block
  }

  private def getInnerTypes(members: UserClassMembers): Array[Compilation] = {
    members.getInnerTypes.asScala.toArray
  }

  private def constructEnumTypeDeclaration(
    path: String,
    typeInfo: TypeInfo,
    enclosing: IMutableTestTypeDeclaration
  ): TestEnumTypeDeclaration = {
    val etd                     = new TestEnumTypeDeclaration(path, enclosing)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)
    val constants               = constructFieldDeclarations(typeInfo).map(_.id)

    etd.setId(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    etd.setModifiers(modifiersAndAnnotations._1.toArray)
    etd.setAnnotations(modifiersAndAnnotations._2.toArray)
    constants.foreach(
      id => etd.appendField(FieldDeclaration(Array(), Array(Modifier("static")), etd, id))
    )
    etd
  }

  private def constructInterfaceTypeDeclaration(
    path: String,
    typeInfo: TypeInfo,
    members: UserInterfaceMembers,
    enclosing: IMutableTestTypeDeclaration
  ): TestInterfaceTypeDeclaration = {
    val itd = getInterfaceTypeDeclaration(path, typeInfo, enclosing)
    constructMethodDeclarationForInterface(members).foreach(itd.appendMethod)
    itd
  }

  private def getInterfaceTypeDeclaration(
    path: String,
    typeInfo: TypeInfo,
    enclosing: IMutableTestTypeDeclaration
  ): TestInterfaceTypeDeclaration = {
    val itd                     = new TestInterfaceTypeDeclaration(path, enclosing)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)

    itd.setId(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    // We don't want to treat the interface keyword as a modifier for InterfaceTypeDeclaration
    itd.setModifiers(
      modifiersAndAnnotations._1.filterNot(_.text.equalsIgnoreCase("interface")).toArray
    )
    itd.setAnnotations(modifiersAndAnnotations._2.toArray)
    itd._implementsTypeList = constructInterfaceTypeList(typeInfo).orNull
    itd
  }

  private def constructClassTypeDeclaration(
    path: String,
    typeInfo: TypeInfo,
    members: UserClassMembers,
    enclosing: IMutableTestTypeDeclaration
  ) = {
    val ctd = getClassTypesDeclaration(path, typeInfo, enclosing)
    getInnerTypes(members)
      .flatMap(x => getTypeDeclaration(x, path, ctd))
      .foreach(ctd._innerTypes.append)
    getInitBlocks(members).foreach(ctd._initializers.append)
    constructMethodDeclarationForClass(members).foreach(ctd.appendMethod)
    ctd
  }

  private def getClassTypesDeclaration(
    path: String,
    typeInfo: TypeInfo,
    enclosing: IMutableTestTypeDeclaration
  ): TestClassTypeDeclaration = {
    val ctd = new TestClassTypeDeclaration(path, enclosing)

    val constructors            = constructConstructorDeclaration(typeInfo)
    val fields                  = constructFieldDeclarations(typeInfo)
    val properties              = constructPropertyDeclaration(typeInfo)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)

    ctd.setId(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    constructors.foreach(ctd._constructors.append)
    ctd.setModifiers(modifiersAndAnnotations._1.toArray)
    ctd.setAnnotations(modifiersAndAnnotations._2.toArray)
    properties.foreach(ctd._properties.append)
    fields.foreach(ctd._fields.append)
    ctd._extendsTypeRef = constructExtendsTypeRef(typeInfo).orNull
    ctd._implementsTypeList = constructInterfaceTypeList(typeInfo).orNull
    ctd
  }

  private def constructInterfaceTypeList(typeInfo: TypeInfo): Option[ArraySeq[TypeRef]] = {
    val interfaceRef =
      typeInfo.getCodeUnitDetails.getInterfaceTypeRefs.asScala.map(x => toTypeRef(Some(x)))
    val tl = ArraySeq.unsafeWrapArray(interfaceRef.flatten.toArray)
    if (tl.nonEmpty) Some(tl) else None
  }

  private def constructExtendsTypeRef(typeInfo: TypeInfo): Option[UnresolvedTypeRef] = {
    toTypeRef(typeInfo.getCodeUnitDetails.getSuperTypeRef.toScala)
  }

  private def constructPropertyDeclaration(typeInfo: TypeInfo): Iterable[PropertyDeclaration] = {
    getFieldInfoByType(typeInfo, Member.Type.PROPERTY)
      .map(toProperties)
  }

  private def constructFieldDeclarations(typeInfo: TypeInfo): Iterable[FieldDeclaration] = {
    getFieldInfoByType(typeInfo, Member.Type.FIELD)
      .map(toField)
  }

  private def getFieldInfoByType(
    typeInfo: TypeInfo,
    fieldType: Member.Type
  ): Iterable[FieldInfo] = {
    typeInfo.fields
      .all()
      .asScala
      .filter(f => f.getMemberType == fieldType)
  }

  private def constructMethodDeclarationForClass(
    members: UserClassMembers
  ): Iterable[MethodDeclaration] = {
    /*
    We use the methods from the code unit class members here since methods with custom types will be
    unresolved and wont be available in typeInfo from the visitor.
     */
    members.getMethods.asScala
      .map(_.getMethodInfo)
      .filter(x => x.getGenerated.isUserDefined && !x.isConstructor)
      .map(m => toMethodDeclaration(m))
  }

  private def constructMethodDeclarationForInterface(
    members: UserInterfaceMembers
  ): Iterable[MethodDeclaration] = {
    /*
    We use the methods from the code unit class members here since methods with custom types will be
    unresolved and wont be available in typeInfo from the visitor.
     */
    members.getMethods.asScala
      .map(_.getMethodInfo)
      .filter(x => x.getGenerated.isUserDefined && !x.isConstructor)
      //The parser returns abstract and access modifiers for each method so we remove them for interface declarations
      .map(m => toMethodDeclaration(m, noModifiers = true))
  }

  private def constructConstructorDeclaration(
    typeInfo: TypeInfo
  ): Iterable[ConstructorDeclaration] = {
    typeInfo
      .methods()
      .getConstructors
      .asScala
      .filter(_.getGenerated.isUserDefined)
      .map(toConstructorDeclaration)
  }

  private def toProperties(from: FieldInfo): PropertyDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    PropertyDeclaration(
      modifiersAndAnnotations._2.toArray,
      modifiersAndAnnotations._1.toArray,
      toTypeRef(from.getType),
      Array(),
      toId(from.getName, from.getLoc)
    )
  }

  private def toField(from: FieldInfo): FieldDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    val fd = FieldDeclaration(
      modifiersAndAnnotations._2.toArray,
      modifiersAndAnnotations._1.toArray,
      toTypeRef(from.getType),
      toId(from.getName, from.getLoc)
    )
    //TODO: figure out block location?
    fd
  }

  private def toMethodDeclaration(
    from: MethodInfo,
    noModifiers: Boolean = false
  ): MethodDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    MethodDeclaration(
      modifiersAndAnnotations._2.toArray,
      if (noModifiers) Array.empty
      else modifiersAndAnnotations._1.toArray,
      Some(toTypeRef(from.getReturnType)),
      toId(from.getCanonicalName, from.getLoc),
      toFormalParameterList(from.getParameters)
    )
    //TODO: figure out block location?
  }

  private def toConstructorDeclaration(from: MethodInfo): ConstructorDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    ConstructorDeclaration(
      modifiersAndAnnotations._2.toArray,
      modifiersAndAnnotations._1.toArray,
      toQName(from.getName, from.getLoc),
      toFormalParameterList(from.getParameters)
    )
    //TODO: figure out block location?
  }

  private def toQName(name: String, loc: apex.jorje.data.Location): QualifiedName = {
    QualifiedName(Array(toId(name, loc)))
  }

  private def toFormalParameterList(pl: java.util.List[Parameter]): ArraySeq[FormalParameter] = {
    ArraySeq.unsafeWrapArray(pl.asScala.map(toFormalParameter).toArray)
  }

  private def toFormalParameter(p: Parameter): FormalParameter = {
    val aAndM = toModifiersAndAnnotations(p.getModifiers.getModifiers)
    FormalParameter(
      aAndM._2.toArray,
      aAndM._1.toArray,
      toTypeRef(Some(p.getTypeRef)).get,
      toId(p.getName.getValue, p.getLoc)
    )
  }

  private def toModifiersAndAnnotations(
    from: ModifierGroup
  ): (ArrayBuffer[Modifier], ArrayBuffer[Annotation]) = {
    val builder = from.copy()
    val modifiers = builder.getModifiers.asScala
      .filterNot(m => m.getModifierType.getApexName == "explicitStatementExecuted")
      .map(m => Modifier(correctSharingNameIfRequired(m.getModifierType.getApexName)))
      .to(ArrayBuffer)
    val annotations = builder.getAnnotations.asScala.map(toAnnotation).to(ArrayBuffer)
    (modifiers, annotations)
  }

  private def correctSharingNameIfRequired(name: String): String = {
    name match {
      case "withSharing"      => "with sharing"
      case "withoutSharing"   => "without sharing"
      case "inheritedSharing" => "inherited sharing"
      case _                  => name
    }
  }

  private def toAnnotation(from: apex.jorje.semantic.ast.modifier.Annotation): Annotation = {
    //TODO: add parameters
    Annotation(from.getType.getApexName, None)
  }

  private def toLoc(from: apex.jorje.data.Location, endLine: Int, endLineOffset: Int): Location = {
    new Location(from.getLine, from.getColumn, 0, endLine, endLineOffset, 0)
  }

  private def toId(name: String, loc: apex.jorje.data.Location): LocatableIdToken = {
    toIdToken(name, loc)
  }

  private def toIdToken(name: String, loc: apex.jorje.data.Location): LocatableIdToken = {
    LocatableIdToken(name, toLoc(loc, loc.getLine, loc.getColumn + name.length - 1))
  }

  private def toTypeRef(from: TypeInfo): UnresolvedTypeRef = {

    //Apex name includes the fully qualified name with typeArguments. We dont need typeArguments for the name
    val segments = mutable.ArrayBuffer[TypeNameSegment]()
    from.getApexName
      .replaceAll("<.*>", "")
      .split("\\.")
      .foreach(
        n =>
          segments.append(
            toTypeNameFromTypeInfo(from.getTypeArguments, n, from.getCodeUnitDetails.getLoc)
          )
      )

    UnresolvedTypeRef(segments.toArray, 0)
  }

  private def toTypeRef(from: Option[apex.jorje.data.ast.TypeRef]): Option[UnresolvedTypeRef] = {
    from match {
      case Some(typ) =>
        val segments   = mutable.ArrayBuffer[TypeNameSegment]()
        var subscripts = 0
        //Things to note here,
        // if its a return type that has '[]' then parser will resolve '[]' to a list
        // but if its a in a typeArgument then it will resolve as ArrayTypeRef
        if (typ.isInstanceOf[ArrayTypeRef]) {
          //TODO: Resolve array subscripts properly
          //Temporary work around for comparison work as something like String[][] resolves
          // into deep nested typeRef with string type arguments
          typ.getNames.forEach(
            x =>
              segments
                .append(new TypeNameSegment(toId(x.getValue, x.getLoc), TypeRef.emptyArraySeq))
          )
          for (_ <- 0 to typ.toString.split("\\[").length - 2) {
            subscripts += 1
          }
        } else {
          //We add the type arguments to the last type and not for each name
          val typArguments = typ.getTypeArguments.asScala
          val typeArguments =
            if (typArguments.nonEmpty)
              toTypeList(typArguments.flatMap(x => toTypeRef(Some(x))))
            else
              TypeRef.emptyArraySeq

          val last = typ.getNames.get(typ.getNames.size() - 1)
          typ.getNames.forEach(
            t =>
              segments.append(
                if (t eq last)
                  new TypeNameSegment(toId(t.getValue, t.getLoc), typeArguments)
                else
                  new TypeNameSegment(toId(t.getValue, t.getLoc), TypeRef.emptyArraySeq)
              )
          )
        }

        return Some(UnresolvedTypeRef(segments.toArray, subscripts))
      case _ =>
    }
    None
  }

  private def toTypeNameFromTypeInfo(
    typeArguments: java.util.List[apex.jorje.semantic.symbol.`type`.TypeInfo],
    name: String,
    location: apex.jorje.data.Location
  ): TypeNameSegment = {
    new TypeNameSegment(toId(name, location), toTypeList(typeArguments.asScala.map(toTypeRef)))
  }

  private def toTypeList(typeRefs: Iterable[UnresolvedTypeRef]): ArraySeq[TypeRef] = {
    ArraySeq.unsafeWrapArray(typeRefs.toArray)
  }

  private class BlockStatementVisitor extends AstVisitor[AdditionalPassScope] {
    val statements: ArrayBuffer[BlockStatement] = ArrayBuffer[BlockStatement]()

    override def visit(node: Method, scope: AdditionalPassScope): Boolean = {
      scope.push(node)
      true
    }

    override def visitEnd(node: Method, scope: AdditionalPassScope): Unit = {
      scope.pop(node)
    }

    override def visit(node: BlockStatement, scope: AdditionalPassScope): Boolean = {
      scope.push(node)
      false
    }

    override def visitEnd(node: BlockStatement, scope: AdditionalPassScope): Unit = {
      scope.pop(node)
      statements.append(node)
    }
  }

}

object JorjeParser {

  import java.util.logging.LogManager

  // Stop Jorje logging a startup message
  LogManager.getLogManager.reset()

  def apply(path: String, contents: String): JorjeParser = {
    new JorjeParser(Map(path -> contents))
  }

  def apply(sources: Map[String, String]): JorjeParser = {
    new JorjeParser(sources)
  }
}
