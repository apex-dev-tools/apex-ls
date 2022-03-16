/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser

import apex.jorje.data.ast
import apex.jorje.data.ast.TypeRefs.ArrayTypeRef
import apex.jorje.semantic.ast.AstNode
import apex.jorje.semantic.ast.compilation.{AnonymousClass, _}
import apex.jorje.semantic.ast.member.{Method, Parameter}
import apex.jorje.semantic.ast.modifier.ModifierGroup
import apex.jorje.semantic.ast.statement.BlockStatement
import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor, Scope}
import apex.jorje.semantic.compiler.{ApexCompiler, CodeUnit, SourceFile}
import apex.jorje.semantic.compiler.parser.ParserEngine
import apex.jorje.semantic.exception.Errors
import apex.jorje.semantic.symbol.`type`.TypeInfo
import apex.jorje.semantic.symbol.member.Member
import apex.jorje.semantic.symbol.member.method.MethodInfo
import apex.jorje.semantic.symbol.member.variable.FieldInfo
import com.financialforce.oparser
import com.financialforce.oparser.{
  Annotation,
  ArraySubscripts,
  ClassTypeDeclaration,
  ConstructorDeclaration,
  EnumTypeDeclaration,
  FieldDeclaration,
  FormalParameter,
  FormalParameterList,
  Id,
  IdToken,
  Initializer,
  InterfaceTypeDeclaration,
  Location,
  MethodDeclaration,
  Modifier,
  PropertyDeclaration,
  QualifiedName,
  TypeArguments,
  TypeDeclaration,
  TypeList,
  TypeName,
  TypeRef
}
import org.apache.commons.lang3.reflect.FieldUtils

import java.util
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.OptionConverters.RichOptional
import scala.language.postfixOps
import scala.util.Try

class SFParser(source: Map[String, String]) {
  private val typeDeclarations: ArrayBuffer[TypeDeclaration] = ArrayBuffer()
  private val parseFailures: ArrayBuffer[String]             = ArrayBuffer()

  def parse: (ArrayBuffer[TypeDeclaration], ArrayBuffer[String]) = {
    parse()
  }

  def parseBlock(): (ArrayBuffer[TypeDeclaration], ArrayBuffer[String]) = {
    parse(ParserEngine.Type.ANONYMOUS)
  }

  private def parse(
    parserEngineType: ParserEngine.Type = ParserEngine.Type.NAMED
  ): (ArrayBuffer[TypeDeclaration], ArrayBuffer[String]) = {
    val (_, cu) =
      CompilerService.visitAstFromString(toSourceFiles, parserEngineType)
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

  private def getTypeDeclaration(path: String, cu: List[CodeUnit]): Option[TypeDeclaration] = {
    cu.find(_.getSourceFile.getKnownName == path) match {
      case Some(cu) =>
        if (cu != null) {
          getTypeDeclaration(cu.getNode, path)
        } else {
          throw new RuntimeException(s"No code unit found for type $path")
        }
      case _ => None
    }
  }

  private def getTypeDeclaration(root: Compilation, path: String): Option[TypeDeclaration] = {
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
        val ctd = constructClassTypeDeclaration(path, typeInfo, getMembers[UserClassMembers](root))
        return Some(ctd)
      case `userInterface` =>
        val itd =
          constructInterfaceTypeDeclaration(path, typeInfo, getMembers[UserInterfaceMembers](root))
        return Some(itd)
      case `userEnum` =>
        val etd = constructEnumTypeDeclaration(path, typeInfo)
        return Some(etd)
      case _ =>
    }
    None
  }

  private def getInitBlocks(members: UserClassMembers) = {
    def toInitializer(x: AstNode, isStatic: Boolean) = {
      val init = Initializer(isStatic)
      init.location = Some(toLoc(x.getLoc, 0, 0))
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
    typeInfo: TypeInfo
  ): EnumTypeDeclaration = {
    val etd                     = new EnumTypeDeclaration(path, None)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)
    val constants               = constructFieldDeclarations(typeInfo).map(_.id)

    etd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    modifiersAndAnnotations._1.foreach(etd.add)
    modifiersAndAnnotations._2.foreach(etd.add)
    constants.foreach(etd.constants.append)
    etd
  }

  private def constructInterfaceTypeDeclaration(
    path: String,
    typeInfo: TypeInfo,
    members: UserInterfaceMembers
  ): InterfaceTypeDeclaration = {
    val itd = getInterfaceTypeDeclaration(path, typeInfo)
    //The parser returns abstract and access modifiers for each method
    // so we can remove them for interface declarations
    val methods = constructMethodDeclarationForInterface(members)
    methods.foreach(x => {
      x.modifiers.clear()
      itd.add(x)
    })
    itd
  }

  private def getInterfaceTypeDeclaration(
    path: String,
    typeInfo: TypeInfo
  ): InterfaceTypeDeclaration = {
    val itd                     = new InterfaceTypeDeclaration(path, None)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)

    itd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    //We don't want to treat the interface keyword as a modifier for InterfaceTypeDeclaration
    modifiersAndAnnotations._1.filterNot(_.text.equalsIgnoreCase("interface")).foreach(itd.add)
    modifiersAndAnnotations._2.foreach(itd.add)
    itd.extendsTypeList = constructInterfaceTypeList(typeInfo)
    itd
  }

  private def constructClassTypeDeclaration(
    path: String,
    typeInfo: TypeInfo,
    members: UserClassMembers
  ) = {
    val ctd = getClassTypesDeclaration(path, typeInfo)
    getInnerTypes(members).flatMap(x => getTypeDeclaration(x, path)).foreach(ctd.innerTypes.append)
    getInitBlocks(members).foreach(ctd.initializers.append)
    constructMethodDeclarationForClass(members).foreach(ctd.add)
    ctd
  }

  private def getClassTypesDeclaration(path: String, typeInfo: TypeInfo): ClassTypeDeclaration = {
    val ctd = new ClassTypeDeclaration(path, None)

    val constructors            = constructConstructorDeclaration(typeInfo)
    val fields                  = constructFieldDeclarations(typeInfo)
    val properties              = constructPropertyDeclaration(typeInfo)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)

    ctd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    constructors.foreach(ctd.constructors.append)
    modifiersAndAnnotations._1.foreach(ctd.add)
    modifiersAndAnnotations._2.foreach(ctd.add)
    properties.foreach(ctd.properties.append)
    fields.foreach(ctd.fields.append)
    ctd.extendsTypeRef = constructExtendsTypeRef(typeInfo)
    ctd.implementsTypeList = constructInterfaceTypeList(typeInfo)
    ctd
  }

  private def constructInterfaceTypeList(typeInfo: TypeInfo): Option[TypeList] = {
    val interfaceRef =
      typeInfo.getCodeUnitDetails.getInterfaceTypeRefs.asScala.map(x => toTypeRef(Some(x)))
    val tl = new TypeList
    interfaceRef.flatten.foreach(tl.add)
    if (tl.typeRefs.nonEmpty) Some(tl) else None
  }

  private def constructExtendsTypeRef(typeInfo: TypeInfo): Option[TypeRef] = {
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
      .map(toMethodDeclaration)
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
      .map(toMethodDeclaration)
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
    new PropertyDeclaration(
      modifiersAndAnnotations._2,
      modifiersAndAnnotations._1,
      toTypeRef(from.getType),
      toId(from.getName, from.getLoc)
    )
  }

  private def toField(from: FieldInfo): FieldDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    val fd = FieldDeclaration(
      modifiersAndAnnotations._2,
      modifiersAndAnnotations._1,
      toTypeRef(from.getType),
      toId(from.getName, from.getLoc)
    )
    //TODO: figure out block location?
    fd
  }

  private def toMethodDeclaration(from: MethodInfo): MethodDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    MethodDeclaration(
      modifiersAndAnnotations._2,
      modifiersAndAnnotations._1,
      toTypeRef(from.getReturnType),
      toId(from.getCanonicalName, from.getLoc),
      toFormalParameterList(from.getParameters)
    )
    //TODO: figure out block location?
  }

  private def toConstructorDeclaration(from: MethodInfo): ConstructorDeclaration = {
    val modifiersAndAnnotations = toModifiersAndAnnotations(from.getModifiers)
    ConstructorDeclaration(
      modifiersAndAnnotations._2,
      modifiersAndAnnotations._1,
      toQName(from.getName, from.getLoc),
      toFormalParameterList(from.getParameters)
    )
    //TODO: figure out block location?
  }

  private def toQName(name: String, loc: apex.jorje.data.Location): QualifiedName = {
    val qName = new QualifiedName()
    qName.add(toId(name, loc))
    qName
  }

  private def toFormalParameterList(pl: java.util.List[Parameter]): FormalParameterList = {
    val fpl = new FormalParameterList
    pl.asScala.foreach(p => fpl.add(toFormalParameter(p)))
    fpl
  }

  private def toFormalParameter(p: Parameter): FormalParameter = {
    val aAndM = toModifiersAndAnnotations(p.getModifiers.getModifiers)
    val fp    = new FormalParameter()
    fp.add(toId(p.getName.getValue, p.getLoc))
    toTypeRef(Some(p.getTypeRef)).foreach(fp.add)
    aAndM._1.foreach(fp.add)
    aAndM._2.foreach(fp.add)
    fp
  }

  private def toModifiersAndAnnotations(
    from: ModifierGroup
  ): (ArrayBuffer[Modifier], ArrayBuffer[Annotation]) = {
    val builder = from.copy()
    val modifiers = builder.getModifiers.asScala
      .filterNot(m => m.getModifierType.getApexName == "explicitStatementExecuted")
      .map(
        m =>
          Modifier(toIdToken(correctSharingNameIfRequired(m.getModifierType.getApexName), m.getLoc))
      )
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
    Annotation(toQName(from.getType.getApexName, from.getLoc), None)
  }

  private def toLoc(
    from: apex.jorje.data.Location,
    endLine: Int,
    endLineOffset: Int
  ): com.financialforce.oparser.Location = {
    new Location(from.getLine, from.getColumn, 0, endLine, endLineOffset, 0)
  }

  private def toId(name: String, loc: apex.jorje.data.Location): Id = {
    Id(toIdToken(name, loc))
  }

  private def toIdToken(name: String, loc: apex.jorje.data.Location): IdToken = {
    IdToken(name, toLoc(loc, loc.getLine, loc.getColumn + name.length - 1))
  }

  private def toTypeRef(from: TypeInfo): com.financialforce.oparser.TypeRef = {
    val tr = new oparser.TypeRef
    //Apex name includes the fully qualified name with typeArguments. We dont need typeArguments for the name
    from.getApexName
      .replaceAll("<.*>", "")
      .split("\\.")
      .foreach(
        n =>
          tr.add(toTypeNameFromTypeInfo(from.getTypeArguments, n, from.getCodeUnitDetails.getLoc))
      )

    tr
  }

  private def toTypeRef(
    from: Option[apex.jorje.data.ast.TypeRef]
  ): Option[com.financialforce.oparser.TypeRef] = {
    from match {
      case Some(typ) =>
        val res = new oparser.TypeRef()
        //Things to note here,
        // if its a return type that has '[]' then parser will resolve '[]' to a list
        // but if its a in a typeArgument then it will resolve as ArrayTypeRef
        if (typ.isInstanceOf[ArrayTypeRef]) {
          //TODO: Resolve array subscripts properly
          //Temporary work around for comparison work as something like String[][] resolves
          // into deep nested typeRef with string type arguments
          typ.getNames.forEach(x => res.add(new TypeName(toId(x.getValue, x.getLoc))))
          for (_ <- 0 to typ.toString.split("\\[").length - 2) {
            res.add(ArraySubscripts())
          }
        } else {
          //We add the type arguments to the last type and not for each name
          val typArguments = typ.getTypeArguments.asScala
          typ.getNames.forEach(
            t =>
              res.add({
                toTypeNameFromTypeRef(new util.ArrayList[ast.TypeRef](), t.getValue, t.getLoc)
              })
          )
          if (typArguments.nonEmpty) {
            val ta = new TypeArguments
            val tl = toTypeList(typArguments.flatMap(x => toTypeRef(Some(x))))
            ta.add(tl)
            res.typeNames.last.typeArguments = Some(ta)
          }
        }

        return Some(res)
      case _ =>
    }
    None
  }

  private def toTypeNameFromTypeRef(
    typeArguments: java.util.List[apex.jorje.data.ast.TypeRef],
    name: String,
    location: apex.jorje.data.Location
  ) = {
    val tp = new TypeName(toId(name, location))
    val tl = toTypeList(typeArguments.asScala.flatMap(x => toTypeRef(Some(x))))
    if (tl.typeRefs.nonEmpty) {
      val typeArgument = new TypeArguments()
      typeArgument.typeList = Some(tl)
      tp.typeArguments = Some(typeArgument)
    }
    tp
  }

  private def toTypeNameFromTypeInfo(
    typeArguments: java.util.List[apex.jorje.semantic.symbol.`type`.TypeInfo],
    name: String,
    location: apex.jorje.data.Location
  ) = {
    val tp = new TypeName(toId(name, location))
    val tl = toTypeList(typeArguments.asScala.map(toTypeRef))
    if (tl.typeRefs.nonEmpty) {
      val typeArgument = new TypeArguments()
      typeArgument.typeList = Some(tl)
      tp.typeArguments = Some(typeArgument)
    }
    tp
  }

  private def toTypeList(typRefs: Iterable[TypeRef]) = {
    val tl = new TypeList
    typRefs.foreach(tl.add)
    tl
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
      true
    }

    override def visitEnd(node: BlockStatement, scope: AdditionalPassScope): Unit = {
      scope.pop(node)
      statements.append(node)
    }
  }

}

object SFParser {

  def apply(path: String, contents: String): SFParser = {
    new SFParser(Map(path -> contents))
  }

  def apply(sources: Map[String, String]): SFParser = {
    new SFParser(sources)
  }
}
