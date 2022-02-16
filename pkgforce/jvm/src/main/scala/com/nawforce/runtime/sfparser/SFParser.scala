package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.compilation.{AnonymousClass, _}
import apex.jorje.semantic.ast.member.Parameter
import apex.jorje.semantic.ast.modifier.ModifierGroup
import apex.jorje.semantic.ast.visitor.{AdditionalPassScope, AstVisitor}
import apex.jorje.semantic.compiler.parser.ParserEngine
import apex.jorje.semantic.symbol.`type`.TypeInfo
import apex.jorje.semantic.symbol.member.Member
import apex.jorje.semantic.symbol.member.method.MethodInfo
import apex.jorje.semantic.symbol.member.variable.FieldInfo
import com.financialforce.oparser
import com.financialforce.oparser.{
  Annotation,
  ClassTypeDeclaration,
  ConstructorDeclaration,
  FieldDeclaration,
  FormalParameter,
  FormalParameterList,
  Id,
  IdToken,
  Location,
  MethodDeclaration,
  Modifier,
  PropertyDeclaration,
  QualifiedName,
  TypeDeclaration,
  TypeList,
  TypeName
}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.parsers.{Source, SourceData}
import org.apache.commons.lang3.reflect.FieldUtils

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.OptionConverters.RichOptional

class SFParser(val source: Source) {
  private val visitor                                  = new TopLevelVisitor()
  private var typeDeclaration: Option[TypeDeclaration] = None

  def parseClass(): Option[TypeDeclaration] = {
    parse()
  }

  def parseBlock(): Option[TypeDeclaration] = {
    parse(ParserEngine.Type.ANONYMOUS)
  }

  private def parse(
    parserEngineType: ParserEngine.Type = ParserEngine.Type.NAMED
  ): Option[TypeDeclaration] = {
    CompilerService.visitAstFromString(source.code.asString, visitor, parserEngineType)
    parserTypeDeclaration()
    typeDeclaration
  }

  private def parserTypeDeclaration(): Unit = {
    visitor.getTopLevel match {
      case Some(value) => {
        val ctd = getTypeDeclaration(value, source.path.basename)
        typeDeclaration = ctd
      }
      case _ =>
    }
  }

  private def getTypeDeclaration(root: Compilation, path: String): Option[TypeDeclaration] = {

    //TODO: figure out ctd location? Check if we can use root.bodyLoc
    val userClass = classOf[UserClass]
    root.getClass match {
      case `userClass` => {
        val typeInfo = root.getDefiningType
        val ctd      = getClasTypesDeclaration(path, typeInfo)
        getInnerTypes(root).flatMap(x => getTypeDeclaration(x, path)).foreach(ctd.innerTypes.append)
        return Some(ctd)
      }
    }
    None
  }

  private def getInnerTypes(root: Compilation): Array[Compilation] = {
    //This is seems very hacky but i haven't found anything that exposes the inner types easily yet
    FieldUtils
      .readDeclaredField(root, "members", true)
      .asInstanceOf[UserClassMembers]
      .getInnerTypes
      .asScala
      .toArray
  }

  private def getClasTypesDeclaration(path: String, typeInfo: TypeInfo): ClassTypeDeclaration = {
    val ctd = new ClassTypeDeclaration(path)
    //TODO: annotations, initialization block

    val constructors =
      getUserDefinedMethods(typeInfo.methods().getConstructors).map(toConstructorDeclaration)

    val methods = getUserDefinedMethods(typeInfo.methods.getStaticsAndInstance)
      .filter(x => x.getGenerated.isUserDefined)
      .map(toMethodDeclaration)

    val fields =
      typeInfo.fields
        .all()
        .asScala
        .filter(f => f.getMemberType == Member.Type.FIELD)
        .map(toField)

    val properties =
      typeInfo
        .fields()
        .all()
        .asScala
        .filter(f => f.getMemberType == Member.Type.PROPERTY)
        .map(toProperties)
    val modifiers = toModifiers(typeInfo.getModifiers)

    //Since SuperTypes and Interface wont resolve in compile stage we can use the typeRef instead
    val superType = toTypeRef(typeInfo.getCodeUnitDetails.getSuperTypeRef.toScala)
    val interfaceRef = {
      typeInfo.getCodeUnitDetails.getInterfaceTypeRefs.asScala.map(x => toTypeRef(Some(x)))
    }
    val tl                            = new TypeList
    var interfaceTl: Option[TypeList] = None
    if (interfaceRef.nonEmpty) {
      interfaceRef.flatten.foreach(tl.add)
      interfaceTl = Some(tl)
    }

    ctd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    constructors.foreach(ctd.constructors.append)
    modifiers.foreach(ctd.add)
    methods.foreach(ctd.add)
    properties.foreach(ctd.properties.append)
    fields.foreach(ctd.fields.append)
    ctd.extendsTypeRef = superType
    ctd.implementsTypeList = interfaceTl
    ctd
  }

  private def getUserDefinedMethods(from: java.util.Collection[MethodInfo]): Array[MethodInfo] = {
    from.asScala.filter(_.getGenerated.isUserDefined).toArray
  }

  private def toProperties(from: FieldInfo): PropertyDeclaration = {
    new PropertyDeclaration(
      toAnnotations().to(ArrayBuffer),
      toModifiers(from.getModifiers).to(ArrayBuffer),
      toTypeRef(from.getType),
      toId(from.getName, from.getLoc)
    )
  }

  private def toField(from: FieldInfo): FieldDeclaration = {
    val fd = FieldDeclaration(
      toAnnotations().to(ArrayBuffer),
      toModifiers(from.getModifiers).to(ArrayBuffer),
      toTypeRef(from.getType),
      toId(from.getName, from.getLoc)
    )
    //TODO: figure out block location?
    fd
  }

  private def toMethodDeclaration(from: MethodInfo): MethodDeclaration = {
    MethodDeclaration(
      toAnnotations().to(ArrayBuffer),
      toModifiers(from.getModifiers).to(ArrayBuffer),
      toTypeRef(from.getReturnType),
      toId(from.getCanonicalName, from.getLoc),
      toFormalParameterList(from.getParameters)
    )
    //TODO: figure out block location?
  }

  private def toConstructorDeclaration(from: MethodInfo): ConstructorDeclaration = {
    ConstructorDeclaration(
      toAnnotations().to(ArrayBuffer),
      toModifiers(from.getModifiers).to(ArrayBuffer),
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
    //TODO: handle parameter annotations here?
    val fp = new FormalParameter()
    fp.add(toId(p.getName.getValue, p.getLoc))
    toTypeRef(Some(p.getTypeRef)).foreach(fp.add)
    toModifiers(p.getModifiers.getModifiers).foreach(fp.add)
    fp
  }

  private def toModifiers(from: ModifierGroup): Array[Modifier] = {
    from
      .all()
      .asScala
      .filter(mod => mod.getApexName != "explicitStatementExecuted")
      .map(x => Modifier(toIdToken(x.getApexName, from.getLoc)))
      .toArray
  }

  private def toAnnotations(): Array[Annotation] = {
    //TODO
    Array.empty
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
    tr.add(new TypeName(toId(from.getCodeUnitDetails.getName, from.getCodeUnitDetails.getLoc)))
    tr
  }

  private def toTypeRef(
    from: Option[apex.jorje.data.ast.TypeRef]
  ): Option[com.financialforce.oparser.TypeRef] = {
    from match {
      case Some(typ) => {
        val res = new oparser.TypeRef()
        typ.getNames.forEach(t => res.add(new TypeName(toId(t.getValue, t.getLoc))))
        return Some(res)
      }
      case _ =>
    }
    None
  }

  private class TopLevelVisitor extends AstVisitor[AdditionalPassScope] {
    private var topLevel: Option[Compilation] = None

    def getTopLevel: Option[Compilation] = topLevel

    override def visitEnd(node: UserClass, scope: AdditionalPassScope): Unit = topLevel = Some(node)

    override def visitEnd(node: UserEnum, scope: AdditionalPassScope): Unit = topLevel = Some(node)

    override def visitEnd(node: UserInterface, scope: AdditionalPassScope): Unit =
      topLevel = Some(node)

    override def visitEnd(node: UserTrigger, scope: AdditionalPassScope): Unit =
      topLevel = Some(node)

    override def visitEnd(node: AnonymousClass, scope: AdditionalPassScope): Unit =
      topLevel = Some(node)
  }
}

object SFParser {

  def apply(path: PathLike, code: SourceData): SFParser = {
    new SFParser(Source(path, code, 0, 0, None))
  }
}
