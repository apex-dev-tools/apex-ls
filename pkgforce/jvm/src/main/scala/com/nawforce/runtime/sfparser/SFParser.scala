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
  EnumTypeDeclaration,
  FieldDeclaration,
  FormalParameter,
  FormalParameterList,
  Id,
  IdToken,
  InterfaceTypeDeclaration,
  Location,
  MethodDeclaration,
  Modifier,
  PropertyDeclaration,
  QualifiedName,
  TypeDeclaration,
  TypeList,
  TypeName,
  TypeRef
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

  def parse: Option[TypeDeclaration] = {
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

    val userClass     = classOf[UserClass]
    val userInterface = classOf[UserInterface]
    val userEnum      = classOf[UserEnum]
    val typeInfo      = root.getDefiningType
    //TODO: figure out td location? Check if we can use root.bodyLoc
    root.getClass match {
      case `userClass` =>
        val ctd = getClasTypesDeclaration(path, typeInfo)
        getInnerTypes(root).flatMap(x => getTypeDeclaration(x, path)).foreach(ctd.innerTypes.append)
        return Some(ctd)
      case `userInterface` =>
        val itd = getInterfaceDeclaration(path, typeInfo)
        return Some(itd)
      case `userEnum` =>
        val etd = getEnumTypeDeclaration(path, typeInfo)
        return Some(etd)
      case _ =>
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

  private def getEnumTypeDeclaration(path: String, typeInfo: TypeInfo): EnumTypeDeclaration = {
    val etd = new EnumTypeDeclaration(path)
    //TODO: annotations
    val annotation = toAnnotations()
    val modifiers  = toModifiers(typeInfo.getModifiers)
    val constants  = constructFieldDeclarations(typeInfo).map(_.id)

    etd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    modifiers.foreach(etd.add)
    annotation.foreach(etd.add)
    constants.foreach(etd.constants.append)
    etd
  }

  private def getInterfaceDeclaration(
    path: String,
    typeInfo: TypeInfo
  ): InterfaceTypeDeclaration = {
    val itd = new InterfaceTypeDeclaration(path)
    //TODO: annotations
    val annotation = toAnnotations()
    val methods    = constructMethodDeclaration(typeInfo)
    val modifiers  = toModifiers(typeInfo.getModifiers)

    itd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    //We don't want to treat the interface keyword as a modifier for InterfaceTypeDeclaration
    modifiers.filterNot(_.text.equalsIgnoreCase("interface")).foreach(itd.add)
    //The parser returns access and the abstract modifier for each method
    // so we can remove them for interface declarations
    methods
      .map(x => {
        x.modifiers.clear()
        x
      })
      .foreach(itd.add)
    annotation.foreach(itd.add)
    itd.extendsTypeList = constructInterfaceTypeList(typeInfo)
    itd
  }

  private def getClasTypesDeclaration(path: String, typeInfo: TypeInfo): ClassTypeDeclaration = {
    val ctd = new ClassTypeDeclaration(path)
    //TODO: annotations, initialization block

    val constructors = constructConstructorDeclaration(typeInfo)
    val methods      = constructMethodDeclaration(typeInfo)
    val annotation   = toAnnotations()
    val fields       = constructFieldDeclarations(typeInfo)
    val properties   = constructPropertyDeclaration(typeInfo)
    val modifiers    = toModifiers(typeInfo.getModifiers)

    ctd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    constructors.foreach(ctd.constructors.append)
    modifiers.foreach(ctd.add)
    methods.foreach(ctd.add)
    annotation.foreach(ctd.add)
    properties.foreach(ctd.properties.append)
    fields.foreach(ctd.fields.append)
    ctd.extendsTypeRef = constructExtendsTypeRef(typeInfo)
    ctd.implementsTypeList = constructInterfaceTypeList(typeInfo)
    ctd
  }

  private def constructInterfaceTypeList(typeInfo: TypeInfo): Option[TypeList] = {
    //Since SuperTypes and Interface wont resolve in compile stage we can use the typeRef instead

    val interfaceRef =
      typeInfo.getCodeUnitDetails.getInterfaceTypeRefs.asScala.map(x => toTypeRef(Some(x)))

    val tl                            = new TypeList
    var interfaceTl: Option[TypeList] = None
    if (interfaceRef.nonEmpty) {
      interfaceRef.flatten.foreach(tl.add)
      interfaceTl = Some(tl)
    }
    interfaceTl
  }

  private def constructExtendsTypeRef(typeInfo: TypeInfo): Option[TypeRef] = {
    //Since SuperTypes and Interface wont resolve in compile stage we can use the typeRef instead
    toTypeRef(typeInfo.getCodeUnitDetails.getSuperTypeRef.toScala)
  }

  private def constructPropertyDeclaration(typeInfo: TypeInfo): Iterable[PropertyDeclaration] = {
    typeInfo
      .fields()
      .all()
      .asScala
      .filter(f => f.getMemberType == Member.Type.PROPERTY)
      .map(toProperties)
  }

  private def constructFieldDeclarations(typeInfo: TypeInfo): Iterable[FieldDeclaration] = {
    typeInfo.fields
      .all()
      .asScala
      .filter(f => f.getMemberType == Member.Type.FIELD)
      .map(toField)
  }

  private def constructMethodDeclaration(typeInfo: TypeInfo): Array[MethodDeclaration] = {
    getUserDefinedMethods(typeInfo.methods.getStaticsAndInstance)
      .map(toMethodDeclaration)
  }

  private def constructConstructorDeclaration(typeInfo: TypeInfo): Array[ConstructorDeclaration] = {
    getUserDefinedMethods(typeInfo.methods().getConstructors).map(toConstructorDeclaration)
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
      .filterNot(mod => mod.getApexName == "explicitStatementExecuted")
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
