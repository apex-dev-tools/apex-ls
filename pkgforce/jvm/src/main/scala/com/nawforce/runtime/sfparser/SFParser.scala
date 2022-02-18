package com.nawforce.runtime.sfparser

import apex.jorje.semantic.ast.AstNode
import apex.jorje.semantic.ast.compilation.{AnonymousClass, _}
import apex.jorje.semantic.ast.member.Parameter
import apex.jorje.semantic.ast.modifier.ModifierGroup
import apex.jorje.semantic.ast.statement.BlockStatement
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
  Initializer,
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
        val td = getTypeDeclaration(value, source.path.basename)
        typeDeclaration = td
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
        val ctd = constructClassTypeDeclaration(path, root)
        return Some(ctd)
      case `userInterface` =>
        val itd = constructInterfaceTypeDeclaration(path, typeInfo)
        return Some(itd)
      case `userEnum` =>
        val etd = constructEnumTypeDeclaration(path, typeInfo)
        return Some(etd)
      case _ =>
    }
    None
  }

  private def getInitBlocks(root: Compilation) = {
    def toInitializer(x: AstNode, isStatic: Boolean) = {
      val init = Initializer(isStatic)
      init.location = Some(toLoc(x.getLoc, 0, 0))
      init
    }

    //This is quite hacky too
    val nonStaticBlocks = getClassMembers(root).getStatements.asScala
      .filter(_.isInstanceOf[BlockStatement])
      .map(toInitializer(_, isStatic = false))

    val staticBlocks = getClassMembers(root).getMethods.asScala
      .filter(_.getMethodInfo.getCanonicalName.equals("<clinit>"))
      .map(toInitializer(_, isStatic = true))

    nonStaticBlocks ++ staticBlocks
  }

  private def getInnerTypes(root: Compilation): Array[Compilation] = {
    getClassMembers(root).getInnerTypes.asScala.toArray
  }

  private def getClassMembers(root: Compilation): UserClassMembers = {
    //This is seems very hacky but its the easiest way to expose innerTypes and initializer blocks
    FieldUtils
      .readDeclaredField(root, "members", true)
      .asInstanceOf[UserClassMembers]
  }

  private def constructEnumTypeDeclaration(
    path: String,
    typeInfo: TypeInfo
  ): EnumTypeDeclaration = {
    val etd                     = new EnumTypeDeclaration(path)
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
    typeInfo: TypeInfo
  ): InterfaceTypeDeclaration = {
    val itd                     = new InterfaceTypeDeclaration(path)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)
    val methods                 = constructMethodDeclaration(typeInfo)

    itd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    //We don't want to treat the interface keyword as a modifier for InterfaceTypeDeclaration
    modifiersAndAnnotations._1.filterNot(_.text.equalsIgnoreCase("interface")).foreach(itd.add)
    modifiersAndAnnotations._2.foreach(itd.add)
    //The parser returns abstract and access modifiers for each method
    // so we can remove them for interface declarations
    methods.foreach(x => {
      x.modifiers.clear()
      itd.add(x)
    })
    itd.extendsTypeList = constructInterfaceTypeList(typeInfo)
    itd
  }

  private def constructClassTypeDeclaration(path: String, root: Compilation) = {
    val typeInfo = root.getDefiningType
    val ctd      = getClasTypesDeclaration(path, typeInfo)
    getInnerTypes(root).flatMap(x => getTypeDeclaration(x, path)).foreach(ctd.innerTypes.append)
    getInitBlocks(root).foreach(ctd.initializers.append)
    ctd
  }

  private def getClasTypesDeclaration(path: String, typeInfo: TypeInfo): ClassTypeDeclaration = {
    val ctd = new ClassTypeDeclaration(path)

    val constructors            = constructConstructorDeclaration(typeInfo)
    val methods                 = constructMethodDeclaration(typeInfo)
    val fields                  = constructFieldDeclarations(typeInfo)
    val properties              = constructPropertyDeclaration(typeInfo)
    val modifiersAndAnnotations = toModifiersAndAnnotations(typeInfo.getModifiers)

    ctd.add(toId(typeInfo.getCodeUnitDetails.getName, typeInfo.getCodeUnitDetails.getLoc))
    constructors.foreach(ctd.constructors.append)
    modifiersAndAnnotations._1.foreach(ctd.add)
    modifiersAndAnnotations._2.foreach(ctd.add)
    methods.foreach(ctd.add)
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

  private def constructMethodDeclaration(typeInfo: TypeInfo): Iterable[MethodDeclaration] = {
    typeInfo.methods.getStaticsAndInstance.asScala
      .filter(_.getGenerated.isUserDefined)
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
      .map(m => Modifier(toIdToken(m.getModifierType.getApexName, m.getLoc)))
      .to(ArrayBuffer)
    val annotations = builder.getAnnotations.asScala.map(toAnnotation).to(ArrayBuffer)
    (modifiers, annotations)
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
