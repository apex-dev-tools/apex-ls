package com.nawforce.apexlink.opcst

import com.financialforce.oparser.{ClassTypeDeclaration => OPClassTypeDeclaration, ConstructorDeclaration => OPConstructorDeclaration, EnumTypeDeclaration => OPEnumTypeDeclaration, FieldDeclaration => OPFieldDeclaration, FormalParameter => OPFormalParameter, Id => OPId, Initializer => OPInitializer, InterfaceTypeDeclaration => OPInterfaceTypeDeclaration, Location => OPLocation, MethodDeclaration => OPMethodDeclaration, PropertyBlock => OPPropertyBlock, PropertyDeclaration => OPPropertyDeclaration}
import com.nawforce.apexlink.cst.{ApexConstructorDeclaration, ApexFieldDeclaration, ApexInitializerBlock, ApexMethodDeclaration, ApexPropertyDeclaration, CST, ClassBodyDeclaration, ClassDeclaration, EnumDeclaration, Expression, FormalParameter, Id, InterfaceDeclaration, LazyBlock, PropertyBlock, QualifiedName, VariableDeclarator}
import com.nawforce.apexlink.finding.{RelativeTypeContext, RelativeTypeName}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.org.Hierarchy
import com.nawforce.apexlink.types.apex.ThisType
import com.nawforce.apexlink.types.core.ParameterDeclaration
import com.nawforce.apexparser.ApexParser.BlockContext
import com.nawforce.pkgforce.modifiers.{MethodOwnerNature, ModifierResults}
import com.nawforce.pkgforce.names.{Names, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.parsers.{CodeParser, Source, SourceData}

import java.lang.ref.WeakReference
import scala.collection.immutable.ArraySeq
import scala.util.chaining.scalaUtilChainingOps

private[opcst] object OutlineParserId {

  def construct(src: Option[OPId], path: PathLike): Id = {
    construct(src.get, path)
  }

  def construct(src: OPId, path: PathLike): Id = {
    val id = Id(Names(src.id.contents))
    id.setLocation(
      path,
      src.id.location.startLine,
      src.id.location.startLineOffset - 1,
      src.id.location.endLine,
      src.id.location.endLineOffset
    )
    id
  }
}

private[opcst] object OutlineParserClassDeclaration {

  def construct(
    path: PathLike,
    ctd: OPClassTypeDeclaration,
    source: Source,
    thisType: ThisType,
    outerTypeName: Option[TypeName],
    modifierResults: ModifierResults,
    endLineOffset: Option[Int] = None
  ): ClassDeclaration = {

    val id = OutlineParserId.construct(ctd.id, source.path)
    val extendType =
      ctd.extendsTypeRef.map(TypeReference.construct).getOrElse(TypeNames.InternalObject)
    val implementsType =
      ctd.implementsTypeList.map(TypeList.construct).getOrElse(TypeNames.emptyTypeNames)

    val typeContext = new RelativeTypeContext

    val constructors = ctd.constructors.flatMap(
      c =>
        OutlineParserClassBodyDeclaration.constructConstructorDeclaration(
          path,
          c,
          source,
          typeContext,
          outerTypeName.isEmpty,
          thisType
        )
    )

    val methods = ctd.methods.flatMap(
      m =>
        OutlineParserClassBodyDeclaration.constructClassMethodDeclaration(
          path,
          m,
          source,
          typeContext,
          modifierResults.methodOwnerNature,
          outerTypeName.isEmpty,
          thisType
        )
    )

    val fields = ctd.fields.flatMap(
      f =>
        OutlineParserClassBodyDeclaration
          .constructFieldDeclaration(path, f, source, typeContext, outerTypeName.isEmpty, thisType)
    )

    val initializers = ctd.initializers.flatMap(
      i =>
        OutlineParserClassBodyDeclaration
          .constructInitializerBlock(i, source, typeContext, outerTypeName.isEmpty, thisType)
    )

    val properties = ctd.properties.flatMap(
      p =>
        OutlineParserClassBodyDeclaration.constructPropertyDeclaration(
          path,
          p,
          source,
          typeContext,
          outerTypeName.isEmpty,
          thisType
        )
    )

    val innerTypes = ctd.innerTypes.flatMap {
      case ic: OPClassTypeDeclaration => constructInner(path, ic, source, thisType)
      case ii: OPInterfaceTypeDeclaration =>
        OutlineParserInterfaceDeclaration.constructInner(path, ii, source, thisType)
      case ie: OPEnumTypeDeclaration =>
        OutlineParserEnumDeclaration.constructInner(path, ie, source, thisType)
      case _ => None
    }

    val bodyDeclarations = ArraySeq[
      ClassBodyDeclaration
    ]() ++ constructors ++ methods ++ fields ++ initializers ++ properties ++ innerTypes

    val declaration = ClassDeclaration(
      source,
      thisType.module,
      typeContext,
      thisType.typeName,
      outerTypeName,
      id,
      modifierResults,
      thisType.inTest,
      Some(extendType),
      implementsType,
      bodyDeclarations
    )
    LocationUtils.stampLocation(
      declaration,
      ctd.location.get.copy(
        startLineOffset = ctd.location.get.startLineOffset - 1,
        endLineOffset = ctd.location.get.endLineOffset + endLineOffset.getOrElse(0)
      ),
      source.path
    )
    typeContext.freeze(declaration)
    declaration
  }

  private def constructInner(
    path: PathLike,
    ic: OPClassTypeDeclaration,
    source: Source,
    outerType: ThisType
  ): Option[ClassDeclaration] = {

    val modifierResults =
      ModifierUtils.classModifiers(path, ic.id.get, ic.annotations, ic.modifiers, outer = false)
    val thisType = outerType.asInner(ic.id.get.id.contents)
    val rv = OutlineParserClassDeclaration.construct(
      path,
      ic,
      source,
      thisType,
      Some(outerType.typeName),
      modifierResults,
      Some(-1)
    )
    Some(rv)
  }
}

private[opcst] object OutlineParserInterfaceDeclaration {

  def constructInner(
    path: PathLike,
    ii: OPInterfaceTypeDeclaration,
    source: Source,
    outerType: ThisType
  ): Option[InterfaceDeclaration] = {

    val thisType = outerType.asInner(ii.id.get.id.contents)
    val modifierResults =
      ModifierUtils.interfaceModifiers(path, ii.id.get, ii.annotations, ii.modifiers, outer = false)
    val rv =
      construct(path, ii, source, thisType, Some(outerType.typeName), modifierResults, Some(-1))
    Some(rv)
  }

  def construct(
    path: PathLike,
    itd: OPInterfaceTypeDeclaration,
    source: Source,
    thisType: ThisType,
    outerTypeName: Option[TypeName],
    modifierResults: ModifierResults,
    endLineOffset: Option[Int] = None
  ): InterfaceDeclaration = {

    val implementsType =
      itd.extendsTypeList.map(TypeList.construct).getOrElse(ArraySeq(TypeNames.InternalInterface))
    val typeContext = new RelativeTypeContext
    val id          = OutlineParserId.construct(itd.id, source.path)

    val methods = itd.methods.flatMap(
      m =>
        OutlineParserClassBodyDeclaration.constructInterfaceMethodDeclaration(
          path,
          m,
          source,
          typeContext,
          outerTypeName.isEmpty,
          thisType
        )
    )

    val declaration = InterfaceDeclaration(
      source,
      thisType.module,
      typeContext,
      thisType.typeName,
      outerTypeName,
      id,
      modifierResults,
      thisType.inTest,
      implementsType,
      ArraySeq.from(methods)
    )
    LocationUtils.stampLocation(
      declaration,
      itd.location.get.copy(
        startLineOffset = itd.location.get.startLineOffset - 1,
        endLineOffset = itd.location.get.endLineOffset + endLineOffset.getOrElse(0)
      ),
      source.path
    )
    typeContext.freeze(declaration)
    declaration
  }
}

private[opcst] object OutlineParserEnumDeclaration {

  def constructInner(
    path: PathLike,
    ie: OPEnumTypeDeclaration,
    source: Source,
    outerType: ThisType
  ): Option[EnumDeclaration] = {
    val modifierResults =
      ModifierUtils.enumModifiers(path, ie.id.get, ie.annotations, ie.modifiers, outer = false)
    val thisType = outerType.asInner(ie.id.get.id.contents)
    val rv       = construct(ie, source, thisType, Some(outerType.typeName), modifierResults, Some(-1))
    Some(rv)
  }

  def construct(
    etd: OPEnumTypeDeclaration,
    source: Source,
    thisType: ThisType,
    outerTypeName: Option[TypeName],
    modifierResults: ModifierResults,
    endLineOffset: Option[Int] = None
  ): EnumDeclaration = {

    val id = OutlineParserId.construct(etd.id, source.path)

    val typeContext = new RelativeTypeContext

    val fields = etd.constants.flatMap(
      c => constructEnumConstant(c, source, thisType, outerTypeName.isDefined)
    )

    val declaration = EnumDeclaration(
      source,
      thisType.module,
      typeContext,
      thisType.typeName,
      outerTypeName,
      id,
      modifierResults,
      thisType.inTest,
      ArraySeq.from(fields)
    )
    LocationUtils.stampLocation(
      declaration,
      etd.location.get.copy(
        startLineOffset = etd.location.get.startLineOffset - 1,
        endLineOffset = etd.location.get.endLineOffset + endLineOffset.getOrElse(0)
      ),
      source.path
    )
    typeContext.freeze(declaration)
    declaration
  }

  private def constructEnumConstant(
    id: OPId,
    source: Source,
    thisType: ThisType,
    isOuter: Boolean
  ): Option[ClassBodyDeclaration] = {

    val modifierResults = ModifierUtils.enumConstantModifiers()
    val vd =
      OutlineParserClassBodyDeclaration.constructVariableDeclarator(id, source, thisType.typeName)

    val declaration = ApexFieldDeclaration(thisType, modifierResults, thisType.typeName, vd)
    LocationUtils.stampLocation(
      declaration,
      id.id.location.copy(startLineOffset = id.id.location.startLineOffset - 1),
      source.path
    )
    Some(declaration)
  }
}

private[opcst] object OutlineParserClassBodyDeclaration {

  def constructConstructorDeclaration(
    path: PathLike,
    cd: OPConstructorDeclaration,
    source: Source,
    typeContext: RelativeTypeContext,
    isOuter: Boolean,
    thisType: ThisType
  ): Option[ClassBodyDeclaration] = {

    val modifierResults =
      ModifierUtils.constructorModifiers(path, cd.id, cd.annotations, cd.modifiers)
    val qualifiedName = QualifiedName(cd.qName.qName.map(id => Names(id.id.contents)).toIndexedSeq)
    LocationUtils.stampLocation(
      qualifiedName,
      cd.id.id.location.copy(startLineOffset = cd.id.id.location.startLineOffset - 1),
      source.path
    )

    val parameters = cd.formalParameterList.formalParameters
      .flatMap(OutlineParserFormalParameter.construct(path, _, source, typeContext))
      .pipe(ArraySeq.from)

    val block = OutlineParserBlock.construct(
      source,
      cd.blockLocation.get,
      if (isOuter) Some(source) else None
    )

    val declaration =
      ApexConstructorDeclaration(modifierResults, qualifiedName, parameters, thisType.inTest, block)
    val location = OPLocation(
      cd.id.id.location.startLine,
      cd.id.id.location.startLineOffset - 1,
      0,
      cd.location.get.endLine,
      cd.location.get.endLineOffset - 1,
      0
    )
    LocationUtils.stampLocation(declaration, location, source.path)
    Some(declaration)
  }

  def constructClassMethodDeclaration(
    path: PathLike,
    md: OPMethodDeclaration,
    source: Source,
    typeContext: RelativeTypeContext,
    methodOwnerNature: MethodOwnerNature,
    isOuter: Boolean,
    thisType: ThisType
  ): Option[ClassBodyDeclaration] = {

    val modifierResults = ModifierUtils.classMethodModifiers(
      path,
      md.id,
      md.annotations,
      md.modifiers,
      methodOwnerNature,
      isOuter
    )

    val block =
      if (md.blockLocation.isEmpty) None
      else
        Some(
          OutlineParserBlock
            .construct(source, md.blockLocation.get, if (isOuter) Some(source) else None)
        )

    val parameters = md.formalParameterList.formalParameters
      .flatMap(OutlineParserFormalParameter.construct(path, _, source, typeContext))
      .pipe(ArraySeq.from)

    val declaration = new ApexMethodDeclaration(
      thisType,
      modifierResults,
      RelativeTypeName(typeContext, TypeReference.construct(md.typeRef)),
      OutlineParserId.construct(md.id, source.path),
      parameters,
      block
    )

    val location = OPLocation(
      md.typeRef.typeNames(0).id.id.location.startLine,
      md.typeRef.typeNames(0).id.id.location.startLineOffset - 1,
      0,
      md.location.get.endLine,
      md.location.get.endLineOffset - 1,
      0
    )

    LocationUtils.stampLocation(declaration, location, source.path)
    Some(declaration)
  }

  def constructInterfaceMethodDeclaration(
    path: PathLike,
    md: OPMethodDeclaration,
    source: Source,
    typeContext: RelativeTypeContext,
    isOuter: Boolean,
    thisType: ThisType
  ): Option[ClassBodyDeclaration] = {

    val modifierResults =
      ModifierUtils.interfaceMethodModifiers(path, md.id, md.annotations, md.modifiers)

    val parameters = md.formalParameterList.formalParameters
      .flatMap(OutlineParserFormalParameter.construct(path, _, source, typeContext))
      .pipe(ArraySeq.from)

    val declaration = new ApexMethodDeclaration(
      thisType,
      modifierResults,
      RelativeTypeName(typeContext, TypeReference.construct(md.typeRef)),
      OutlineParserId.construct(md.id, source.path),
      parameters,
      None
    )

    val location = OPLocation(
      md.typeRef.typeNames(0).id.id.location.startLine,
      md.typeRef.typeNames(0).id.id.location.startLineOffset - 1,
      0,
      md.location.get.endLine,
      md.location.get.endLineOffset - 1,
      0
    )

    LocationUtils.stampLocation(declaration, location, source.path)
    Some(declaration)
  }

  def constructFieldDeclaration(
    path: PathLike,
    fd: OPFieldDeclaration,
    source: Source,
    typeContext: RelativeTypeContext,
    isOuter: Boolean,
    thisType: ThisType
  ): Option[ClassBodyDeclaration] = {

    val modifierResults =
      ModifierUtils.fieldModifiers(path, fd.id, fd.annotations, fd.modifiers, isOuter)
    val fieldTypeName = TypeReference.construct(fd.typeRef)
    val vd            = constructVariableDeclarator(fd, source, fieldTypeName, isOuter)

    val declaration = ApexFieldDeclaration(thisType, modifierResults, fieldTypeName, vd)
    val location = OPLocation(
      fd.typeRef.typeNames(0).id.id.location.startLine,
      fd.typeRef.typeNames(0).id.id.location.startLineOffset - 1,
      0,
      if (fd.blockLocation.isDefined) fd.blockLocation.get.endLine else fd.id.id.location.endLine,
      if (fd.blockLocation.isDefined) fd.blockLocation.get.endLineOffset
      else fd.id.id.location.endLineOffset + 1,
      0
    )
    LocationUtils.stampLocation(declaration, location, source.path)
    Some(declaration)
  }

  def constructVariableDeclarator(
    id: OPId,
    source: Source,
    typeName: TypeName
  ): VariableDeclarator = {

    VariableDeclarator(typeName, OutlineParserId.construct(id, source.path), None)
  }

  def constructVariableDeclarator(
    fd: OPFieldDeclaration,
    source: Source,
    typeName: TypeName,
    isOuter: Boolean
  ): VariableDeclarator = {

    def parseInitializer(): Option[Expression] = {

      // parseExpression does not seem to be greedy. Try and force it...
      // ... by appending a '}'

      val length  = fd.blockLocation.get.endByteOffset - fd.blockLocation.get.startByteOffset + 2
      val tmpData = new Array[Byte](length)
      Array.copy(source.code.source, fd.blockLocation.get.startByteOffset, tmpData, 0, length - 1)
      tmpData(length - 1) = '}'

      val newSrcData = SourceData(tmpData, 0, length, None, source.code.isASCII)
      val fieldSource: Source = Source(
        source.path,
        newSrcData,
        0, 
        0,
        if (isOuter) Some(source) else None,
        Some(fd.blockLocation.get.startLine),
        Some(fd.blockLocation.get.startLineOffset - 2)
      )

      val parser = new CodeParser(fieldSource)
      val result = parser.parseExpression()
      result.issues.foreach(Hierarchy.OrgImpl.log)
      if (result.issues.nonEmpty) return None
      var expr: Option[Expression] = None
      CST.sourceContext.withValue(Some(fieldSource)) {
        expr = Some(Expression.construct(result.value))
      }
      expr
    }

    val init: Option[Expression] = if (fd.blockLocation.isDefined) parseInitializer() else None

    val declaration =
      VariableDeclarator(typeName, OutlineParserId.construct(fd.id, source.path), init)
    LocationUtils.stampLocation(
      declaration,
      LocationUtils.extendLocation(fd.id.id.location, startLineOffset = -1),
      source.path
    )
    declaration
  }

  def constructInitializerBlock(
    i: OPInitializer,
    source: Source,
    typeContext: RelativeTypeContext,
    isOuter: Boolean,
    thisType: ThisType
  ): Option[ClassBodyDeclaration] = {

    val modifierResults = ModifierUtils.initializerBlockModifiers(i.isStatic)
    val declaration = ApexInitializerBlock(
      modifierResults,
      OutlineParserBlock
        .construct(source, i.blockLocation.get, if (isOuter) Some(source) else None),
      thisType.inTest
    )
    LocationUtils.stampLocation(
      declaration,
      i.location.get.copy(startLineOffset = i.location.get.startLineOffset - 1),
      source.path
    )
    Some(declaration)
  }

  def constructPropertyDeclaration(
    path: PathLike,
    pd: OPPropertyDeclaration,
    source: Source,
    typeContext: RelativeTypeContext,
    isOuter: Boolean,
    thisType: ThisType
  ): Option[ClassBodyDeclaration] = {

    val propertyTypeName = TypeReference.construct(pd.typeRef)

    def parsePropertyBlock(pb: OPPropertyBlock): Option[PropertyBlock] = {

      SourceOps.withSource(source, pb.blockLocation.get, 0, if (isOuter) Some(source) else None) {
        propertyBlockSource =>
          val parser = new CodeParser(propertyBlockSource)
          val result = parser.parsePropertyBlock()
          result.issues.foreach(Hierarchy.OrgImpl.log)
          if (result.issues.nonEmpty) return None

          var rv: Option[PropertyBlock] = None
          CST.sourceContext.withValue(Some(propertyBlockSource)) {
            rv = PropertyBlock.construct(parser, result.value, propertyTypeName)
          }
          rv
      }
    }

    val modifierResults =
      ModifierUtils.fieldModifiers(path, pd.id, pd.annotations, pd.modifiers, isOuter)
    val propertyBlocks = ArraySeq.from(pd.propertyBlocks.flatMap(parsePropertyBlock))

    val declaration =
      ApexPropertyDeclaration(
        thisType,
        modifierResults,
        propertyTypeName,
        OutlineParserId.construct(pd.id, source.path),
        propertyBlocks
      )

    val location = OPLocation(
      pd.typeRef.typeNames(0).id.id.location.startLine,
      pd.typeRef.typeNames(0).id.id.location.startLineOffset - 1,
      0,
      pd.location.get.endLine,
      pd.location.get.endLineOffset - 1,
      0
    )

    LocationUtils.stampLocation(declaration, location, source.path)
    Some(declaration)
  }
}

private[opcst] object OutlineParserFormalParameter {

  val noParams: ArraySeq[ParameterDeclaration] = ArraySeq()

  def construct(
    path: PathLike,
    src: OPFormalParameter,
    source: Source,
    typeContext: RelativeTypeContext
  ): Option[FormalParameter] = {

    val fp = FormalParameter(
      ModifierUtils
        .parameterModifiers(path, src.id.get, src.annotations, src.modifiers),
      RelativeTypeName(typeContext, TypeReference.construct(src.typeRef.get)),
      OutlineParserId.construct(src.id, source.path)
    )
    Some(fp)
  }
}

private[opcst] object OutlineParserBlock {
  val b = new BlockContext(null, 0)

  def construct(src: Source, blockLocation: OPLocation, outer: Option[Source]): LazyBlock = {

    SourceOps.withSource(src, blockLocation, 1, outer) { source =>
      val wf = new WeakReference(b)
      wf.clear()
      LazyBlock(source, wf, isTrigger = false)
    }
  }
}

private[opcst] object SourceOps {

  def withSource[T](base: Source, blockLocation: OPLocation, offset: Int, outer: Option[Source])(
    f: Source => T
  ): T = {

    val newSrcData = SourceData(
      base.code.source,
      blockLocation.startByteOffset - offset,
      blockLocation.endByteOffset - blockLocation.startByteOffset + 1,
      None,
      base.code.isASCII
    )
    val source: Source = Source(
      base.path,
      newSrcData,
      0,
      0,
      outer,
      Some(blockLocation.startLine),
      Some(blockLocation.startLineOffset - 2)
    )
    f(source)
  }
}
