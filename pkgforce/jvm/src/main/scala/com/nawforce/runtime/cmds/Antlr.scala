package com.nawforce.runtime.cmds

import com.financialforce.oparser._
import com.nawforce.apexparser.{ApexLexer, ApexParser, CaseInsensitiveInputStream}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.parsers.CodeParser.ParserRuleContext
import com.nawforce.runtime.parsers.CollectingErrorListener
import com.nawforce.runtime.workspace.{
  ClassTypeDeclaration,
  EnumTypeDeclaration,
  InterfaceTypeDeclaration,
  TypeDeclaration
}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import java.io.ByteArrayInputStream
import scala.collection.compat.immutable.ArraySeq
import scala.jdk.CollectionConverters._

object Antlr {

  def parse(path: PathLike, source: Array[Byte]): Option[TypeDeclaration] = {
    val cis: CaseInsensitiveInputStream = new CaseInsensitiveInputStream(
      CharStreams.fromStream(new ByteArrayInputStream(source, 0, source.length))
    )
    val tokenStream = new CommonTokenStream(new ApexLexer(cis))
    tokenStream.fill()

    val parser   = new ApexParser(tokenStream)
    val listener = new CollectingErrorListener(path)
    parser.removeErrorListeners()
    parser.addErrorListener(listener)

    val tree = parser.compilationUnit()

    if (listener.issues.nonEmpty)
      throw new Exception(listener.issues.head.toString)

    if (Option(tree.typeDeclaration().classDeclaration()).isDefined) {
      val ctd = new ClassTypeDeclaration(null, "", null)

      ctd.setAnnotations(
        tree
          .typeDeclaration()
          .modifier()
          .asScala
          .filter(_.annotation() != null)
          .map(m => antlrAnnotation(m.annotation()))
          .toArray
      )
      ctd.setModifiers(
        tree
          .typeDeclaration()
          .modifier()
          .asScala
          .filter(_.annotation() == null)
          .map(toModifier)
          .toArray
      )

      antlrClassTypeDeclaration(ctd, tree.typeDeclaration().classDeclaration())
      return Some(ctd)
    }
    if (Option(tree.typeDeclaration().interfaceDeclaration()).isDefined) {
      val itd = new InterfaceTypeDeclaration(null, "", null)

      itd.setAnnotations(
        tree
          .typeDeclaration()
          .modifier()
          .asScala
          .filter(_.annotation() != null)
          .map(m => antlrAnnotation(m.annotation()))
          .toArray
      )
      itd.setModifiers(
        tree
          .typeDeclaration()
          .modifier()
          .asScala
          .filter(_.annotation() == null)
          .map(toModifier)
          .toArray
      )

      antlrInterfaceTypeDeclaration(itd, tree.typeDeclaration().interfaceDeclaration())
      return Some(itd)
    }
    if (Option(tree.typeDeclaration().enumDeclaration()).isDefined) {
      val etd = new EnumTypeDeclaration(null, "", null)

      etd.setAnnotations(
        tree
          .typeDeclaration()
          .modifier()
          .asScala
          .filter(_.annotation() != null)
          .map(m => antlrAnnotation(m.annotation()))
          .toArray
      )
      etd.setModifiers(
        tree
          .typeDeclaration()
          .modifier()
          .asScala
          .filter(_.annotation() == null)
          .map(toModifier)
          .toArray
      )

      antlrEnumTypeDeclaration(etd, tree.typeDeclaration().enumDeclaration())
      return Some(etd)
    }
    None
  }

  def toId(ctx: ApexParser.IdContext): LocatableId = {
    LocatableId(ctx.children.asScala.mkString(" "), location(ctx))
  }

  def toId(text: String): LocatableId = {
    LocatableId(text, Location.default)
  }

  def toModifier(ctx: ApexParser.ModifierContext): Modifier = {
    Modifier(ctx.children.asScala.mkString(" "))
  }

  def antlrAnnotation(ctx: ApexParser.AnnotationContext): Annotation = {
    val qName = QualifiedName(ctx.qualifiedName().id().asScala.map(id => toId(id)).toArray)
    val args = Option(ctx.elementValue())
      .map(_.getText)
      .orElse(Option(ctx.elementValuePairs()).map(_.getText))
      .orElse(if (ctx.getText.endsWith("()")) Some("") else None)
    Annotation(qName.toString, args)
  }

  def antlrTypeList(ctx: ApexParser.TypeListContext): TypeList = {
    new TypeList(
      ArraySeq.unsafeWrapArray(
        ctx
          .typeRef()
          .asScala
          .map(tr => antlrTypeRef(tr))
          .toArray
      )
    )
  }

  def antlrTypeArguments(ctx: ApexParser.TypeArgumentsContext): TypeArguments = {
    new TypeArguments(antlrTypeList(ctx.typeList()))
  }

  def antlrTypeName(typeRef: UnresolvedTypeRef, ctx: ApexParser.TypeNameContext): Unit = {
    val typeArguments =
      Option(ctx.typeArguments()).map(ta => antlrTypeArguments(ta)).getOrElse(TypeArguments.empty)
    val tnOpt = Option(ctx.LIST())
      .map(l => new TypeNameSegment(LocatableId(l.toString, Location.default), typeArguments))
      .orElse(
        Option(ctx.SET())
          .map(l => new TypeNameSegment(LocatableId(l.toString, Location.default), typeArguments))
      )
      .orElse(
        Option(ctx.MAP())
          .map(l => new TypeNameSegment(LocatableId(l.toString, Location.default), typeArguments))
      )
      .orElse(Option(ctx.id()).map(l => new TypeNameSegment(toId(l), typeArguments)))

    if (tnOpt.isEmpty)
      throw new Exception("Missing type name")
    val tn = tnOpt.get
    typeRef.typeNameSegments.append(tn)
  }

  def antlrTypeRef(ctx: ApexParser.TypeRefContext): UnresolvedTypeRef = {
    val typeRef = new UnresolvedTypeRef
    typeRef.arraySubscripts = Option(ctx.arraySubscripts()).map(_.RBRACK().size()).getOrElse(0)
    ctx
      .typeName()
      .forEach(tn => {
        antlrTypeName(typeRef, tn)
      })
    typeRef
  }

  def antlrClassTypeDeclaration(
    ctd: ClassTypeDeclaration,
    ctx: ApexParser.ClassDeclarationContext
  ): Unit = {
    ctd.setId(toId(ctx.id()))

    if (Option(ctx.typeRef()).isDefined) {
      ctd.setExtends(antlrTypeRef(ctx.typeRef()))
    }

    if (Option(ctx.typeList()).isDefined) {
      ctd.setImplements(antlrTypeList(ctx.typeList()))
    }

    ctx.classBody().classBodyDeclaration.forEach { c =>
      {
        Option(c.memberDeclaration()).foreach(d => {
          val md = new MemberDeclaration
          md.setAnnotations(
            c.modifier()
              .asScala
              .filter(_.annotation() != null)
              .map(m => antlrAnnotation(m.annotation()))
              .toArray
          )
          md.setModifiers(
            c.modifier().asScala.filter(_.annotation() == null).map(toModifier).toArray
          )

          Option(d.classDeclaration()).foreach(icd => {
            val innerClassDeclaration = new ClassTypeDeclaration(null, "", ctd)
            innerClassDeclaration.setAnnotations(md.annotations)
            innerClassDeclaration.setModifiers(md.modifiers)
            ctd.appendInnerType(innerClassDeclaration)
            antlrClassTypeDeclaration(innerClassDeclaration, icd)
          })

          Option(d.interfaceDeclaration()).foreach(iid => {
            val innerInterfaceDeclaration = new InterfaceTypeDeclaration(null, "", ctd)
            innerInterfaceDeclaration.setAnnotations(md.annotations)
            innerInterfaceDeclaration.setModifiers(md.modifiers)
            ctd.appendInnerType(innerInterfaceDeclaration)
            antlrInterfaceTypeDeclaration(innerInterfaceDeclaration, iid)
          })

          Option(d.enumDeclaration()).foreach(ied => {
            val innerEnumDeclaration = new EnumTypeDeclaration(null, "", ctd)
            innerEnumDeclaration.setAnnotations(md.annotations)
            innerEnumDeclaration.setModifiers(md.modifiers)
            ctd.appendInnerType(innerEnumDeclaration)
            antlrEnumTypeDeclaration(innerEnumDeclaration, ied)
          })

          Option(d.constructorDeclaration()).foreach(antlrConstructorDeclaration(ctd, md, _))
          Option(d.methodDeclaration()).foreach(antlrMethodDeclaration(ctd, md, _))
          Option(d.propertyDeclaration()).foreach(antlrPropertyDeclaration(ctd, md, _))
          Option(d.fieldDeclaration()).foreach(antlrFieldDeclaration(ctd, md, _))
        })
        Option(c.block()).foreach(_ => {
          ctd.appendInitializer(Initializer(Option(c.STATIC()).isDefined))
        })
      }
    }
  }

  def antlrInterfaceTypeDeclaration(
    itd: InterfaceTypeDeclaration,
    ctx: ApexParser.InterfaceDeclarationContext
  ): Unit = {
    itd.setId(toId(ctx.id()))

    if (Option(ctx.typeList()).isDefined) {
      itd.setImplements(antlrTypeList(ctx.typeList()))
    }

    ctx
      .interfaceBody()
      .interfaceMethodDeclaration()
      .asScala
      .foreach(mctx => {
        val md = new MemberDeclaration
        md.setAnnotations(
          mctx
            .modifier()
            .asScala
            .filter(_.annotation() != null)
            .map(m => antlrAnnotation(m.annotation()))
            .toArray
        )
        md.setModifiers(
          mctx.modifier().asScala.filter(_.annotation() == null).map(toModifier).toArray
        )
        antlrMethodDeclaration(itd, md, mctx)
      })
  }

  def antlrEnumTypeDeclaration(
    etd: EnumTypeDeclaration,
    ctx: ApexParser.EnumDeclarationContext
  ): Unit = {
    etd.setId(toId(ctx.id()))

    ctx
      .enumConstants()
      .id()
      .asScala
      .foreach(ictx => {
        val id = toId(ictx)
        etd.appendField(FieldDeclaration(Array(), Array(Modifier("static")), etd, id))
      })
  }

  def antlrConstructorDeclaration(
    ctd: ClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.ConstructorDeclarationContext
  ): Unit = {

    val qName = QualifiedName(ctx.qualifiedName().id().asScala.map(toId).toArray)
    val formalParameterList = new FormalParameterList(
      Option(ctx.formalParameters())
        .flatMap(fp => Option(fp.formalParameterList()))
        .map(
          fpl =>
            ArraySeq.unsafeWrapArray(
              fpl
                .formalParameter()
                .asScala
                .map(antlrFormalParameter)
                .toArray
            )
        )
        .getOrElse(ArraySeq.empty)
    )

    val constructor =
      ConstructorDeclaration(md.annotations, md.modifiers, qName, formalParameterList)

    ctd.appendConstructor(constructor)
  }

  def antlrMethodDeclaration(
    res: TypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.MethodDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())

    val formalParameterList = new FormalParameterList(
      Option(ctx.formalParameters())
        .flatMap(fp => Option(fp.formalParameterList()))
        .map(
          fpl =>
            ArraySeq.unsafeWrapArray(
              fpl
                .formalParameter()
                .asScala
                .map(antlrFormalParameter)
                .toArray
            )
        )
        .getOrElse(ArraySeq.empty)
    )

    if (Option(ctx.typeRef()).isDefined) {
      md.add(antlrTypeRef(ctx.typeRef()))
    } else {
      md.typeRef = Some(new UnresolvedTypeRef)
      md.typeRef.get.typeNameSegments.append(new TypeNameSegment(toId("void"), TypeArguments.empty))
    }

    val method =
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef, id, formalParameterList)

    res.appendMethod(method)
  }

  def antlrMethodDeclaration(
    res: TypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.InterfaceMethodDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())

    val formalParameterList = new FormalParameterList(
      Option(ctx.formalParameters())
        .flatMap(fp => Option(fp.formalParameterList()))
        .map(
          fpl =>
            ArraySeq.unsafeWrapArray(
              fpl
                .formalParameter()
                .asScala
                .map(antlrFormalParameter)
                .toArray
            )
        )
        .getOrElse(ArraySeq.empty)
    )

    if (Option(ctx.typeRef()).isDefined) {
      md.add(antlrTypeRef(ctx.typeRef()))
    } else {
      md.typeRef = Some(new UnresolvedTypeRef)
      md.typeRef.get.typeNameSegments.append(new TypeNameSegment(toId("void"), TypeArguments.empty))
    }

    val method =
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef, id, formalParameterList)

    res.appendMethod(method)
  }

  def antlrFormalParameter(ctx: ApexParser.FormalParameterContext): FormalParameter = {
    FormalParameter(
      ctx
        .modifier()
        .asScala
        .filter(_.annotation() != null)
        .map(m => antlrAnnotation(m.annotation()))
        .toArray,
      ctx.modifier().asScala.filter(_.annotation() == null).map(toModifier).toArray,
      antlrTypeRef(ctx.typeRef()),
      toId(ctx.id())
    )
  }

  def antlrPropertyDeclaration(
    ctd: ClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.PropertyDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())
    md.add(antlrTypeRef(ctx.typeRef()))

    val property = PropertyDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)
    ctd.appendProperty(property)
  }

  def antlrFieldDeclaration(
    ctd: ClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.FieldDeclarationContext
  ): Unit = {
    md.add(antlrTypeRef(ctx.typeRef()))

    Option(ctx.variableDeclarators())
      .foreach(_.variableDeclarator().asScala.foreach(v => {
        val id = toId(v.id())
        val field =
          FieldDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)
        ctd.appendField(field)
      }))
  }

  def location(context: ParserRuleContext): Location = {
    Location(
      context.start.getLine,
      context.start.getCharPositionInLine,
      context.start.getStartIndex,
      context.stop.getLine,
      context.stop.getCharPositionInLine + context.stop.getText.length,
      context.stop.getStopIndex
    )
  }
}
