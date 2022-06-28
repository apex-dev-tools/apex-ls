package com.financialforce.oparser.testutil

import com.financialforce.oparser._
import com.financialforce.types.base._
import com.financialforce.types.{ITypeDeclaration, base}
import com.nawforce.apexparser.{ApexLexer, ApexParser, CaseInsensitiveInputStream}
import org.antlr.v4.runtime._

import java.io.ByteArrayInputStream
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

case class Issue(path: String, line: Int, lineOffset: Int, msg: String)

class CollectingErrorListener(path: String) extends BaseErrorListener {
  var _issues: mutable.ArrayBuffer[Issue] = _

  override def syntaxError(
    recognizer: Recognizer[_, _],
    offendingSymbol: Any,
    line: Int,
    charPositionInLine: Int,
    msg: String,
    e: RecognitionException
  ): Unit = {
    if (_issues == null)
      _issues = new mutable.ArrayBuffer[Issue]()
    _issues.addOne(Issue(path, line, charPositionInLine, msg))
  }

  def issues: ArraySeq[Issue] = {
    if (_issues != null)
      ArraySeq.unsafeWrapArray(_issues.toArray)
    else
      ArraySeq.empty
  }
}

object Antlr {

  def parse(path: String, source: Array[Byte]): Option[ITypeDeclaration] = {
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
      val ctd = new TestClassTypeDeclaration(path, enclosing = null)

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
      val itd = new TestInterfaceTypeDeclaration(path, enclosing = null)

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
      val etd = new TestEnumTypeDeclaration(path, enclosing = null)

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

  def toId(ctx: ApexParser.IdContext): LocatableIdToken = {
    LocatableIdToken(ctx.children.asScala.mkString(" "), location(ctx))
  }

  def toId(text: String): LocatableIdToken = {
    LocatableIdToken(text, Location.default)
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

  def antlrTypeList(ctx: ApexParser.TypeListContext): ArraySeq[TypeRef] = {
    ArraySeq.unsafeWrapArray(
      ctx
        .typeRef()
        .asScala
        .map(tr => antlrTypeRef(tr))
        .toArray
    )
  }

  def antlrTypeArguments(ctx: ApexParser.TypeArgumentsContext): ArraySeq[TypeRef] = {
    antlrTypeList(ctx.typeList())
  }

  def antlrTypeName(ctx: ApexParser.TypeNameContext): TypeNameSegment = {
    val typeArguments =
      Option(ctx.typeArguments()).map(ta => antlrTypeArguments(ta)).getOrElse(TypeRef.emptyArraySeq)
    val tnOpt = Option(ctx.LIST())
      .map(l => new TypeNameSegment(LocatableIdToken(l.toString, Location.default), typeArguments))
      .orElse(
        Option(ctx.SET())
          .map(
            l => new TypeNameSegment(LocatableIdToken(l.toString, Location.default), typeArguments)
          )
      )
      .orElse(
        Option(ctx.MAP())
          .map(
            l => new TypeNameSegment(LocatableIdToken(l.toString, Location.default), typeArguments)
          )
      )
      .orElse(Option(ctx.id()).map(l => new TypeNameSegment(toId(l), typeArguments)))

    if (tnOpt.isEmpty) {
      throw new Exception("Missing type name")
    }
    tnOpt.get
  }

  def antlrTypeRef(ctx: ApexParser.TypeRefContext): UnresolvedTypeRef = {
    val segments = new mutable.ArrayBuffer[TypeNameSegment]()
    ctx
      .typeName()
      .forEach(tn => {
        segments.append(antlrTypeName(tn))
      })

    base.UnresolvedTypeRef(
      segments.toArray,
      Option(ctx.arraySubscripts()).map(_.RBRACK().size()).getOrElse(0)
    )
  }

  def antlrClassTypeDeclaration(
    ctd: TestClassTypeDeclaration,
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
            val innerClassDeclaration = new TestClassTypeDeclaration(ctd.path, ctd)
            innerClassDeclaration.setAnnotations(md.annotations)
            innerClassDeclaration.setModifiers(md.modifiers)
            ctd.appendInnerType(innerClassDeclaration)
            antlrClassTypeDeclaration(innerClassDeclaration, icd)
          })

          Option(d.interfaceDeclaration()).foreach(iid => {
            val innerInterfaceDeclaration = new TestInterfaceTypeDeclaration(ctd.path, ctd)
            innerInterfaceDeclaration.setAnnotations(md.annotations)
            innerInterfaceDeclaration.setModifiers(md.modifiers)
            ctd.appendInnerType(innerInterfaceDeclaration)
            antlrInterfaceTypeDeclaration(innerInterfaceDeclaration, iid)
          })

          Option(d.enumDeclaration()).foreach(ied => {
            val innerEnumDeclaration = new TestEnumTypeDeclaration(ctd.path, ctd)
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
    itd: TestInterfaceTypeDeclaration,
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
    etd: TestEnumTypeDeclaration,
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
    ctd: TestClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.ConstructorDeclarationContext
  ): Unit = {

    val qName = QualifiedName(ctx.qualifiedName().id().asScala.map(toId).toArray)
    val formalParameterList =
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
        .getOrElse(FormalParameter.emptyArraySeq)

    val constructor =
      ConstructorDeclaration(md.annotations, md.modifiers, qName, formalParameterList)

    ctd.appendConstructor(constructor)
  }

  def antlrMethodDeclaration(
    res: TestTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.MethodDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())

    val formalParameterList =
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
        .getOrElse(FormalParameter.emptyArraySeq)

    if (Option(ctx.typeRef()).isDefined) {
      md.add(antlrTypeRef(ctx.typeRef()))
    } else {
      md.typeRef = Some(
        UnresolvedTypeRef(Array(new TypeNameSegment(toId("void"), TypeRef.emptyArraySeq)), 0)
      )
    }

    val method =
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef, id, formalParameterList)

    res.appendMethod(method)
  }

  def antlrMethodDeclaration(
    res: TestTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.InterfaceMethodDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())

    val formalParameterList =
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
        .getOrElse(FormalParameter.emptyArraySeq)

    if (Option(ctx.typeRef()).isDefined) {
      md.add(antlrTypeRef(ctx.typeRef()))
    } else {
      md.typeRef = Some(
        new UnresolvedTypeRef(Array(new TypeNameSegment(toId("void"), TypeRef.emptyArraySeq)), 0)
      )
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
    ctd: TestClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.PropertyDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())
    md.add(antlrTypeRef(ctx.typeRef()))

    val property = PropertyDeclaration(md.annotations, md.modifiers, md.typeRef.get, Array(), id)
    ctd.appendProperty(property)
  }

  def antlrFieldDeclaration(
    ctd: TestClassTypeDeclaration,
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
