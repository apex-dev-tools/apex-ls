/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import com.nawforce.apexparser.{ApexLexer, ApexParser, CaseInsensitiveInputStream}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import java.io.ByteArrayInputStream
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Antlr {

  def parse(source: Array[Byte]): Option[TypeDeclaration] = {
    val cis: CaseInsensitiveInputStream = new CaseInsensitiveInputStream(
      CharStreams.fromStream(new ByteArrayInputStream(source, 0, source.length))
    )
    val tokenStream = new CommonTokenStream(new ApexLexer(cis))
    tokenStream.fill()

    val parser = new ApexParser(tokenStream)
    val tree = parser.compilationUnit()

    if (Option(tree.typeDeclaration().classDeclaration()).isDefined) {
      val ctd = new ClassTypeDeclaration("", null)
      tree
        .typeDeclaration()
        .modifier()
        .asScala
        .filter(_.annotation() != null)
        .foreach(m => antlrAnnotation(ctd, m.annotation()))
      tree
        .typeDeclaration()
        .modifier()
        .asScala
        .filter(_.annotation() == null)
        .map(toModifier)
        .foreach(ctd.add)
      antlrClassTypeDeclaration(ctd, tree.typeDeclaration().classDeclaration())
      return Some(ctd)
    }
    if (Option(tree.typeDeclaration().interfaceDeclaration()).isDefined) {
      val itd = new InterfaceTypeDeclaration("", null)
      tree
        .typeDeclaration()
        .modifier()
        .asScala
        .filter(_.annotation() != null)
        .foreach(m => antlrAnnotation(itd, m.annotation()))
      tree
        .typeDeclaration()
        .modifier()
        .asScala
        .filter(_.annotation() == null)
        .map(toModifier)
        .foreach(itd.add)
      antlrInterfaceTypeDeclaration(itd, tree.typeDeclaration().interfaceDeclaration())
      return Some(itd)
    }
    if (Option(tree.typeDeclaration().enumDeclaration()).isDefined) {
      val etd = new EnumTypeDeclaration("", null)
      tree
        .typeDeclaration()
        .modifier()
        .asScala
        .filter(_.annotation() != null)
        .foreach(m => antlrAnnotation(etd, m.annotation()))
      tree
        .typeDeclaration()
        .modifier()
        .asScala
        .filter(_.annotation() == null)
        .map(toModifier)
        .foreach(etd.add)
      antlrEnumTypeDeclaration(etd, tree.typeDeclaration().enumDeclaration())
      return Some(etd)
    }
    None
  }

  def antlrId(i: IdAssignable, ctx: ApexParser.IdContext): Unit = {
    i.add(toId(ctx))
  }

  def toId(ctx: ApexParser.IdContext): Id = {
    Id(IdToken(ctx.children.asScala.mkString(" "), Location.default))
  }

  def toId(text: String): Id = {
    Id(IdToken(text, Location.default))
  }

  def toModifier(ctx: ApexParser.ModifierContext): Modifier = {
    Modifier(IdToken(ctx.children.asScala.mkString(" "), Location.default))
  }

  def antlrAnnotation(a: AnnotationAssignable, ctx: ApexParser.AnnotationContext): Unit = {
    val qName = new QualifiedName
    ctx.qualifiedName().id().asScala.foreach(id => antlrId(qName, id))
    a.add(Annotation(qName, None))
  }

  def antlrTypeList(res: TypeListAssignable, ctx: ApexParser.TypeListContext): Unit = {
    val typeList = new TypeList
    ctx
      .typeRef()
      .forEach(tr => {
        antlrTypeRef(typeList, tr)
      })
    if (typeList.typeRefs.nonEmpty) res.add(typeList)
  }

  def antlrTypeArguments(
    res: TypeArgumentsAssignable,
    ctx: ApexParser.TypeArgumentsContext
  ): Unit = {
    val typeArguments = new TypeArguments
    antlrTypeList(typeArguments, ctx.typeList())
    if (typeArguments.typeList.isDefined)
      res.add(typeArguments)
  }

  def antlrTypeName(res: TypeNameAssignable, ctx: ApexParser.TypeNameContext): Unit = {
    val tnOpt = Option(ctx.LIST())
      .map(l => new TypeName(Id(IdToken(l.toString, Location.default))))
      .orElse(Option(ctx.SET()).map(l => new TypeName(Id(IdToken(l.toString, Location.default)))))
      .orElse(Option(ctx.MAP()).map(l => new TypeName(Id(IdToken(l.toString, Location.default)))))
      .orElse(Option(ctx.id()).map(l => new TypeName(toId(l))))

    if (tnOpt.isEmpty)
      throw new Exception("Missing type name")
    val tn = tnOpt.get
    res.add(tn)
    Option(ctx.typeArguments()).foreach(ta => antlrTypeArguments(tn, ta))
  }

  def antlrArraySubscripts(
    res: ArraySubscriptsAssignable,
    ctx: ApexParser.ArraySubscriptsContext
  ): Unit = {
    ctx
      .RBRACK()
      .forEach(_ => {
        res.add(new ArraySubscripts)
      })
  }

  def antlrTypeRef(res: TypeRefAssignable, ctx: ApexParser.TypeRefContext): Unit = {

    val typeRef = new TypeRef
    res.add(typeRef)

    ctx
      .typeName()
      .forEach(tn => {
        antlrTypeName(typeRef, tn)
      })
    if (Option(ctx.arraySubscripts()).isDefined) {
      antlrArraySubscripts(typeRef, ctx.arraySubscripts())
    }
  }

  def antlrClassTypeDeclaration(
    ctd: ClassTypeDeclaration,
    ctx: ApexParser.ClassDeclarationContext
  ): Unit = {
    ctd._id = toId(ctx.id())

    if (Option(ctx.typeRef()).isDefined) {
      antlrTypeRef(ctd, ctx.typeRef())
    }

    if (Option(ctx.typeList()).isDefined) {
      antlrTypeList(ctd, ctx.typeList())
    }

    ctx.classBody().classBodyDeclaration.forEach { c =>
      {
        Option(c.memberDeclaration()).foreach(d => {
          val md = new MemberDeclaration
          c.modifier()
            .asScala
            .filter(_.annotation() != null)
            .foreach(m => antlrAnnotation(md, m.annotation()))
          c.modifier().asScala.filter(_.annotation() == null).map(toModifier).foreach(md.add)

          Option(d.classDeclaration()).foreach(icd => {
            val innerClassDeclaration = new ClassTypeDeclaration("", null)
            innerClassDeclaration._annotations.addAll(md.annotations)
            innerClassDeclaration._modifiers.addAll(md.modifiers)
            ctd._innerTypes.append(innerClassDeclaration)
            antlrClassTypeDeclaration(innerClassDeclaration, icd)
          })

          Option(d.interfaceDeclaration()).foreach(iid => {
            val innerInterfaceDeclaration = new InterfaceTypeDeclaration("", ctd)
            innerInterfaceDeclaration._annotations.addAll(md.annotations)
            innerInterfaceDeclaration._modifiers.addAll(md.modifiers)
            ctd._innerTypes.append(innerInterfaceDeclaration)
            antlrInterfaceTypeDeclaration(innerInterfaceDeclaration, iid)
          })

          Option(d.enumDeclaration()).foreach(ied => {
            val innerEnumDeclaration = new EnumTypeDeclaration("", ctd)
            innerEnumDeclaration._annotations.addAll(md.annotations)
            innerEnumDeclaration._modifiers.addAll(md.modifiers)
            ctd._innerTypes.append(innerEnumDeclaration)
            antlrEnumTypeDeclaration(innerEnumDeclaration, ied)
          })

          Option(d.constructorDeclaration()).foreach(antlrConstructorDeclaration(ctd, md, _))
          Option(d.methodDeclaration()).foreach(antlrMethodDeclaration(ctd, md, _))
          Option(d.propertyDeclaration()).foreach(antlrPropertyDeclaration(ctd, md, _))
          Option(d.fieldDeclaration()).foreach(antlrFieldDeclaration(ctd, md, _))
        })
        Option(c.block()).foreach(i => {
          ctd.add(Initializer(Option(c.STATIC()).isDefined))
        })
      }
    }
  }

  def antlrInterfaceTypeDeclaration(
    itd: InterfaceTypeDeclaration,
    ctx: ApexParser.InterfaceDeclarationContext
  ): Unit = {
    itd._id = toId(ctx.id())

    if (Option(ctx.typeList()).isDefined) {
      antlrTypeList(itd, ctx.typeList())
    }

    ctx
      .interfaceBody()
      .interfaceMethodDeclaration()
      .asScala
      .foreach(mctx => {
        val md = new MemberDeclaration
        mctx
          .modifier()
          .asScala
          .filter(_.annotation() != null)
          .foreach(m => antlrAnnotation(md, m.annotation()))
        mctx.modifier().asScala.filter(_.annotation() == null).map(toModifier).foreach(md.add)

        antlrMethodDeclaration(itd, md, mctx)
      })
  }

  def antlrEnumTypeDeclaration(
    etd: EnumTypeDeclaration,
    ctx: ApexParser.EnumDeclarationContext
  ): Unit = {
    etd._id = toId(ctx.id())

    ctx
      .enumConstants()
      .id()
      .asScala
      .foreach(ictx => {
        etd.constants.append(toId(ictx))
      })
  }

  def antlrConstructorDeclaration(
    ctd: ClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.ConstructorDeclarationContext
  ): Unit = {

    val qName = new QualifiedName
    ctx.qualifiedName().id().asScala.map(toId).foreach(qName.add)

    val formalParameterList = new FormalParameterList

    Option(ctx.formalParameters())
      .flatMap(fp => Option(fp.formalParameterList()))
      .foreach(
        fpl =>
          fpl.formalParameter().asScala.foreach(fp => antlrFormalParameter(formalParameterList, fp))
      )

    val constructor =
      ConstructorDeclaration(md.annotations, md.modifiers, qName, formalParameterList)

    ctd._constructors.append(constructor)
  }

  def antlrMethodDeclaration(
    res: MethodDeclarationAssignable,
    md: MemberDeclaration,
    ctx: ApexParser.MethodDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())

    val formalParameterList = new FormalParameterList

    Option(ctx.formalParameters())
      .flatMap(fp => Option(fp.formalParameterList()))
      .foreach(
        fpl =>
          fpl.formalParameter().asScala.foreach(fp => antlrFormalParameter(formalParameterList, fp))
      )

    if (Option(ctx.typeRef()).isDefined) {
      antlrTypeRef(md, ctx.typeRef())
    } else {
      md.typeRef = Some(new TypeRef)
      md.typeRef.get.add(new TypeName(toId("void")))
    }

    val method =
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef.get, id, formalParameterList)

    res.add(method)
  }

  def antlrMethodDeclaration(
    res: MethodDeclarationAssignable,
    md: MemberDeclaration,
    ctx: ApexParser.InterfaceMethodDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())

    val formalParameterList = new FormalParameterList

    Option(ctx.formalParameters())
      .flatMap(fp => Option(fp.formalParameterList()))
      .foreach(
        fpl =>
          fpl.formalParameter().asScala.foreach(fp => antlrFormalParameter(formalParameterList, fp))
      )

    if (Option(ctx.typeRef()).isDefined) {
      antlrTypeRef(md, ctx.typeRef())
    } else {
      md.typeRef = Some(new TypeRef)
      md.typeRef.get.add(new TypeName(toId("void")))
    }

    val method =
      MethodDeclaration(md.annotations, md.modifiers, md.typeRef.get, id, formalParameterList)

    res.add(method)
  }

  def antlrFormalParameter(
    fpl: FormalParameterList,
    ctx: ApexParser.FormalParameterContext
  ): Unit = {
    val fp = new FormalParameter
    ctx
      .modifier()
      .asScala
      .filter(_.annotation() != null)
      .foreach(m => antlrAnnotation(fp, m.annotation()))
    ctx.modifier().asScala.filter(_.annotation() == null).map(toModifier).foreach(fp.add)

    antlrTypeRef(fp, ctx.typeRef())
    fp.add(toId(ctx.id()))
    fpl.add(fp)
  }

  def antlrPropertyDeclaration(
    ctd: ClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.PropertyDeclarationContext
  ): Unit = {

    val id = toId(ctx.id())
    antlrTypeRef(md, ctx.typeRef())

    val property =
      new PropertyDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)

    ctd._properties.append(property)
  }

  def antlrFieldDeclaration(
    ctd: ClassTypeDeclaration,
    md: MemberDeclaration,
    ctx: ApexParser.FieldDeclarationContext
  ): Unit = {
    antlrTypeRef(md, ctx.typeRef())

    Option(ctx.variableDeclarators())
      .foreach(_.variableDeclarator().asScala.foreach(v => {
        val id = toId(v.id())
        val field =
          FieldDeclaration(md.annotations, md.modifiers, md.typeRef.get, id)
        ctd._fields.append(field)
      }))
  }
}
