package com.nawforce.runtime.sfparser_test
import com.financialforce.oparser.StringUtils.{
  asConstructorSignatureString,
  asMethodSignatureString,
  asSignatureString
}
import com.financialforce.oparser._
import com.nawforce.pkgforce.names.Names
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.sfparser_run.{SFParser, SymbolProvider}
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM, TypeDeclaration}

import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object ResolvedOutputComparisonTest {
  var errors = 0
  var total  = 0

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      System.err.println(s"No workspace directory argument provided.")
      return
    }
    if (args.length < 2) {
      System.err.println(
        s"Not enough arguments provided, expected workspace directory and apex db path, '${args.mkString(", ")}'}"
      )
      return
    }

    val absolutePath = Paths.get(Option(args.head).getOrElse("")).toAbsolutePath.normalize()
    val dbpath       = Paths.get(args.tail.headOption.getOrElse("")).toAbsolutePath.normalize()

    val files: Seq[Path] = getFilesFromPath(absolutePath)
    val sources: Map[String, String] = files
      .map(path => {
        path.toString -> getUTF8ContentsFromPath(path)
      })
      .toMap

    val sfParserOutput =
      SFParser(null, sources).parseClassWithSymbolProvider(SymbolProvider(dbpath))
    val index = FileSystemHelper.run(sources) { root: PathLike =>
      {
        new IPM.Index(root)
      }
    }
    sources.keys
      .filterNot(x => sfParserOutput._2.contains(x))
      .foreach(f => {
        total += 1
        val sf = findSfParserOutput(f, sfParserOutput)
        val op = index.rootModule.get.findExactTypeId(sf.get.getFullName)
        compareResolved(sf, op)
      })
    println(s"Number of errors $errors out of $total")
  }

  private def findSfParserOutput(
    path: String,
    output: (ArrayBuffer[TypeDeclaration], ArrayBuffer[String])
  ) = {
    output._1.find(_.paths.head == path)
  }

  private def compareResolved(
    fromSf: Option[IModuleTypeDeclaration],
    fromIndex: Option[IModuleTypeDeclaration]
  ): Unit = {
    val comparator = new ResolvedComparator(RuleSets.apply, fromSf.get)
    try {
      comparator.compare(fromIndex.get)
    } catch {
      case ex: Throwable =>
        errors += 1
        System.err.println(s"Failed output on ${fromSf.get.getFullName}. Reason: ${ex.getMessage}")
    }

  }

  private def getUTF8ContentsFromPath(absolutePath: Path): String = {
    val contentsBytes = Files.readAllBytes(absolutePath)
    new String(contentsBytes, "utf8")
  }

  private def getFilesFromPath(absolutePath: Path) = {
    if (Files.isDirectory(absolutePath)) {
      println("Directory")
      val s = Files.walk(absolutePath)
      s.filter(file => !Files.isDirectory(file))
        .filter(
          file =>
            file.getFileName.toString.toLowerCase
              .endsWith("cls") || file.getFileName.toString.toLowerCase.endsWith("-meta.xml")
        )
        .toArray
        .map(_.asInstanceOf[Path])
        .toIndexedSeq
    } else {
      println("Single file")
      Seq(absolutePath)
    }
  }

}

class ResolvedComparator(rules: RuleSets, firstDecl: IModuleTypeDeclaration) {

  def getWarnings = Array.empty[String] //TODO

  def compare(secondDecl: IModuleTypeDeclaration): Unit = {

    def compareExtends(first: TypeRef, second: TypeRef): (Boolean, String) = {
      throwIfOneIsNull(first, second)
      if (first == null && second == null)
        return (true, "")
      (
        first != null && second != null && compareTypeRef(first, second),
        s"Diff Extends ${first.getFullName} != ${second.getFullName}"
      )
    }
    def compareTypeList(first: TypeList, second: TypeList): (Boolean, String) = {
      throwIfOneIsNull(first, second)
      if (first == null && second == null)
        return (true, "")
      (
        first != null && second != null &&
          first.typeRefs.forall(f => second.typeRefs.exists(s => compareTypeRef(f, s))),
        s"Diff implementsTypeList ${first.typeRefs
          .map(_.getFullName)} != ${second.typeRefs.map(_.getFullName)}"
      )
    }

    def compareConstructors(
      first: ArraySeq[ConstructorDeclaration],
      second: ArraySeq[ConstructorDeclaration]
    ): (Boolean, String) = {
      val check = first.forall(f => {
        val matches = second.find(s => s.qName == f.qName)
        assert(matches.nonEmpty)
        matches.exists(
          s =>
            compareAnnotations(f.annotations, s.annotations) &&
              compareModifiers(f.modifiers, s.modifiers) &&
              compareParamList(f.formalParameterList, s.formalParameterList)
        )
      })
      (
        check,
        s"Diff constructors ${first.map(asConstructorSignatureString).mkString(";")} != ${second.map(asConstructorSignatureString).mkString(";")}"
      )
    }

    def compareFields(
      first: ArraySeq[FieldDeclaration],
      second: ArraySeq[FieldDeclaration]
    ): (Boolean, String) = {
      (
        first.forall(f => {
          val s = second.find(s => f.id == s.id)
          assert(s.nonEmpty)
          compareAnnotations(f.annotations, s.get.annotations) &&
          compareModifiers(f.modifiers, s.get.modifiers) &&
          compareTypeRef(f.typeRef, s.get.typeRef)
        }),
        s"Diff Fields ${first.map(asSignatureString).mkString(";")} != ${second.map(asSignatureString).mkString(";")}"
      )
    }
    def compareProperties(
      first: ArraySeq[PropertyDeclaration],
      second: ArraySeq[PropertyDeclaration]
    ): (Boolean, String) = {
      (
        first.forall(f => {
          val s = second.find(s => f.id == s.id)
          assert(s.nonEmpty)
          compareAnnotations(f.annotations, s.get.annotations) &&
          compareModifiers(f.modifiers, s.get.modifiers) &&
          compareTypeRef(f.typeRef, s.get.typeRef)
        }),
        s"Diff Properties ${first.map(asSignatureString).mkString(";")} != ${second.map(asSignatureString).mkString(";")}"
      )
    }
    def compareMethods(
      first: ArraySeq[MethodDeclaration],
      second: ArraySeq[MethodDeclaration]
    ): (Boolean, String) = {
      (
        first.forall(f => {
          val matches = second.filter(s => s.id == f.id)
          matches.exists(s => {
            f.id == s.id && compareTypeRef(f.typeRef, s.typeRef) && compareModifiers(
              f.modifiers,
              s.modifiers
            ) && compareAnnotations(f.annotations, s.annotations) && compareParamList(
              f.formalParameterList,
              s.formalParameterList
            )
          })
        }),
        s"Diff Methods ${first.map(asMethodSignatureString).mkString(";")} != ${second.map(asMethodSignatureString).mkString(";")}"
      )
    }

    compareAndThrow[TypeRef](compareExtends, firstDecl.extendsTypeRef, secondDecl.extendsTypeRef)
    compareAndThrow[TypeList](
      compareTypeList,
      firstDecl.implementsTypeList,
      secondDecl.implementsTypeList
    )
    compareAndThrow[ArraySeq[ConstructorDeclaration]](
      compareConstructors,
      firstDecl.constructors,
      secondDecl.constructors
    )
    compareAndThrow[ArraySeq[FieldDeclaration]](compareFields, firstDecl.fields, secondDecl.fields)
    compareAndThrow[ArraySeq[PropertyDeclaration]](
      compareProperties,
      firstDecl.properties,
      secondDecl.properties
    )
    compareAndThrow[ArraySeq[MethodDeclaration]](
      compareMethods,
      firstDecl.methods,
      secondDecl.methods
    )

  }

  private def throwIfOneIsNull(first: Any, second: Any): Unit = {
    if ((first == null && second != null) || (second == null && first != null))
      throw new Exception(s"One Object is null while the other is not. '$first != $second'")
  }

  def compareSignature[T <: Signature](first: T, second: T): Boolean = {
    compareTypeRef(first.typeRef, second.typeRef) &&
    compareModifiers(first.modifiers, second.modifiers) &&
    compareAnnotations(first.annotations, second.annotations) &&
    first.id == second.id
  }
  def compareTypeRef(first: TypeRef, second: TypeRef): Boolean = {
    rules.atLeastOne(first, second)
  }
  def compareTypeRef(first: Option[TypeRef], second: Option[TypeRef]): Boolean = {
    rules.atLeastOne(OptionalTypeRef(first), OptionalTypeRef(second))
  }

  def compareAnnotations(first: Array[Annotation], second: Array[Annotation]): Boolean = {
    first.forall(f => second.exists(s => rules.forAll(f, s)))
  }
  def compareModifiers(first: Array[Modifier], second: Array[Modifier]): Boolean = {
    first.forall(f => second.exists(s => rules.forAll(f, s)))
  }
  def compareParamList(first: FormalParameterList, second: FormalParameterList): Boolean = {
    val typeRefs =
      first.formalParameters.map(_.typeRef) zip second.formalParameters.map(_.typeRef)

    val modifiers =
      first.formalParameters.map(m => m.modifiers) zip second.formalParameters.map(m => m.modifiers)
    val annotations =
      first.formalParameters.map(_.annotations) zip second.formalParameters.map(_.annotations)
    val ids = first.formalParameters.map(_.id) zip second.formalParameters.map(_.id)

    typeRefs.forall(tr => compareTypeRef(tr._1, tr._2)) && modifiers.forall(
      m => compareModifiers(m._1, m._2)
    ) && annotations.forall(a => compareAnnotations(a._1, a._2)) && ids.forall(f => f._1 == f._2)
  }

  private def compareAndThrow[T](rule: (T, T) => (Boolean, String), first: T, second: T): Unit = {
    val check = rule(first, second)
    if (!check._1) {
      throw new Exception(s"Objects Failed checks. Message: ${check._2}")
    }
  }
}

trait Rule[T] {
  def evaluate(first: T, second: T): Boolean = {
    first == second
  }
}

// Wrapper to allow pattern matching despite erasure
case class OptionalTypeRef(value: Option[TypeRef])

class ModifierRule   extends Rule[Modifier]
class AnnotationRule extends Rule[Annotation]
class TypeRefRule    extends Rule[OptionalTypeRef]

case class RuleSets(
  modifierRuleSet: Array[ModifierRule],
  annotationRuleSet: Array[AnnotationRule],
  typeRefRuleSet: Array[TypeRefRule]
) {

  def forAll[T](first: T, second: T): Boolean = {
    first match {
      case f: Annotation =>
        annotationRuleSet.forall(_.evaluate(f, second.asInstanceOf[Annotation]))
      case f: Modifier => modifierRuleSet.forall(_.evaluate(f, second.asInstanceOf[Modifier]))
      case f: TypeRef =>
        typeRefRuleSet.forall(
          _.evaluate(OptionalTypeRef(Some(f)), OptionalTypeRef(Some(second.asInstanceOf[TypeRef])))
        )
      case f: OptionalTypeRef =>
        typeRefRuleSet.forall(_.evaluate(f, second.asInstanceOf[OptionalTypeRef]))
    }
  }

  def atLeastOne[T](first: T, second: T): Boolean = {
    first match {
      case f: Annotation =>
        annotationRuleSet.exists(_.evaluate(f, second.asInstanceOf[Annotation]))
      case f: Modifier => modifierRuleSet.exists(_.evaluate(f, second.asInstanceOf[Modifier]))
      case f: TypeRef =>
        typeRefRuleSet.forall(
          _.evaluate(OptionalTypeRef(Some(f)), OptionalTypeRef(Some(second.asInstanceOf[TypeRef])))
        )
      case f: OptionalTypeRef =>
        typeRefRuleSet.exists(_.evaluate(f, second.asInstanceOf[OptionalTypeRef]))
    }
  }
}

object RuleSets {
  def apply: RuleSets = {
    new RuleSets(
      Array(new ModifierRule),
      Array(new AnnotationRule),
      Array(
        new TypeRefNamesAreEqual,
        new TypeRefNamesAreEqualForPlatformTypes,
        new IgnoreSystemNamespace,
        new TypeRefOneIsVoidOtherIsNone
      )
    )
  }
}

class TypeRefNamesAreEqual() extends TypeRefRule {
  override def evaluate(first: OptionalTypeRef, second: OptionalTypeRef): Boolean = {
    first.value match {
      case Some(tr) =>
        val check =
          second.value.nonEmpty && tr.getFullName.equalsIgnoreCase(processSecond(second.value.get))
        check
      case None => second.value.isEmpty
    }
  }
  def processSecond(second: TypeRef): String = {
    second.getFullName
  }

}

class TypeRefNamesAreEqualForPlatformTypes() extends TypeRefNamesAreEqual {
  //We have to use toString for platform as getFullName will give the internal name
  override def processSecond(second: TypeRef): String = {
    second match {
      case decl: PlatformTypeDeclaration => decl.toString
      case _                             => second.getFullName
    }
  }
}

class IgnoreSystemNamespace() extends TypeRefNamesAreEqualForPlatformTypes {
  override def processSecond(second: TypeRef): String = {
    val processed = super.processSecond(second)
    if (processed.contains("System.")) {
      return processed.replaceAll("System.", "")
    }
    processed
  }
}

case class TypeRefOneIsVoidOtherIsNone() extends TypeRefRule {
  override def evaluate(first: OptionalTypeRef, second: OptionalTypeRef): Boolean = {
    first.value match {
      case Some(tr) =>
        if (tr.getFullName.equalsIgnoreCase(Names.Void.value)) second.value.isEmpty
        else first == second
      case _ =>
        first == second
    }
  }
}
