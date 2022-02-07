package com.nawforce.apexlink.opcst

import com.financialforce.oparser.{
  OutlineParser,
  ClassTypeDeclaration => OPClassTypeDeclaration,
  EnumTypeDeclaration => OPEnumTypeDeclaration,
  InterfaceTypeDeclaration => OPInterfaceTypeDeclaration
}
import com.nawforce.apexlink.cst.{ClassDeclaration, EnumDeclaration, InterfaceDeclaration}
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.types.apex.{FullDeclaration, ThisType}
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents.ClassDocument
import com.nawforce.pkgforce.modifiers.ISTEST_ANNOTATION
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.runtime.parsers.{Source, SourceData}

object OutlineParserFullDeclaration {

  def toFullDeclaration(
    cls: ClassDocument,
    srcData: SourceData,
    module: OPM.Module
  ): Option[FullDeclaration] = {

    val contentsString: String = srcData.asString

    LoggerOps.info("Using outline parser")
    val (success, reason, td) = OutlineParser.parse(cls.path.toString, contentsString)
    val rv =
      if (!success) {
        LoggerOps.info(s"FAILED to parse ${cls.path.toString} $reason")
        None
      } else {
        td.get match {
          case ctd: OPClassTypeDeclaration => toClassDeclaration(ctd, cls, srcData, module, None)
          case itd: OPInterfaceTypeDeclaration =>
            toInterfaceDeclaration(itd, cls, srcData, module, None)
          case etd: OPEnumTypeDeclaration => toEnumDeclaration(etd, cls, srcData, module, None)
          case _                          => None
        }
      }
    rv
  }

  private def toClassDeclaration(
    ctd: OPClassTypeDeclaration,
    cls: ClassDocument,
    srcData: SourceData,
    module: OPM.Module,
    outerTypeName: Option[TypeName]
  ): Option[ClassDeclaration] = {

    val source: Source     = Source(cls.path, srcData, 0, 0, None)
    val thisTypeNameWithNS = TypeName(cls.name).withNamespace(module.namespace)

    val modifierResults =
      ModifierUtils.classModifiers(
        cls.path,
        ctd.id.get,
        ctd.annotations,
        ctd.modifiers,
        outerTypeName.isEmpty
      )

    val thisType =
      ThisType(module, thisTypeNameWithNS, modifierResults.modifiers.contains(ISTEST_ANNOTATION))

    val rv = OutlineParserClassDeclaration.construct(
      cls.path,
      ctd,
      source,
      thisType,
      outerTypeName,
      modifierResults
    )
    Some(rv)
  }

  private def toInterfaceDeclaration(
    itd: OPInterfaceTypeDeclaration,
    cls: ClassDocument,
    srcData: SourceData,
    module: OPM.Module,
    outerTypeName: Option[TypeName]
  ): Option[InterfaceDeclaration] = {
    val source: Source     = Source(cls.path, srcData, 0, 0, None)
    val thisTypeNameWithNS = TypeName(cls.name).withNamespace(module.namespace)

    val modifierResults =
      ModifierUtils.interfaceModifiers(
        cls.path,
        itd.id.get,
        itd.annotations,
        itd.modifiers,
        outerTypeName.isEmpty
      )

    val thisType =
      ThisType(module, thisTypeNameWithNS, modifierResults.modifiers.contains(ISTEST_ANNOTATION))

    val rv = OutlineParserInterfaceDeclaration.construct(
      cls.path,
      itd,
      source,
      thisType,
      outerTypeName,
      modifierResults
    )
    Some(rv)
  }

  private def toEnumDeclaration(
    etd: OPEnumTypeDeclaration,
    cls: ClassDocument,
    srcData: SourceData,
    module: OPM.Module,
    outerTypeName: Option[TypeName]
  ): Option[EnumDeclaration] = {
    val source: Source     = Source(cls.path, srcData, 0, 0, None)
    val thisTypeNameWithNS = TypeName(cls.name).withNamespace(module.namespace)

    val modifierResults =
      ModifierUtils.enumModifiers(
        cls.path,
        etd.id.get,
        etd.annotations,
        etd.modifiers,
        outerTypeName.isEmpty
      )

    val thisType =
      ThisType(module, thisTypeNameWithNS, modifierResults.modifiers.contains(ISTEST_ANNOTATION))

    val rv =
      OutlineParserEnumDeclaration.construct(etd, source, thisType, outerTypeName, modifierResults)
    Some(rv)
  }
}
