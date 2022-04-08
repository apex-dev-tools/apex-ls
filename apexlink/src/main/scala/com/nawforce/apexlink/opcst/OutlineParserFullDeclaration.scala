package com.nawforce.apexlink.opcst

import com.financialforce.oparser.OutlineParser
import com.nawforce.apexlink.cst.{ClassDeclaration, EnumDeclaration, InterfaceDeclaration}
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.types.apex.{FullDeclaration, ThisType}
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents.ClassDocument
import com.nawforce.pkgforce.modifiers.ISTEST_ANNOTATION
import com.nawforce.pkgforce.names.TypeName
import com.nawforce.pkgforce.types.ModuleClassFactory
import com.nawforce.runtime.parsers.{Source, SourceData}
import com.nawforce.runtime.platform.OutlineParserModifierOps.{classModifiers, enumModifiers, interfaceModifiers}
import com.nawforce.runtime.workspace.{ClassTypeDeclaration, EnumTypeDeclaration, InterfaceTypeDeclaration, ModuleClassFactory}

object OutlineParserFullDeclaration {

  def toFullDeclaration(
    cls: ClassDocument,
    srcData: SourceData,
    module: OPM.Module
  ): Option[FullDeclaration] = {

    val contentsString: String = srcData.asString

    val (success, reason, td) =
      OutlineParser.parse(cls.path.toString, contentsString, ModuleClassFactory, null)
    val rv =
      if (!success) {
        LoggerOps.info(s"FAILED to parse ${cls.path.toString} $reason")
        None
      } else {
        td.get match {
          case ctd: ClassTypeDeclaration => toClassDeclaration(ctd, cls, srcData, module, None)
          case itd: InterfaceTypeDeclaration =>
            toInterfaceDeclaration(itd, cls, srcData, module, None)
          case etd: EnumTypeDeclaration => toEnumDeclaration(etd, cls, srcData, module, None)
          case _                          => None
        }
      }
    rv
  }

  private def toClassDeclaration(
                                  ctd: ClassTypeDeclaration,
                                  cls: ClassDocument,
                                  srcData: SourceData,
                                  module: OPM.Module,
                                  outerTypeName: Option[TypeName]
  ): Option[ClassDeclaration] = {

    val source: Source     = Source(cls.path, srcData, 0, 0, None)
    val thisTypeNameWithNS = TypeName(cls.name).withNamespace(module.namespace)

    val modifierResults =
      classModifiers(cls.path, ctd.id, ctd.annotations, ctd.modifiers, outerTypeName.isEmpty)

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
                                      itd: InterfaceTypeDeclaration,
                                      cls: ClassDocument,
                                      srcData: SourceData,
                                      module: OPM.Module,
                                      outerTypeName: Option[TypeName]
  ): Option[InterfaceDeclaration] = {
    val source: Source     = Source(cls.path, srcData, 0, 0, None)
    val thisTypeNameWithNS = TypeName(cls.name).withNamespace(module.namespace)

    val modifierResults =
      interfaceModifiers(cls.path, itd.id, itd.annotations, itd.modifiers, outerTypeName.isEmpty)

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
                                 etd: EnumTypeDeclaration,
                                 cls: ClassDocument,
                                 srcData: SourceData,
                                 module: OPM.Module,
                                 outerTypeName: Option[TypeName]
  ): Option[EnumDeclaration] = {
    val source: Source     = Source(cls.path, srcData, 0, 0, None)
    val thisTypeNameWithNS = TypeName(cls.name).withNamespace(module.namespace)

    val modifierResults =
      enumModifiers(cls.path, etd.id, etd.annotations, etd.modifiers, outerTypeName.isEmpty)

    val thisType =
      ThisType(module, thisTypeNameWithNS, modifierResults.modifiers.contains(ISTEST_ANNOTATION))

    val rv =
      OutlineParserEnumDeclaration.construct(etd, source, thisType, outerTypeName, modifierResults)
    Some(rv)
  }
}
