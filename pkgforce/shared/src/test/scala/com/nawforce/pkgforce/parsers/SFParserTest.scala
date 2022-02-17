package com.nawforce.pkgforce.parsers

import com.financialforce.oparser.{
  ClassTypeDeclaration,
  Compare,
  EnumTypeDeclaration,
  InterfaceTypeDeclaration,
  OutlineParser
}
import com.nawforce.runtime.parsers.SourceData
import com.nawforce.runtime.platform.Path
import com.nawforce.runtime.sfparser.SFParser
import org.scalatest.funsuite.AnyFunSuite

class SFParserTest extends AnyFunSuite {

  test("Classes") {
    val path = Path("Dummy.cls")
    val content =
      """
        | public abstract class Dummy extends Bar implements Baz, Boo{

        |   private Foo f = new Foo();
        |   public string prop {get; set;}
        |   public static final String s = 's';
        |   private Season seas = Season.FALL;
        |
        |   public Dummy(){}
        |   public Dummy(Integer a) {}
        |   public Dummy(String q){}
        |
        |   public abstract void abs();
        |   public abstract void vMethod(String a, Abc b);
        |   public boolean bMethod() {}
        |   public static void sMethod(){}
        |   private void pMethod(){}
        |
        |   private class Inner {
        |     public String b = 'b';
        |   }
        |
        |   private class SecondInner {}
        |
        |   private enum Season {
        |      WINTER,
        |      SPRING,
        |      SUMMER,
        |      FALL
        |   }
        |   private interface innerInter {
        |      void add();
        |      Integer minus();
        |   }
        | }
        |""".stripMargin

    val op  = OutlineParser.parse(path.basename, content)._3.get
    val sfp = SFParser(path, SourceData(content)).parse.get

    Compare.compareClassTypeDeclarations(
      op.asInstanceOf[ClassTypeDeclaration],
      sfp.asInstanceOf[ClassTypeDeclaration]
    )

  }

  test("Interface") {
    val path = Path("Dummy.cls")
    val content =
      """
        | public interface Dummy extends Baz {
        |   void add();
        |   Integer multiply(Integer a, Integer b);
        |   void minus();
        | }
        |""".stripMargin

    val op  = OutlineParser.parse(path.basename, content)._3.get
    val sfp = SFParser(path, SourceData(content)).parse.get
    Compare.compareInterfaceTypeDeclarations(
      op.asInstanceOf[InterfaceTypeDeclaration],
      sfp.asInstanceOf[InterfaceTypeDeclaration]
    )
    println(sfp)
  }

  test("Enums") {
    val path = Path("Dummy.cls")
    val content =
      """
        | public enum Season {
        |    WINTER,
        |    SPRING,
        |    SUMMER,
        |    FALL
        | }
        |""".stripMargin

    val op  = OutlineParser.parse(path.basename, content)._3.get
    val sfp = SFParser(path, SourceData(content)).parse.get
    Compare.compareEnumTypeDeclarations(
      op.asInstanceOf[EnumTypeDeclaration],
      sfp.asInstanceOf[EnumTypeDeclaration]
    )
    println(sfp)
  }

}
