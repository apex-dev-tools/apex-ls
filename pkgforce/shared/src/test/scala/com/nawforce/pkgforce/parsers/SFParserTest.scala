package com.nawforce.pkgforce.parsers

import com.financialforce.oparser.{ClassTypeDeclaration, Compare, OutlineParser}
import com.nawforce.runtime.parsers.SourceData
import com.nawforce.runtime.platform.Path
import com.nawforce.runtime.sfparser.SFParser
import org.scalatest.funsuite.AnyFunSuite

class SFParserTest extends AnyFunSuite {

  test("Classes") {
    val path = Path("Dummy.cls")
    val content =
      """
        | public class Dummy extends Bar implements Baz, Boo{
        |   private Foo f = new Foo();
        |   public string prop {get; set;}
        |   public static String s = 's';
        |
        |
        |   public Dummy(){}
        |   public Dummy(Integer a) {}
        |   public Dummy(String q){}
        |
        |   public void vMethod(String a, Abc b){}
        |   public boolean bMethod() {}
        |   public static void sMethod(){}
        |   private void pMethod(){}
        |
        |   private class Inner {
        |     public String b = 'b';
        |   }
        |
        |   private class SecondInner {}
        | }
        |""".stripMargin

    val op  = OutlineParser.parse(path.basename, content)._3.get
    val sfp = SFParser(path, SourceData(content)).parseClass().get

    Compare.compareClassTypeDeclarations(
      op.asInstanceOf[ClassTypeDeclaration],
      sfp.asInstanceOf[ClassTypeDeclaration]
    )
  }

}
