/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.runtime.sfparser

import com.financialforce.oparser._
import com.nawforce.runtime.platform.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class SFParserTest extends AnyFunSuite with BeforeAndAfter {
  before {
    SubsetCompare.clearWarnings()
  }

  test("Classes") {
    val path = Path("Dummy.cls")
    val content =
      """
        | public abstract class Dummy extends Bar implements Baz, Boo{
        |   static {
        |   Bar bar = new Bar();
        |   }
        |   {
        |   Foo b = new Foo();
        |   }
        |   @TestVisible
        |   private Foo f = new Foo();
        |   public string prop {get; set;}
        |   public static final String s = 's';
        |   private Dummy.Season seas = Season.FALL;
        |
        |   public Dummy(){}
        |   public Dummy(Integer a) {}
        |   public Dummy(String q){}
        |
        |   public abstract void abs();
        |   public abstract void vMethod(String[] a, Abc b);
        |   public List<boolean> bMethod() {}
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
        |      Boo add();
        |      Integer minus();
        |   }
        | }
        |""".stripMargin

    val op  = OutlineParser.parse(path.basename, content)._3.get
    val sfp = SFParser(path.basename, content).parse._1.head

    SubsetCompare.subsetOffClassDeclarations(
      op.asInstanceOf[ClassTypeDeclaration],
      sfp.asInstanceOf[ClassTypeDeclaration]
    )

    assert(SubsetCompare.getWarnings.isEmpty, "Warnings are not empty")

  }

  test("class sharing") {
    val path = Path("Dummy.cls")
    val content =
      """
        | public with sharing class Dummy extends Baz {
        |  private Boo test(){return new Boo();}
        |
        | }
        |""".stripMargin

    val op  = OutlineParser.parse(path.basename, content)._3.get
    val sfp = SFParser(path.basename, content).parse._1.head
    SubsetCompare.subsetOffClassDeclarations(
      op.asInstanceOf[ClassTypeDeclaration],
      sfp.asInstanceOf[ClassTypeDeclaration]
    )
    assert(SubsetCompare.getWarnings.isEmpty, "Warnings are not empty")
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
    val sfp = SFParser(path.basename, content).parse._1.head
    SubsetCompare.compareInterfaceTypeDeclarations(
      op.asInstanceOf[InterfaceTypeDeclaration],
      sfp.asInstanceOf[InterfaceTypeDeclaration]
    )
    assert(SubsetCompare.getWarnings.isEmpty, "Warnings are not empty")
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
    val sfp = SFParser(path.basename, content).parse._1.head
    SubsetCompare.compareEnumTypeDeclarations(
      op.asInstanceOf[EnumTypeDeclaration],
      sfp.asInstanceOf[EnumTypeDeclaration]
    )
    assert(SubsetCompare.getWarnings.isEmpty, "Warnings are not empty")
  }

}
