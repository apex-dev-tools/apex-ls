/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.financialforce.oparser

import com.financialforce.oparser.testutil._
import org.scalatest.funsuite.AnyFunSuite

class JorjeSmokeTest extends AnyFunSuite {
  private val path = "Dummy.cls"

  test("Classes") {
    val content =
      """
        | public abstract class Dummy extends Bar implements Baz, Boo{
        |   static {
        |     Bar bar = new Bar();
        |   }
        |   static {}
        |   {}
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

    val op         = OutlineParser.parse(path, content, TestClassFactory, ctx = null)._3.get
    val sfp        = JorjeParser(path, content).parse._1.head
    val comparator = SubsetComparator(op)
    comparator.unresolvedSubsetOf(sfp)

    assert(comparator.getWarnings.isEmpty, "Warnings are not empty")

  }

  test("class sharing") {
    val content =
      """
        | public with sharing class Dummy extends Baz {
        |  private Boo test(){return new Boo();}
        |
        | }
        |""".stripMargin

    val op         = OutlineParser.parse(path, content, TestClassFactory, ctx = null)._3.get
    val sfp        = JorjeParser(path, content).parse._1.head
    val comparator = SubsetComparator(op)
    comparator.unresolvedSubsetOf(sfp)

    assert(comparator.getWarnings.isEmpty, "Warnings are not empty")
  }

  test("Interface") {
    val content =
      """
        | public interface Dummy extends Baz {
        |   void add();
        |   Integer multiply(Integer a, Integer b);
        |   void minus();
        | }
        |""".stripMargin

    val op         = OutlineParser.parse(path, content, TestClassFactory, ctx = null)._3.get
    val sfp        = JorjeParser(path, content).parse._1.head
    val comparator = SubsetComparator(op)
    comparator.unresolvedSubsetOf(sfp)

    assert(comparator.getWarnings.isEmpty, "Warnings are not empty")
  }

  test("Enums") {
    val content =
      """
        | public enum Season {
        |    WINTER,
        |    SPRING,
        |    SUMMER,
        |    FALL
        | }
        |""".stripMargin

    val op         = OutlineParser.parse(path, content, TestClassFactory, ctx = null)._3.get
    val sfp        = JorjeParser(path, content).parse._1.head
    val comparator = SubsetComparator(op)
    comparator.unresolvedSubsetOf(sfp)

    assert(comparator.getWarnings.isEmpty, "Warnings are not empty")
  }

  test("Classes with multiple static initializers") {
    val content =
      """
        | public class Dummy {
        |   static String a;
        |   static {
        |     Dummy a = new Dummy;
        |     {}
        |   }
        |   static {
        |     Dummy a = new Dummy;
        |   }
        | }
        |""".stripMargin

    val op         = OutlineParser.parse(path, content, TestClassFactory, ctx = null)._3.get
    val sfp        = JorjeParser(path, content).parse._1.head
    val comparator = SubsetComparator(op)
    comparator.unresolvedSubsetOf(sfp)

    assert(comparator.getWarnings.isEmpty, "Warnings are not empty")
  }

  test("Classes with multiple instance initializers") {
    val content =
      """
        | public class Dummy {
        |   String a;
        |   {
        |     Dummy a = new Dummy;
        |     {}
        |   }
        |   {
        |     Dummy a = new Dummy;
        |   }
        | }
        |""".stripMargin

    val op         = OutlineParser.parse(path, content, TestClassFactory, ctx = null)._3.get
    val sfp        = JorjeParser(path, content).parse._1.head
    val comparator = SubsetComparator(op)
    comparator.unresolvedSubsetOf(sfp)

    assert(comparator.getWarnings.isEmpty, "Warnings are not empty")
  }

}
