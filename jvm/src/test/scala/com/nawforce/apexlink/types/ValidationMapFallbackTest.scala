package com.nawforce.apexlink.types

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.cst.{
  Block,
  BodyDeclarationVerifyContext,
  ClassBodyDeclaration,
  ScopeVerifyContext
}
import com.nawforce.apexlink.cst.{ClassDeclaration => CSTClassDeclaration}
import com.nawforce.apexlink.types.apex.TriggerDeclaration
import com.nawforce.pkgforce.modifiers.ModifierResults
import com.nawforce.pkgforce.names.{Name, Names}
import com.nawforce.pkgforce.parsers.{ApexNode, Nature, CLASS_NATURE}
import com.nawforce.pkgforce.path.Location
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class ValidationMapFallbackTest extends AnyFunSuite with TestHelper {

  test("Trigger validation map returns empty when block verification fails") {
    val trigger = triggerDeclaration("trigger Dummy on Account (before insert) { }")
      .asInstanceOf[TriggerDeclaration]

    val throwingBlock = new ThrowingBlock
    throwingBlock.setLocation(trigger.location.path, 1, 0, 1, 0)

    val failingTrigger = trigger.copy(block = Some(throwingBlock))
    val validationMap  = failingTrigger.getValidationMap(1, 0)

    assert(validationMap.isEmpty)
    assert(throwingBlock.verifyCount == 1)
    assert(throwingBlock.lastThrown.exists(_.isInstanceOf[RuntimeException]))
  }

  test("Full declaration validation map returns empty when body validation fails") {
    val classDecl = typeDeclaration("public class Dummy { }")
      .asInstanceOf[CSTClassDeclaration]

    val throwingBody = new ThrowingBodyDeclaration
    throwingBody.setLocation(classDecl.location.path, 1, 0, 1, 0)

    val failingClass  = classDecl.copy(_bodyDeclarations = ArraySeq(throwingBody))
    val validationMap = failingClass.getValidationMap(1, 0)

    assert(validationMap.isEmpty)
    assert(throwingBody.verifyCount == 1)
    assert(throwingBody.lastThrown.exists(_.isInstanceOf[RuntimeException]))
  }

  private final class ThrowingBlock extends Block {
    private var _verifyCount: Int = 0

    override def statements(context: Option[ScopeVerifyContext]): Seq[Nothing] = Seq.empty

    override def verify(context: ScopeVerifyContext): Unit = {
      _verifyCount += 1
      val error = new RuntimeException("boom")
      _lastThrown = Some(error)
      throw error
    }

    private var _lastThrown: Option[RuntimeException] = None

    def verifyCount: Int                     = _verifyCount
    def lastThrown: Option[RuntimeException] = _lastThrown
  }

  private final class ThrowingBodyDeclaration
      extends ClassBodyDeclaration(ModifierResults(ArraySeq.empty, ArraySeq.empty)) {
    private var _verifyCount: Int                     = 0
    private var _lastThrown: Option[RuntimeException] = None

    override val children: scala.collection.compat.immutable.ArraySeq[ApexNode] =
      scala.collection.compat.immutable.ArraySeq.empty
    override val name: Name           = Names("ThrowingBody")
    override val nature: Nature       = CLASS_NATURE
    override def idLocation: Location = Location(1, 0, 1, 0)

    override protected def verify(context: BodyDeclarationVerifyContext): Unit = {
      _verifyCount += 1
      val error = new RuntimeException("boom")
      _lastThrown = Some(error)
      throw error
    }

    def verifyCount: Int                     = _verifyCount
    def lastThrown: Option[RuntimeException] = _lastThrown
  }
}
