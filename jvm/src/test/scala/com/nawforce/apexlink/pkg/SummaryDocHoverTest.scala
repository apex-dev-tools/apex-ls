/*
 Copyright (c) 2026 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.pkg

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.org.OPM
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.apexlink.types.apex.{FullDeclaration, SummaryDeclaration, SummaryDocumented}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Environment
import org.scalatest.funsuite.AnyFunSuite
import upickle.default.{readBinary, writeBinary}

import java.nio.charset.StandardCharsets
import scala.collection.immutable.ArraySeq
import scala.util.hashing.MurmurHash3

/** Documentation shown on hover must survive a warm load, where cross-file targets are summaries
  * built from the parsed cache and hold no source, and must never be recovered from a file that no
  * longer matches the summary.
  */
class SummaryDocHoverTest extends AnyFunSuite {

  private val dummy =
    """/** A dummy class, café. */
      |public class Dummy {
      |  /** Builds a dummy. */
      |  public Dummy(Integer a) {}
      |  public Dummy() {}
      |  /** Runs it. */
      |  public void run() {}
      |  public void plain() {}
      |  /** The name. */
      |  public String name;
      |  /** A count. */
      |  public Integer count { get; set; }
      |  /** Nested type. */
      |  public class Inner {
      |    /** Goes. */
      |    public void go() {}
      |  }
      |}""".stripMargin

  private val foo =
    "public class Foo { void f() { Dummy d = new Dummy(1); new Dummy(); d.run(); d.plain(); " +
      "String s = d.name; Integer c = d.count; Dummy.Inner i = new Dummy.Inner(); i.go(); " +
      "Dummy.toString(); } }"

  private val sources = Map("Dummy.cls" -> dummy, "Foo.cls" -> foo)

  private val documented = Seq(
    "Dummy.toString" -> "A dummy class, café.",
    "Dummy(1)"       -> "Builds a dummy.",
    "run()"          -> "Runs it.",
    "name;"          -> "The name.",
    "count;"         -> "A count.",
    "Inner()"        -> "Nested type.",
    "go()"           -> "Goes."
  )
  private val undocumented = Seq("Dummy()", "plain()")

  private def withIsolatedRuntime[T](op: => T): T = {
    val originalCache     = Environment.getCacheDirOverride
    val originalAutoFlush = ServerOps.isAutoFlushEnabled
    val originalParser    = ServerOps.getCurrentParser
    Environment.setCacheDirOverride(Some(None))
    try op
    finally {
      Environment.setCacheDirOverride(originalCache)
      ServerOps.setAutoFlush(originalAutoFlush)
      ServerOps.setCurrentParser(originalParser)
    }
  }

  private def openOrg(root: PathLike, parser: String): OPM.OrgImpl = {
    val options = OpenOptions
      .default()
      .withParser(parser)
      .withAutoFlush(enabled = false)
      .withCacheDirectory(root.join(".cache").toString)
      .withCache(true)
    Org.newOrg(root, options).asInstanceOf[OPM.OrgImpl]
  }

  private def declaration(org: OPM.OrgImpl, name: String): Any =
    org.unmanaged.orderedModules.flatMap(_.findModuleType(TypeName(Name(name)))).head

  private def hover(org: OPM.OrgImpl, root: PathLike, probe: String): String = {
    val offset = foo.indexOf(probe)
    assert(offset >= 0, probe)
    org.unmanaged.getHover(root.join("Foo.cls"), line = 1, offset + 1, None).content.get
  }

  private def hovers(org: OPM.OrgImpl, root: PathLike): Map[String, String] =
    (documented.map(_._1) ++ undocumented).map(probe => probe -> hover(org, root, probe)).toMap

  private def signature(content: String): String = content.split("\n\n").head

  private def withWarmOrg(
    parser: String
  )(op: (PathLike, OPM.OrgImpl, Map[String, String]) => Unit): Unit = {
    withIsolatedRuntime {
      FileSystemHelper.runTempDir(sources, setupCache = true) { root: PathLike =>
        val cold = openOrg(root, parser)
        assert(declaration(cold, "Dummy").isInstanceOf[FullDeclaration])
        val coldHovers = hovers(cold, root)
        cold.flush()

        val warm = openOrg(root, parser)
        assert(declaration(warm, "Dummy").isInstanceOf[SummaryDeclaration])
        op(root, warm, coldHovers)
      }
    }
  }

  Seq(OutlineParserSingleThreaded.shortName, ANTLRParser.shortName).foreach(parser => {
    test(s"warm hover shows the same documentation as a cold load ($parser)") {
      withWarmOrg(parser) { (root, warm, coldHovers) =>
        documented.foreach { case (probe, doc) =>
          assert(coldHovers(probe).endsWith(s"\n\n$doc"), probe)
        }
        undocumented.foreach(probe => assert(!coldHovers(probe).contains("\n\n"), probe))
        assert(hovers(warm, root) == coldHovers)
      }
    }
  })

  test("warm hover omits documentation once the source no longer matches the summary") {
    withWarmOrg(OutlineParserSingleThreaded.shortName) { (root, warm, coldHovers) =>
      root.join("Dummy.cls").write(dummy.replace("Runs it.", "Walks it."))
      assert(declaration(warm, "Dummy").isInstanceOf[SummaryDeclaration])
      assert(hovers(warm, root) == coldHovers.map { case (k, v) => k -> signature(v) })
    }
  }

  test("warm hover omits documentation when the source is missing") {
    withWarmOrg(OutlineParserSingleThreaded.shortName) { (root, warm, coldHovers) =>
      root.join("Dummy.cls").delete()
      assert(hovers(warm, root) == coldHovers.map { case (k, v) => k -> signature(v) })
    }
  }

  test("reading summary documentation validates hash and range") {
    FileSystemHelper.runTempDir(Map("Dummy.cls" -> dummy)) { root: PathLike =>
      val path  = root.join("Dummy.cls")
      val bytes = dummy.getBytes(StandardCharsets.UTF_8)
      val hash  = MurmurHash3.bytesHash(bytes)
      val start = dummy.indexOf("/** Runs")
      val doc   = DocSummary(start, "/** Runs it. */".length)

      // Starts after the non-ASCII character so byte and char offsets differ
      assert(bytes.length != dummy.length)
      val byteStart = new String(bytes, StandardCharsets.UTF_8)
        .substring(0, start)
        .getBytes(StandardCharsets.UTF_8)
        .length
      val byteDoc = DocSummary(byteStart, doc.length)
      assert(SummaryDocumented.read(path, hash, byteDoc).contains("/** Runs it. */"))

      assert(SummaryDocumented.read(path, hash + 1, byteDoc).isEmpty)
      assert(SummaryDocumented.read(path, hash, doc).isEmpty)
      assert(SummaryDocumented.read(path, hash, DocSummary(-1, 5)).isEmpty)
      assert(SummaryDocumented.read(path, hash, DocSummary(byteStart, 0)).isEmpty)
      assert(SummaryDocumented.read(path, hash, DocSummary(bytes.length - 2, 10)).isEmpty)
      assert(SummaryDocumented.read(path, hash, DocSummary(Int.MaxValue, Int.MaxValue)).isEmpty)
      assert(SummaryDocumented.read(root.join("Missing.cls"), hash, byteDoc).isEmpty)
    }
  }

  test("summaries record doc offsets rather than doc text") {
    withWarmOrg(OutlineParserSingleThreaded.shortName) { (_, warm, _) =>
      val summary = declaration(warm, "Dummy").asInstanceOf[SummaryDeclaration].summary
      val bytes   = dummy.getBytes(StandardCharsets.UTF_8)
      def text(doc: Option[DocSummary]): String =
        new String(bytes, doc.get.offset, doc.get.length, StandardCharsets.UTF_8)

      assert(text(summary.doc) == "/** A dummy class, café. */")
      assert(text(summary.methods.find(_.name == "run").get.doc) == "/** Runs it. */")
      assert(summary.methods.find(_.name == "plain").get.doc.isEmpty)
      assert(text(summary.fields.find(_.name == "name").get.doc) == "/** The name. */")
      assert(text(summary.fields.find(_.name == "count").get.doc) == "/** A count. */")
      assert(
        text(summary.constructors.find(_.parameters.nonEmpty).get.doc) == "/** Builds a dummy. */"
      )
      assert(summary.constructors.find(_.parameters.isEmpty).get.doc.isEmpty)
      assert(text(summary.nestedTypes.head.doc) == "/** Nested type. */")
      assert(text(summary.nestedTypes.head.methods.head.doc) == "/** Goes. */")

      val encoded = writeBinary(ApexSummary(summary, Array()))
      assert(!new String(encoded, StandardCharsets.ISO_8859_1).contains("Runs it."))
    }
  }

  test("summary doc round-trips and defaults to absent") {
    val location = Location(1, 0, 1, 10)
    val doc      = Some(DocSummary(3, 12))
    val method = MethodSummary(
      location,
      location,
      "run",
      ArraySeq(),
      TypeName.Void,
      ArraySeq(),
      hasBlock = true,
      Array(),
      isSynthetic = false,
      doc
    )
    val field = FieldSummary(
      location,
      location,
      "name",
      com.nawforce.pkgforce.parsers.FIELD_NATURE,
      ArraySeq(),
      TypeNames.String,
      com.nawforce.pkgforce.modifiers.PUBLIC_MODIFIER,
      com.nawforce.pkgforce.modifiers.PUBLIC_MODIFIER,
      Array(),
      doc
    )
    val constructor = ConstructorSummary(location, location, ArraySeq(), ArraySeq(), Array(), doc)
    val tpe = TypeSummary(
      1,
      location,
      location,
      "Dummy",
      TypeName(Name("Dummy")),
      "class",
      ArraySeq(),
      inTest = false,
      None,
      ArraySeq(),
      ArraySeq(),
      ArraySeq(field),
      ArraySeq(constructor),
      ArraySeq(method),
      ArraySeq(),
      Array(),
      doc
    )
    assert(readBinary[TypeSummary](writeBinary(tpe)) == tpe)
    assert(readBinary[TypeSummary](writeBinary(tpe)).methods.head.doc == doc)

    val undocumented = TypeSummary(
      1,
      location,
      location,
      "Dummy",
      TypeName(Name("Dummy")),
      "class",
      ArraySeq(),
      inTest = false,
      None,
      ArraySeq(),
      ArraySeq(),
      ArraySeq(field.copy(doc = None)),
      ArraySeq(ConstructorSummary(location, location, ArraySeq(), ArraySeq(), Array())),
      ArraySeq(
        MethodSummary(
          location,
          location,
          "run",
          ArraySeq(),
          TypeName.Void,
          ArraySeq(),
          true,
          Array()
        )
      ),
      ArraySeq(),
      Array()
    )
    assert(undocumented.doc.isEmpty)
    assert(undocumented.constructors.head.doc.isEmpty)
    assert(undocumented.methods.head.doc.isEmpty)
    assert(readBinary[TypeSummary](writeBinary(undocumented)) == undocumented)
  }
}
