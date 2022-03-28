/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.api.{BuildInfo, Org, Package, ServerOps, TypeSummary}
import com.nawforce.apexlink.cst.CompilationUnit
import com.nawforce.apexlink.deps.{DownWalker, TransitiveCollector}
import com.nawforce.apexlink.finding.TypeFinder
import com.nawforce.apexlink.finding.TypeResolver.TypeCache
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.plugins.PluginsManager
import com.nawforce.apexlink.rpc._
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexDeclaration,
  ApexFullDeclaration,
  TriggerDeclaration
}
import com.nawforce.apexlink.types.core.{TypeDeclaration, TypeId}
import com.nawforce.apexlink.types.other._
import com.nawforce.apexlink.types.platform.PlatformTypeDeclaration
import com.nawforce.apexlink.types.schema.{SObjectDeclaration, SchemaSObjectType}
import com.nawforce.apexparser.ApexParser
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.modifiers.{ISTEST_ANNOTATION, TEST_METHOD_MODIFIER}
import com.nawforce.pkgforce.names.{Name, TypeIdentifier, TypeName}
import com.nawforce.pkgforce.path.{PathLike, PathLocation}
import com.nawforce.pkgforce.pkgs.TriHierarchy
import com.nawforce.pkgforce.stream._
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}
import com.nawforce.runtime.parsers.{CodeParser, SourceData}
import com.nawforce.runtime.platform.Path

import java.nio.charset.StandardCharsets
import java.util
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing.MurmurHash3

/** Org/Package/Module hierarchy. This is based on generics to maintain consistency while we migrate features over
  * to pkgforce. The generics force the use of inner classes which is not so desirable,
  */
object OPM extends TriHierarchy {
  type TOrg     = OrgImpl
  type TPackage = PackageImpl
  type TModule  = Module

  class OrgImpl(val path: PathLike, initWorkspace: Option[Workspace])
      extends TriOrg
      with Org
      with OrgTestClasses {
    val workspace: Workspace = initWorkspace.getOrElse(new Workspace(Seq()))

    /** Issues log for all packages in org. This is managed independently as errors may be raised against files
      * for which there is no natural type representation.
      */
    private[nawforce] val issueManager = new IssuesManager

    /** Manager for post validation plugins */
    private[nawforce] val pluginsManager = new PluginsManager

    /** Parsed Apex data cache, the cache holds summary information about Apex types to speed startup */
    private[nawforce] val parsedCache =
      ParsedCache.create(MurmurHash3.stringHash(BuildInfo.implementationBuild)) match {
        case Right(pc) => Some(pc)
        case Left(err) => LoggerOps.info(err); None
      }

    /** Is this Org using auto-flushing of the parsedCache. */
    private val autoFlush = ServerOps.getAutoFlush

    /** The Org flusher. */
    private val flusher =
      if (autoFlush) new CacheFlusher(this, parsedCache) else new Flusher(this, parsedCache)

    /** Packages in org in deploy order, the last entry is the unmanaged package identified by namespace = None */
    override val packages: ArraySeq[PackageImpl] = {

      def createModule(
        pkg: PackageImpl,
        dependencies: ArraySeq[Module],
        index: DocumentIndex
      ): Module = {
        new Module(pkg, dependencies, index)
      }

      OrgInfo.current.withValue(this) {

        // Fold over layers to create packages - with any package(namespace) dependencies linked to each package
        // The workspace layers form a deploy ordering, so each is dependent on all previously created
        val declared =
          workspace.layers.foldLeft(ArraySeq[PackageImpl]())((acc, pkgLayer) => {
            acc :+ new PackageImpl(
              this,
              pkgLayer.namespace,
              acc,
              workspace,
              ArraySeq.unsafeWrapArray(pkgLayer.layers.toArray),
              createModule,
              issueManager
            )
          })

        // If no unmanaged, create it
        val unmanaged =
          if (declared.isEmpty || declared.lastOption.exists(_.namespace.nonEmpty))
            Seq(
              new PackageImpl(
                this,
                None,
                declared,
                workspace,
                ArraySeq.empty,
                createModule,
                issueManager
              )
            )
          else
            Seq.empty
        ArraySeq.unsafeWrapArray((declared ++ unmanaged).toArray)
      }
    }

    /** After packages/module setup load metadata & flush to cache */
    OrgInfo.current.withValue(this) {
      packages.foreach(_.modules.foreach(_.freeze()))
      CodeParser.clearCaches()
      if (autoFlush)
        flusher.refreshAndFlush()
    }

    /** Get all loaded packages. */
    def getPackages(): Array[Package] = packages.toArray[Package]

    /** Provide access to IssueManager for org */
    override def issues: IssuesManager = issueManager

    /** Check to see if cache has been flushed */
    override def isDirty(): Boolean = flusher.isDirty

    /** Write dirty metadata to the cache, only works for manual flush orgs */
    def flush(): Boolean = {
      if (!autoFlush)
        flusher.refreshAndFlush()
      else
        false
    }

    /** Queue a metadata refresh request */
    def queueMetadataRefresh(request: RefreshRequest): Unit = {
      flusher.queue(request)
    }

    def getPackageForPath(path: String): Package = {
      packages.find(_.isPackagePath(path)).orNull
    }

    /** Get a array of type identifiers available across all packages. */
    def getTypeIdentifiers(apexOnly: Boolean): Array[TypeIdentifier] = {
      OrgInfo.current.withValue(this) {
        packages.foldLeft(Array[TypeIdentifier]())(
          (acc, pkg) => acc ++ pkg.getTypeIdentifiers(apexOnly)
        )
      }
    }

    /** Extract all dependencies */
    override def getDependencies: java.util.Map[String, Array[String]] = {
      OrgInfo.current.withValue(this) {
        val dependencies = new util.HashMap[String, Array[String]]()
        packages
          .filterNot(_.isGhosted)
          .foreach(_.orderedModules.head.populateDependencies(dependencies))
        dependencies
      }
    }

    /** Find a location for an identifier */
    override def getIdentifierLocation(identifier: TypeIdentifier): PathLocation = {
      OrgInfo.current.withValue(this) {
        (findTypeIdentifier(identifier) match {
          case Some(ad: ApexDeclaration) => Some(PathLocation(ad.location.path, ad.idLocation))
          case _                         => None
        }).orNull
      }
    }

    /** Search package modules for the TypeDeclaration matching a TypeIdentifier. */
    def findTypeIdentifier(identifier: TypeIdentifier): Option[TypeDeclaration] = {
      packagesByNamespace
        .get(identifier.namespace)
        .flatMap(pkg => {
          pkg.orderedModules.view.flatMap(_.moduleType(identifier.typeName)).headOption
        })
    }

    def getDependencyGraph(
      identifiers: Array[TypeIdentifier],
      depth: Integer,
      apexOnly: Boolean,
      ignoring: Array[TypeIdentifier]
    ): DependencyGraph = {
      OrgInfo.current.withValue(this) {
        val depWalker = new DownWalker(this, apexOnly)
        val nodeData = depWalker
          .walk(identifiers, depth, ignoring)
          .map(n => {
            DependencyNode(
              n.id,
              nodeFileSize(n.id),
              n.nature,
              n.transitiveCount,
              n.extending,
              n.implementing,
              n.using
            )
          })
        val nodeIndex = nodeData.map(_.identifier).zipWithIndex.toMap

        val linkData = new ArrayBuffer[DependencyLink]()
        nodeData.foreach(n => {
          val source = nodeIndex(n.identifier)

          def safeLink(nature: String)(identifier: TypeIdentifier): Unit = {
            nodeIndex
              .get(identifier)
              .foreach(
                target => if (source != target) linkData += DependencyLink(source, target, nature)
              )
          }

          n.extending.foreach(safeLink("extends"))
          n.implementing.foreach(safeLink("implements"))
          n.using.foreach(safeLink("uses"))
        })

        DependencyGraph(nodeData, linkData.toArray)
      }
    }

    private def nodeFileSize(identifier: TypeIdentifier): Int = {
      Option(getIdentifierLocation(identifier))
        .map(location => location.path.size.toInt)
        .getOrElse(0)
    }

    /** Locate a definition for a symbol */
    override def getDefinition(
      path: String,
      line: Int,
      offset: Int,
      content: String
    ): Array[LocationLink] = {
      if (path == null)
        return Array.empty

      OrgInfo.current.withValue(this) {
        packages
          .find(_.isPackagePath(path))
          .map(_.getDefinition(Path(path), line, offset, Option(content)))
          .getOrElse(Array.empty)
      }
    }

    override def getCompletionItems(
      path: String,
      line: Int,
      offset: Int,
      content: String
    ): Array[CompletionItemLink] = {
      getCompletionItemsInternal(Path(path), line, offset, content)
    }

    def getCompletionItemsInternal(
      path: PathLike,
      line: Int,
      offset: Int,
      content: String
    ): Array[CompletionItemLink] = {
      if (path == null || content == null)
        return Array.empty

      OrgInfo.current.withValue(this) {
        packages
          .find(_.isPackagePathInternal(path))
          .map(_.getCompletionItems(path, line, offset, content))
          .getOrElse(Array.empty)
      }
    }

    def getDependencyBombs(count: Int): Array[BombScore] = {
      val maxBombs   = Math.max(0, count)
      val allClasses = packages.flatMap(_.orderedModules.flatMap(_.nonTestClasses.toSeq))
      val bombs      = mutable.PriorityQueue[BombScore]()(Ordering.by(1000 - _.score))
      allClasses.foreach(cls => {
        if (!cls.inTest) {
          val score = cls.bombScore(allClasses.size)
          if (score._3 > 0)
            bombs.enqueue(BombScore(cls.typeId.asTypeIdentifier, score._2, score._1, score._3))
          if (bombs.size > maxBombs) {
            bombs.dequeue()
          }
        }
      })
      bombs.dequeueAll.toArray.reverse
    }

    def getDependencyCounts(
      paths: Array[String],
      excludeTestClasses: Boolean
    ): Array[(String, Int)] = {

      def getTypeAndSummaryOfPath(path: String): Option[(TypeIdentifier, TypeSummary)] =
        packages.view
          .flatMap(
            pkg =>
              Option(pkg.getTypeOfPath(path))
                .flatMap(
                  typeId =>
                    Option(pkg.getSummaryOfType(typeId))
                      .flatMap(summary => Option(typeId, summary))
                )
          )
          .headOption

      def countTransitiveDependencies(
        typeId: TypeIdentifier,
        transitiveDependencies: Array[TypeIdentifier]
      ): Int = {
        transitiveDependencies.count(t => t != typeId)
      }

      val collector = new TransitiveCollector(this, true)

      paths
        .flatMap { path =>
          getTypeAndSummaryOfPath(path)
            .filter {
              case (_, summary) =>
                !excludeTestClasses || !summary.modifiers.contains(ISTEST_ANNOTATION)
            }
            .map {
              case (typeId, _) => (typeId, collector.transitives(typeId))
            }
            .map {
              case (typeId, transitiveDependencies) =>
                (path, countTransitiveDependencies(typeId, transitiveDependencies))
            }
        }
    }

    def getAllTestMethods: Array[TestMethod] = {
      val allClasses = packages.flatMap(_.orderedModules.flatMap(_.testClasses.toSeq))

      allClasses
        .flatMap(
          c =>
            c.methods
              .filter(
                m =>
                  m.modifiers.contains(ISTEST_ANNOTATION) || m.modifiers
                    .contains(TEST_METHOD_MODIFIER)
              )
              .map(m => TestMethod(c.name.toString(), m.name.toString()))
        )
        .toSet
        .toArray
    }
  }

  class PackageImpl(
    val org: OrgImpl,
    val namespace: Option[Name],
    override val basePackages: ArraySeq[PackageImpl],
    workspace: Workspace,
    layers: ArraySeq[ModuleLayer],
    mdlFactory: (PackageImpl, ArraySeq[Module], DocumentIndex) => Module,
    logger: IssueLogger
  ) extends TriPackage
      with PackageAPI
      with DefinitionProvider
      with CompletionProvider {

    val modules: ArraySeq[Module] =
      layers
        .foldLeft(ArraySeq[Module]())((acc, layer) => {
          val issuesAndIndex = workspace.indexes(layer)
          logger.logAll(issuesAndIndex.issues)
          val module = mdlFactory(this, acc, issuesAndIndex.value)
          acc :+ module
        })

    /** Is this or any base package of this a ghost package. */
    lazy val hasGhosted: Boolean = isGhosted || basePackages.exists(_.hasGhosted)

    /** Find module for a path. */
    def getPackageModule(path: PathLike): Option[Module] = {
      orderedModules.find(_.isVisibleFile(path)) match {
        case Some(module) if MetadataDocument(path).nonEmpty => Some(module)
        case _                                               => None
      }
    }

    /** Get summary of package context containing namespace & base package namespace information. */
    def packageContext: PackageContext = {
      val ghostedPackages = basePackages
        .groupBy(_.isGhosted)
        .map(kv => (kv._1, kv._2.map(_.namespace.map(_.value).getOrElse("")).sorted.toArray))
      PackageContext(
        namespace.map(_.value),
        ghostedPackages.getOrElse(true, Array.empty),
        ghostedPackages.getOrElse(false, Array.empty)
      )
    }

    /** Set of namespaces used by this package and its base packages. */
    lazy val namespaces: Set[Name] = {
      namespace.toSet ++
        basePackages.flatMap(_.namespaces) ++
        PlatformTypeDeclaration.namespaces
    }

    /** Load a class to obtain it's FullDeclaration, issues are not updated, this just returns a temporary version of
      * the class so that it can be inspected.
      */
    protected def loadClass(
      path: PathLike,
      source: String
    ): (Option[(ApexParser, ApexParser.CompilationUnitContext)], Option[ApexFullDeclaration]) = {
      MetadataDocument(path) match {
        case Some(doc: ApexClassDocument) =>
          getPackageModule(path)
            .map(module => {
              val existingIssues = org.issueManager.pop(path)
              val parser         = CodeParser(doc.path, SourceData(source.getBytes(StandardCharsets.UTF_8)))
              val result         = parser.parseClassReturningParser()
              try {
                (
                  Some(result.value),
                  CompilationUnit
                    .construct(parser, module, doc.name, result.value._2)
                    .map(_.typeDeclaration)
                )
              } catch {
                case ex: Throwable =>
                  LoggerOps.info(s"CST construction failed for ${doc.path}", ex)
                  (None, None)
              } finally {
                org.issueManager.push(path, existingIssues)
              }
            })
            .getOrElse((None, None))
        case _ => (None, None)
      }
    }

    protected def loadTrigger(
      path: PathLike,
      source: String
    ): (Option[(ApexParser, ApexParser.TriggerUnitContext)], Option[ApexFullDeclaration]) = {
      MetadataDocument(path) match {
        case Some(doc: ApexTriggerDocument) =>
          getPackageModule(path)
            .map(module => {
              val existingIssues = org.issueManager.pop(path)
              val parser         = CodeParser(doc.path, SourceData(source.getBytes(StandardCharsets.UTF_8)))
              val result         = parser.parseTriggerReturningParser()
              try {
                (Some(result.value), TriggerDeclaration.construct(parser, module, result.value._2))
              } catch {
                case ex: Throwable =>
                  LoggerOps.info(s"CST construction failed for ${doc.path}", ex)
                  (None, None)
              } finally {
                org.issueManager.push(path, existingIssues)
              }
            })
            .getOrElse((None, None))
        case _ => (None, None)
      }
    }
  }

  class Module(
    override val pkg: PackageImpl,
    override val dependents: ArraySeq[Module],
    val index: DocumentIndex
  ) extends TriModule
      with ModuleRefresh
      with ModuleFind
      with TypeFinder
      with ModuleCompletions {

    def namespaces: Set[Name] = pkg.namespaces

    private[nawforce] var types = mutable.Map[TypeName, TypeDeclaration]()
    private val schemaManager   = SchemaSObjectType(this)

    def freeze(): Unit = {
      // FUTURE: Have return types, currently can't be done because class loading code needs access to in-flight types
      upsertMetadata(AnyDeclaration(this))
      upsertMetadata(schemaSObjectType)
      upsertMetadata(schemaSObjectType, Some(TypeName(schemaSObjectType.name)))

      new StreamDeployer(this, PackageStream.eventStream(index), types)
    }

    def schemaSObjectType: SchemaSObjectType = schemaManager

    def any: AnyDeclaration = types(TypeNames.Any).asInstanceOf[AnyDeclaration]

    def labels: LabelDeclaration = types(TypeNames.Label).asInstanceOf[LabelDeclaration]

    def interviews: InterviewDeclaration =
      types(TypeNames.Interview).asInstanceOf[InterviewDeclaration]

    def pages: PageDeclaration = types(TypeNames.Page).asInstanceOf[PageDeclaration]

    def components: ComponentDeclaration =
      types(TypeNames.Component).asInstanceOf[ComponentDeclaration]

    def nonTestClasses: Iterable[ApexClassDeclaration] =
      types.values.collect {
        case ac: ApexClassDeclaration if !ac.inTest => ac
      }

    def testClasses: Iterable[ApexClassDeclaration] =
      types.values.collect {
        case ac: ApexClassDeclaration if ac.inTest => ac
      }

    override def isVisibleFile(path: PathLike): Boolean = {
      index.isVisibleFile(path)
    }

    /** Count of loaded types, for debug info */
    def typeCount: Int = types.size

    /* Iterator over available types */
    def getTypes: Iterable[TypeDeclaration] = {
      types.values
    }

    /** Iterate metadata defined types, this will include referenced platform SObjects irrespective of if they have been
      * extended or not which is perhaps not quite accurate to the method name.
      */
    def getMetadataDefinedTypeIdentifiers(apexOnly: Boolean): Iterable[TypeIdentifier] = {
      types.values
        .collect {
          case x: ApexDeclaration                 => x
          case x: SObjectDeclaration if !apexOnly => x
        }
        .map(td => TypeIdentifier(namespace, td.typeName))
    }

    /* Search for a specific outer or inner type */
    def moduleType(typeName: TypeName): Option[TypeDeclaration] = {
      types
        .get(typeName)
        .orElse(
          typeName.outer
            .flatMap(types.get)
            .flatMap(_.nestedTypes.find(_.typeName == typeName))
        )
    }

    def replaceType(typeName: TypeName, typeDeclaration: Option[TypeDeclaration]): Unit = {
      if (typeDeclaration.nonEmpty) {
        val td = typeDeclaration.get
        types.put(typeName, td)

        // Labels must also be updated against just 'Label' to simulate the defaulting of the System namespace, this is
        // a design flaw but I am living it for now, if this is skipped Label would resolve against an empty platform
        // type.
        typeName match {
          case TypeNames.Label => types.put(TypeName(labels.name), labels)
          case _               => ()
        }

      } else {
        types.remove(typeName)
      }
    }

    // Upsert some metadata to the package
    def upsertMetadata(td: TypeDeclaration, altTypeName: Option[TypeName] = None): Unit = {
      types.put(altTypeName.getOrElse(td.typeName), td)
    }

    // Remove some metadata from the package
    // Future: Support component & flow removal
    def removeMetadata(td: TypeDeclaration): Unit = {
      removeMetadata(td.typeName)
    }

    def removeMetadata(typeName: TypeName): Unit = {
      types.remove(typeName)
    }

    // Add dependencies for Apex types to a map
    def populateDependencies(dependencies: java.util.Map[String, Array[String]]): Unit = {
      val typeCache = new TypeCache()
      types.values.foreach {
        case td: ApexClassDeclaration =>
          val depends = mutable.Set[TypeId]()
          td.gatherDependencies(depends, apexOnly = false, outerTypesOnly = true, typeCache)
          depends.remove(td.typeId)
          if (depends.nonEmpty)
            dependencies.put(td.typeName.toString, depends.map(_.typeName.toString).toArray)
        case _ => ()
      }
    }
  }
}
