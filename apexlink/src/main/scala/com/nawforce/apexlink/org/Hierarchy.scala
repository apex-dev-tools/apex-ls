package com.nawforce.apexlink.org

import com.nawforce.apexlink.api.{Org, Package, ServerOps, TypeSummary}
import com.nawforce.apexlink.cst.CompilationUnit
import com.nawforce.apexlink.deps.{DownWalker, TransitiveCollector}
import com.nawforce.apexlink.finding.TypeResolver.{TypeCache, TypeResponse}
import com.nawforce.apexlink.finding.{TypeFinder, TypeResolver}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.plugins.PluginsManager
import com.nawforce.apexlink.rpc._
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexDeclaration, ApexFullDeclaration, FullDeclaration, TriggerDeclaration}
import com.nawforce.apexlink.types.core.{DependentType, TypeDeclaration, TypeId}
import com.nawforce.apexlink.types.other._
import com.nawforce.apexlink.types.platform.{PlatformTypeDeclaration, PlatformTypes}
import com.nawforce.apexlink.types.schema.{SObjectDeclaration, SchemaSObjectType}
import com.nawforce.apexparser.ApexParser
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.modifiers.{GLOBAL_MODIFIER, ISTEST_ANNOTATION, TEST_METHOD_MODIFIER}
import com.nawforce.pkgforce.names.{EncodedName, Name, TypeIdentifier, TypeName}
import com.nawforce.pkgforce.path.{PathLike, PathLocation}
import com.nawforce.pkgforce.pkgs.TriHierarchy
import com.nawforce.pkgforce.stream._
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}
import com.nawforce.runtime.parsers.{CodeParser, SourceData}
import com.nawforce.runtime.platform.Path

import java.io.File
import java.nio.charset.StandardCharsets
import java.util
import java.util.jar.JarFile
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.DynamicVariable
import scala.util.hashing.MurmurHash3

object Hierarchy extends TriHierarchy {
  type TOrg = OrgImpl
  type TPackage = PackageImpl
  type TModule = Module

  class OrgImpl(val path: PathLike, initWorkspace: Option[Workspace]) extends TriOrg with Org {
    val workspace: Workspace = initWorkspace.getOrElse(new Workspace(Seq()))

    /** Issues log for all packages in org. This is managed independently as errors may be raised against files
     * for which there is no natural type representation. */
    private[nawforce] val issueManager = new IssuesManager

    /** Manager for post validation plugins */
    private[nawforce] val pluginsManager = new PluginsManager

    /** Parsed Apex data cache, the cache holds summary information about Apex types to speed startup */
    private[nawforce] val parsedCache =
      ParsedCache.create(MurmurHash3.stringHash(OrgImpl.implementationBuild)) match {
        case Right(pc) => Some(pc)
        case Left(err) => LoggerOps.info(err); None
      }

    /** Is this Org using auto-flushing of the parsedCache. */
    private val autoFlush = ServerOps.getAutoFlush

    /** The Org flusher. */
    private val flusher =
      if (autoFlush) new CacheFlusher(this, parsedCache) else new Flusher(this, parsedCache)

    /** Lookup of available packages from the namespace (which must be unique), populated when packages created */
    var packagesByNamespace: Map[Option[Name], PackageImpl] = _

    val packages: Seq[PackageImpl] = {
      OrgImpl.current.withValue(this) {

        // Fold over layers to create packages - with any package(namespace) dependencies linked to each package
        // The workspace layers form a deploy ordering, so each is dependent on all previously created
        val declared =
          workspace.layers.foldLeft(Seq[PackageImpl]())((acc, pkgLayer) => {
            acc :+ new PackageImpl(this, pkgLayer.namespace, acc, workspace, pkgLayer.layers, createModule, issueManager)
          })

        // If no unmanaged, create it
        val unmanaged =
          if (declared.isEmpty || declared.lastOption.exists(_.namespace.nonEmpty))
            Seq(new PackageImpl(this, None, declared, workspace, Seq.empty, createModule, issueManager))
          else
            Seq.empty

        // Finally, freeze everything
        val packages = declared ++ unmanaged
        packagesByNamespace = packages.map(p => (p.namespace, p)).toMap
        packages.foreach(_.modules.foreach(_.freeze()))
        CodeParser.clearCaches()
        packages
      }
    }

    private def createModule(pkg: PackageImpl, index: DocumentIndex, dependencies: Seq[Module]): Module = {
      new Module(pkg, index, dependencies)
    }

    /** After loading packages we want to flush, but flushing depends on the package list being available. */
    private val initialFlush =
      if (autoFlush)
        flusher.refreshAndFlush()

    /** All orgs have an unmanaged package, it has to be the last entry in 'packages'. */
    val unmanaged: PackageImpl = packages.last

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
      OrgImpl.current.withValue(this) {
        packages.foldLeft(Array[TypeIdentifier]())((acc, pkg) => acc ++ pkg.getTypeIdentifiers(apexOnly))
      }
    }

    /** Extract all dependencies */
    override def getDependencies: java.util.Map[String, Array[String]] = {
      OrgImpl.current.withValue(this) {
        val dependencies = new util.HashMap[String, Array[String]]()
        packages
          .filterNot(_.isGhosted)
          .foreach(_.orderedModules.head.populateDependencies(dependencies))
        dependencies
      }
    }

    /** Find a location for an identifier */
    override def getIdentifierLocation(identifier: TypeIdentifier): PathLocation = {
      OrgImpl.current.withValue(this) {
        (findTypeIdentifier(identifier) match {
          case Some(ad: ApexDeclaration) => Some(PathLocation(ad.location.path, ad.idLocation))
          case _ => None
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

    def getDependencyGraph(identifiers: Array[TypeIdentifier], depth: Integer, apexOnly: Boolean, ignoring: Array[TypeIdentifier]): DependencyGraph = {
      OrgImpl.current.withValue(this) {
        val depWalker = new DownWalker(this, apexOnly)
        val nodeData = depWalker
          .walk(identifiers, depth, ignoring)
          .map(n => {
            DependencyNode(n.id, nodeFileSize(n.id), n.nature, n.transitiveCount, n.extending, n.implementing, n.using)
          })
        val nodeIndex = nodeData.map(_.identifier).zipWithIndex.toMap

        val linkData = new ArrayBuffer[DependencyLink]()
        nodeData.foreach(n => {
          val source = nodeIndex(n.identifier)

          def safeLink(nature: String)(identifier: TypeIdentifier): Unit = {
            nodeIndex
              .get(identifier)
              .foreach(target => if (source != target) linkData += DependencyLink(source, target, nature))
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
    override def getDefinition(path: String, line: Int, offset: Int, content: String): Array[LocationLink] = {
      if (path == null)
        return Array.empty

      OrgImpl.current.withValue(this) {
        packages
          .find(_.isPackagePath(path))
          .map(_.getDefinition(Path(path), line, offset, Option(content)))
          .getOrElse(Array.empty)
      }
    }

    override def getCompletionItems(path: String, line: Int, offset: Int, content: String): Array[CompletionItemLink] = {
      if (path == null || content == null)
        return Array.empty

      OrgImpl.current.withValue(this) {
        packages
          .find(_.isPackagePath(path))
          .map(_.getCompletionItems(Path(path), line, offset, content))
          .getOrElse(Array.empty)
      }
    }

    def getDependencyBombs(count: Int): Array[BombScore] = {
      val maxBombs = Math.max(0, count)
      val allClasses = packages.flatMap(_.orderedModules.flatMap(_.nonTestClasses.toSeq))
      val bombs = mutable.PriorityQueue[BombScore]()(Ordering.by(1000 - _.score))
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

    def getTestClassNames(paths: Array[String], findTests: Boolean): Array[String] = {
      def findPackageIdentifierAndSummary(path: String): Option[(Package, TypeIdentifier, TypeSummary)] = {
        packages.view
          .flatMap(pkg => {
            Option(pkg.getTypeOfPath(path))
              .flatMap(typeId =>
                Option(pkg.getSummaryOfType(typeId))
                  .map(summary => (pkg, typeId, summary)))
          })
          .headOption
      }

      def findReferencedTestPaths(pkg: Package,
                                  typeId: TypeIdentifier,
                                  summary: TypeSummary,
                                  filterTypeId: TypeIdentifier): Array[String] = {
        if (summary.modifiers.contains(ISTEST_ANNOTATION)) return Array(summary.name)
        if (!findTests) return Array.empty

        Option(pkg.getDependencyHolders(typeId, apexOnly = true)).getOrElse(Array.empty).flatMap { dependentTypeId =>
          Option(pkg.getSummaryOfType(dependentTypeId)).toArray
            .filter { dependentSummary =>
              dependentSummary.modifiers.contains(ISTEST_ANNOTATION)
            }
            .filter { _ =>
              pkg.hasDependency(dependentTypeId, filterTypeId)
            }
            .map { dependentSummary =>
              dependentSummary.name
            }
        }
      }

      def targetsForInterfaces(pkg: Package,
                               summary: TypeSummary): ArraySeq[(TypeIdentifier, TypeIdentifier, TypeSummary)] = {
        summary.interfaces.flatMap { interface =>
          Option(pkg.getTypeIdentifier(interface))
            .flatMap { interfaceTypeId =>
              val outerTypeId = interfaceTypeId.typeName.outer.map(pkg.getTypeIdentifier).getOrElse(interfaceTypeId)
              Option(pkg.getSummaryOfType(outerTypeId))
                .map((interfaceTypeId, outerTypeId, _))
            }
        }
      }

      def targetsForSuperclass(pkg: Package,
                               summary: TypeSummary): Array[(TypeIdentifier, TypeIdentifier, TypeSummary)] = {
        summary.superClass
          .flatMap { tn =>
            Option(pkg.getTypeIdentifier(tn))
              .flatMap { tid =>
                Option(pkg.getSummaryOfType(tid))
                  .flatMap { summary =>
                    val otid = tid.typeName.outer.map(pkg.getTypeIdentifier).getOrElse(tid)
                    Some(Array((tid, otid, summary)) ++ targetsForInterfaces(pkg, summary))
                  }
              }
          }
          .getOrElse(Array.empty)
      }

      paths.flatMap { path =>
        findPackageIdentifierAndSummary(path).toArray.flatMap {
          case (pkg, typeId, summary) =>
            val interfaces = targetsForInterfaces(pkg, summary)
            val nestedInterfaces = summary.nestedTypes.flatMap { nestedSummary =>
              targetsForInterfaces(pkg, nestedSummary)
            }
            val superClassTargets = targetsForSuperclass(pkg, summary)

            val targets = Seq((typeId, typeId, summary)) ++ interfaces ++ nestedInterfaces ++ superClassTargets

            targets.flatMap {
              case (actualTypeId, outerTypeId, outerSummary) =>
                findReferencedTestPaths(pkg, outerTypeId, outerSummary, actualTypeId)
            }
        }
      }.distinct
    }

    def getDependencyCounts(paths: Array[String]): Array[(String, Int)] = {

      def getTypeOfPath(path: String): Option[TypeIdentifier] =
        packages.view.flatMap(pkg => Option(pkg.getTypeOfPath(path))).headOption

      def countTransitiveDependencies(typeId: TypeIdentifier, transitiveDependencies: Array[TypeIdentifier]): Int = {
        transitiveDependencies.count(t => t != typeId)
      }

      val collector = new TransitiveCollector(this, true)

      paths
        .flatMap { path =>
          getTypeOfPath(path)
            .map { typeId =>
              (typeId, collector.transitives(typeId))
            }
            .map {
              case (typeId, transitiveDependencies) => (path, countTransitiveDependencies(typeId, transitiveDependencies))
            }
        }
    }

    def getAllTestMethods: Array[TestMethod] = {
      val allClasses = packages.flatMap(_.orderedModules.flatMap(_.testClasses.toSeq))

      allClasses.flatMap(c => c.methods
        .filter(m => m.modifiers.contains(ISTEST_ANNOTATION) || m.modifiers.contains(TEST_METHOD_MODIFIER))
        .map(m => TestMethod(c.name.toString(), m.name.toString()))).toSet.toArray
    }
  }

  object OrgImpl {

    lazy val implementationBuild: String = {
      val sourceURI = classOf[OrgImpl].getProtectionDomain.getCodeSource.getLocation.toURI
      if (!sourceURI.toString.endsWith(".jar"))
        ""
      else {
        val manifest = new JarFile(new File(sourceURI)).getManifest
        Option(manifest.getMainAttributes.getValue("Implementation-Build")).getOrElse("")
      }
    }

    /** Access the in-scope Org */
    private[nawforce] val current: DynamicVariable[OrgImpl] = new DynamicVariable[OrgImpl](null)

    /** Log an issue against the in-scope org */
    private[nawforce] def log(issue: Issue): Unit = {
      if (issue.path != null)
        OrgImpl.current.value.issueManager.add(issue)
    }

    /** Log a general error against the in-scope org */
    private[nawforce] def logError(pathLocation: PathLocation, message: String): Unit = {
      log(new Issue(pathLocation.path, Diagnostic(ERROR_CATEGORY, pathLocation.location, message)))
    }
  }


  class PackageImpl(val org: OrgImpl, val namespace: Option[Name], override val basePackages: Seq[PackageImpl],
                    workspace: Workspace, layers: Seq[ModuleLayer],
                    mdlFactory: (PackageImpl, DocumentIndex, Seq[Module]) => Module, logger: IssueLogger)
    extends TriPackage with PackageAPI with DefinitionProvider with CompletionProvider {

    val modules: Seq[Module] =
      layers.foldLeft(Map[ModuleLayer, Module]())((acc, layer) => {
        val issuesAndIndex = workspace.indexes(layer)
        logger.logAll(issuesAndIndex.issues)
        val module = mdlFactory(this, issuesAndIndex.value, layers.flatMap(acc.get))
        acc + (layer -> module)
      }).values.toSeq

    /** Is this or any base package of this a ghost package. */
    lazy val hasGhosted: Boolean = isGhosted || basePackages.exists(_.hasGhosted)

    /** Find module for a path. */
    def getPackageModule(path: PathLike): Option[Module] = {
      orderedModules.find(_.isVisibleFile(path)) match {
        case Some(module) if MetadataDocument(path).nonEmpty => Some(module)
        case _ => None
      }
    }

    /** Get summary of package context containing namespace & base package namespace information. */
    def packageContext: PackageContext = {
      val ghostedPackages = basePackages
        .groupBy(_.isGhosted)
        .map(kv => (kv._1, kv._2.map(_.namespace.map(_.value).getOrElse("")).sorted.toArray))
      PackageContext(namespace.map(_.value),
        ghostedPackages.getOrElse(true, Array.empty),
        ghostedPackages.getOrElse(false, Array.empty))
    }

    /** Set of namespaces used by this package and its base packages. */
    lazy val namespaces: Set[Name] = {
      namespace.toSet ++
        basePackages.flatMap(_.namespaces) ++
        PlatformTypeDeclaration.namespaces
    }

    /* Check if a type is ghosted in this package */
    def isGhostedType(typeName: TypeName): Boolean = {
      if (typeName.outer.contains(TypeNames.Schema)) {
        val encName = EncodedName(typeName.name)
        basePackages.filter(_.isGhosted).exists(_.namespace == encName.namespace)
      } else {
        basePackages.filter(_.isGhosted).exists(_.namespace.contains(typeName.outerName)) ||
          typeName.params.exists(isGhostedType)
      }
    }

    /** Check if a field name is ghosted in this package. */
    def isGhostedFieldName(name: Name): Boolean = {
      EncodedName(name).namespace match {
        case None => false
        case Some(ns) => basePackages.filter(_.isGhosted).exists(_.namespace.contains(ns))
      }
    }

    /** Load a class to obtain it's FullDeclaration, issues are not updated, this just returns a temporary version of
     * the class so that it can be inspected. */
    protected def loadClass(path: PathLike, source: String)
    : (Option[(ApexParser, ApexParser.CompilationUnitContext)], Option[ApexFullDeclaration]) = {
      MetadataDocument(path) match {
        case Some(doc: ApexClassDocument) =>
          getPackageModule(path).map(module => {
            val existingIssues = org.issueManager.pop(path)
            val parser = CodeParser(doc.path, SourceData(source.getBytes(StandardCharsets.UTF_8)))
            val result = parser.parseClassReturningParser()
            try {
              (Some(result.value),
                CompilationUnit.construct(parser, module, doc.name, result.value._2).map(_.typeDeclaration))
            } catch {
              case ex: Throwable =>
                LoggerOps.info(s"CST construction failed for ${doc.path}", ex)
                (None, None)
            } finally {
              org.issueManager.push(path, existingIssues)
            }
          }).getOrElse((None, None))
        case _ => (None, None)
      }
    }

    protected def loadTrigger(path: PathLike, source: String)
    : (Option[(ApexParser, ApexParser.TriggerUnitContext)], Option[ApexFullDeclaration]) = {
      MetadataDocument(path) match {
        case Some(doc: ApexTriggerDocument) =>
          getPackageModule(path).map(module => {
            val existingIssues = org.issueManager.pop(path)
            val parser = CodeParser(doc.path, SourceData(source.getBytes(StandardCharsets.UTF_8)))
            val result = parser.parseTriggerReturningParser()
            try {
              (Some(result.value),
                TriggerDeclaration.construct(parser, module, result.value._2))
            } catch {
              case ex: Throwable =>
                LoggerOps.info(s"CST construction failed for ${doc.path}", ex)
                (None, None)
            } finally {
              org.issueManager.push(path, existingIssues)
            }
          }).getOrElse((None, None))
        case _ => (None, None)
      }
    }
  }

  class Module(override val pkg: PackageImpl, override val index: DocumentIndex, override val dependents: Seq[Module])
    extends TriModule with TypeFinder with ModuleCompletions {

    val basePackages: Seq[PackageImpl] = pkg.basePackages.reverse
    val namespace: Option[Name] = pkg.namespace

    def namespaces: Set[Name] = pkg.namespaces

    val baseModules: Seq[Module] = dependents.reverse

    private[nawforce] var types = mutable.Map[TypeName, TypeDeclaration]()
    private val schemaManager = SchemaSObjectType(this)

    def freeze(): Unit = {
      // FUTURE: Have return types, currently can't be done because class loading code needs access to in-flight types
      upsertMetadata(AnyDeclaration(this))
      upsertMetadata(schemaSObjectType)
      upsertMetadata(schemaSObjectType, Some(TypeName(schemaSObjectType.name)))

      new StreamDeployer(this, PackageStream.eventStream(index), types)
    }

    override def toString: String = s"Module(${index.path})"

    def schemaSObjectType: SchemaSObjectType = schemaManager

    def any: AnyDeclaration = types(TypeNames.Any).asInstanceOf[AnyDeclaration]

    def labels: LabelDeclaration = types(TypeNames.Label).asInstanceOf[LabelDeclaration]

    def interviews: InterviewDeclaration =
      types(TypeNames.Interview).asInstanceOf[InterviewDeclaration]

    def pages: PageDeclaration = types(TypeNames.Page).asInstanceOf[PageDeclaration]

    def components: ComponentDeclaration =
      types(TypeNames.Component).asInstanceOf[ComponentDeclaration]

    def nonTestClasses: Iterable[ApexClassDeclaration] = types.values.collect {
      case ac: ApexClassDeclaration if !ac.inTest => ac
    }

    def testClasses: Iterable[ApexClassDeclaration] = types.values.collect {
      case ac: ApexClassDeclaration if ac.inTest => ac
    }

    /** Count of loaded types, for debug info */
    def typeCount: Int = types.size

    /** Test if a file is visible to this module, i.e. in scope & not ignored */
    def isVisibleFile(path: PathLike): Boolean = {
      index.isVisibleFile(path)
    }

    /* Transitive Bases (dependent modules for this modules & its dependents) */
    def transitiveBaseModules: Set[Module] = {
      namespace
        .map(_ => dependents.toSet ++ dependents.flatMap(_.transitiveBaseModules))
        .getOrElse(baseModules.toSet)
    }

    /* Iterator over available types */
    def getTypes: Iterable[TypeDeclaration] = {
      types.values
    }

    /** Iterate metadata defined types, this will include referenced platform SObjects irrespective of if they have been
     * extended or not which is perhaps not quite accurate to the method name. */
    def getMetadataDefinedTypeIdentifiers(apexOnly: Boolean): Iterable[TypeIdentifier] = {
      types.values
        .collect {
          case x: ApexDeclaration => x
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
            .flatMap(_.nestedTypes.find(_.typeName == typeName)))
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
          case _ => ()
        }

      } else {
        types.remove(typeName)
      }
    }

    def isGhostedType(typeName: TypeName): Boolean = pkg.isGhostedType(typeName)

    /* Check if a field name is ghosted in this package */
    def isGhostedFieldName(name: Name): Boolean = pkg.isGhostedFieldName(name)

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

    /* Find a package/platform type. For general needs don't call this direct, use TypeRequest which will delegate here
     * if needed. This is the fallback handling for the TypeFinder which performs local searching for types, so this is
     * only useful if you know local searching is not required. */
    def findType(typeName: TypeName, from: TypeDeclaration): TypeResponse = {
      findType(typeName, Some(from))
    }

    def findType(typeName: TypeName): TypeResponse = {
      findType(typeName, None)
    }

    private def findType(typeName: TypeName, from: Option[TypeDeclaration]): TypeResponse = {
      if (namespace.nonEmpty) {
        val td = findPackageType(typeName.withTail(TypeName(namespace.get)), from).map(Right(_))
        if (td.nonEmpty)
          return td.get
      }

      val td = findPackageType(typeName, from).map(Right(_))
      if (td.nonEmpty)
        return td.get

      // From may be used to locate type variable types so must be accurate even for a platform type request
      from.map(TypeResolver.platformType(typeName, _)).orElse(Some(TypeResolver.platformTypeOnly(typeName, this))).get
    }

    // Find locally, or fallback to a searching base packages
    def findPackageType(typeName: TypeName, from: Option[TypeDeclaration], inPackage: Boolean = true): Option[TypeDeclaration] = {
      // Might be an outer in this module
      var declaration = findModuleType(typeName)
      if (declaration.nonEmpty) {
        if (inPackage || declaration.get.modifiers.contains(GLOBAL_MODIFIER))
          return declaration
        else
          return None
      }

      // Or maybe an inner
      if (typeName.outer.nonEmpty) {
        declaration = findPackageType(typeName.outer.get, from, inPackage = inPackage)
          .flatMap(_.findNestedType(typeName.name).filter(td => td.isExternallyVisible || inPackage))
        if (declaration.nonEmpty)
          return declaration
      }

      // Try base modules & packages of this module
      baseModules.view
        .flatMap(_.findPackageType(typeName, from))
        .headOption
        .orElse(
          basePackages.view
            .flatMap(pkg => pkg.orderedModules.lastOption.flatMap(_.findPackageType(typeName, from, inPackage = false)))
            .headOption)
    }

    /** Find a type just in this module. */
    def findModuleType(typeName: TypeName): Option[TypeDeclaration] = {
      // Use aliased type name here so we don't mishandle an ambiguous typename when searching
      val targetType = TypeNames.aliasOrReturn(typeName)

      // Direct hit
      var declaration = types.get(targetType)
      if (declaration.nonEmpty)
        return declaration

      // SObject and alike, we want module specific version of these
      declaration = types.get(targetType.withTail(TypeNames.Schema))
      if (declaration.nonEmpty)
        return declaration

      if (targetType.params.isEmpty && (targetType.outer.isEmpty || targetType.outer.contains(TypeNames.Schema))) {
        val encName = EncodedName(targetType.name).defaultNamespace(namespace)
        if (encName.ext.nonEmpty) {
          return types.get(TypeName(encName.fullName, Nil, Some(TypeNames.Schema)))
        }
      }
      None
    }

    def refreshInternal(existingLabels: LabelDeclaration): Seq[(TypeId, Set[TypeId])] = {
      val newLabels = createLabelDeclaration()
      val holders = existingLabels.getTypeDependencyHolders
      newLabels.updateTypeDependencyHolders(holders)
      replaceType(newLabels.typeName, Some(newLabels))
      newLabels.validate()
      Seq((newLabels.typeId, holders.toSet))
    }

    /* Replace a path, returns the TypeId of the type that was updated and a Set of TypeIds for the dependency
     * holders of that type. */
    def refreshInternal(path: PathLike): Seq[(TypeId, Set[TypeId])] = {
      PlatformTypes.withLoadingObserver(schemaSObjectType) {

        checkPathInPackageOrThrow(path)
        val doc = MetadataDocument(path).getOrElse(
          throw new IllegalArgumentException(s"Metadata type is not supported for '$path'"))
        val sourceOpt = resolveSource(path)
        val typeId = TypeId(this, doc.typeName(namespace))

        // Update internal document tracking
        index.upsert(pkg.org.issueManager, doc)
        if (sourceOpt.isEmpty)
          index.remove(doc)

        // Clear errors as might fail to create type, SObjects are handled later due to multiple files
        pkg.org.issueManager.pop(doc.path)

        // Create type & forward holders to limit need for invalidation chaining
        val newTypes = createTypes(doc, sourceOpt)
        if (newTypes.nonEmpty) {
          newTypes.map(newType => {
            val existingType = getDependentType(newType.typeName)
            val holders = existingType
              .map(_.getTypeDependencyHolders)
              .getOrElse(DependentType.emptyTypeDependencyHolders)
            newType.updateTypeDependencyHolders(holders)

            // Update and validate
            replaceType(newType.typeName, Some(newType))
            newType.validate()
            (typeId, holders.toSet)
          })
        } else {
          val existingType = getDependentType(typeId.typeName)
          val holders = existingType
            .map(_.getTypeDependencyHolders)
            .getOrElse(DependentType.emptyTypeDependencyHolders)
          removeTypes(doc)
          Seq((typeId, holders.toSet))
        }
      }
    }

    private def getDependentType(typeName: TypeName): Option[DependentType] = {
      types
        .get(typeName)
        .flatMap {
          case dt: DependentType => Some(dt)
          case _ => None
        }
    }

    private def createLabelDeclaration(): LabelDeclaration = {
      val events = LabelGenerator.iterator(index)
      val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
      LabelDeclaration(this).merge(stream)
    }

    private def createTypes(doc: MetadataDocument, source: Option[SourceData]): Seq[DependentType] = {
      doc match {
        case doc: ApexClassDocument =>
          source.flatMap(s => FullDeclaration.create(this, doc, s, forceConstruct = true)).toSeq
        case _: ApexTriggerDocument =>
          source.flatMap(s => TriggerDeclaration.create(this, doc.path, s)).toSeq
        case doc: SObjectLike =>
          if (doc.path.toString.endsWith("object-meta.xml"))
            refreshSObject(doc.path.parent)
          else
            refreshSObject(doc.path)
        case _: SObjectFieldDocument | _: SObjectFieldSetDocument | _: SObjectSharingReasonDocument =>
          val sObjectDir = doc.path.parent.parent
          MetadataDocument(sObjectDir.join(s"${sObjectDir.basename}.object-meta.xml")) match {
            case Some(_: SObjectLike) => refreshSObject(sObjectDir)
            case _ => Seq()
          }
        case _: LabelsDocument =>
          Seq(createLabelDeclaration())
        case _: PageDocument =>
          val events = PageGenerator.iterator(index)
          val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
          Seq(PageDeclaration(this).merge(stream))
        case _: ComponentDocument =>
          val events = ComponentGenerator.iterator(index)
          val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
          Seq(ComponentDeclaration(this).merge(stream))
        case _: FlowDocument =>
          val events = FlowGenerator.iterator(index)
          val stream = new PackageStream(ArraySeq.unsafeWrapArray(events.toArray))
          Seq(InterviewDeclaration(this).merge(stream))
      }
    }

    private def removeTypes(doc: MetadataDocument): Unit = {
      doc match {
        case doc: SObjectDocument =>
          if (doc.path.toString.endsWith("object-meta.xml"))
            removeSObjectTypes(doc.path.parent.basename)
          else
            removeSObjectTypes(doc.path.basename.replaceFirst("\\.object$", ""))
        case _: SObjectFieldDocument | _: SObjectFieldSetDocument | _: SObjectSharingReasonDocument =>
          val sObjectDir = doc.path.parent.parent
          removeSObjectTypes(sObjectDir.basename)
        case _ => types.remove(doc.typeName(namespace))
      }
    }

    private def removeSObjectTypes(sobjectName: String): Unit = {
      val name = EncodedName(sobjectName)
      if (name.ext.contains(Name("c"))) {
        val typeName = TypeName(name.fullName, Nil, Some(TypeNames.Schema))
        val objectNames = Seq(typeName,
          typeName.withNameReplace("__c$", "__Share"),
          typeName.withNameReplace("__c$", "__Feed"),
          typeName.withNameReplace("__c$", "__History"))
        objectNames.foreach(typeName => schemaSObjectType.remove(typeName.name))
        objectNames.foreach(types.remove)
      }
    }

    private def refreshSObject(sObjectPath: PathLike): Seq[DependentType] = {
      if (sObjectPath.exists) {
        clearSObjectErrors(sObjectPath)
        val deployer = new SObjectDeployer(this)
        val sobjects = deployer.createSObjects(
          SObjectGenerator.iterator(DocumentIndex(pkg.org.issueManager, namespace, sObjectPath)).buffered)

        sobjects.foreach(sobject => schemaSObjectType.add(sobject.typeName.name, hasFieldSets = true))
        sobjects.toIndexedSeq
      } else {
        Seq()
      }
    }

    private def clearSObjectErrors(path: PathLike): Unit = {
      if (!path.isDirectory) {
        pkg.org.issueManager.pop(path)
      } else {
        val (files, directories) = path.splitDirectoryEntries()
        files.foreach(file => pkg.org.issueManager.pop(file))
        directories.foreach(clearSObjectErrors)
      }
    }

    private def checkPathInPackageOrThrow(path: PathLike): Unit = {
      if (!index.isVisibleFile(path))
        throw new IllegalArgumentException(s"Metadata is not part of this package for '$path'")
    }

    private def resolveSource(path: PathLike): Option[SourceData] = {
      path.readSourceData() match {
        case Left(_) => None
        case Right(data) => Some(data)
      }
    }
  }


}
