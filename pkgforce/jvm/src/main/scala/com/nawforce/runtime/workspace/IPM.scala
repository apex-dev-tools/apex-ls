/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser._
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents.{ApexNature, DocumentIndex}
import com.nawforce.pkgforce.names.Name
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.pkgs.TriHierarchy
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration

import java.util.concurrent.{ExecutorService, Executors}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object IPM extends TriHierarchy {
  type TOrg     = Index
  type TPackage = Package
  type TModule  = Module

  class Index(val path: PathLike) extends TriOrg {

    val issues: IssuesManager = new IssuesManager

    val workspace: Workspace = {
      val wsAndIssues = Workspace.apply(path)
      wsAndIssues.issues.foreach(issues.add)
      wsAndIssues.value.getOrElse(new Workspace(Seq()))
    }

    override val packages: ArraySeq[Package] = {

      val loadingPool = Executors.newFixedThreadPool(2)

      def createModule(
        pkg: Package,
        index: DocumentIndex,
        dependencies: ArraySeq[Module]
      ): Module = {
        new MetadataModule(pkg, dependencies, index, loadingPool)
      }

      val logger = new CatchingLogger

      // Fold over platform namespaces to create packages for each, these form a chain
      val platform = PlatformTypeDeclaration.namespaces.foldLeft(ArraySeq[Package]())((acc, ns) => {
        acc :+ new PlatformPackage(
          this,
          ns,
          acc.headOption.map(pkg => ArraySeq(pkg)).getOrElse(ArraySeq.empty)
        )
      })

      // Fold over layers to create packages - with any package(namespace) dependencies linked to each package
      // The workspace layers form a deploy ordering, so each is dependent on all previously created
      val declared =
        workspace.layers.foldLeft(ArraySeq[Package]())((acc, pkgLayer) => {
          acc :+ new MetadataPackage(
            this,
            pkgLayer.namespace,
            acc,
            workspace,
            ArraySeq.unsafeWrapArray(pkgLayer.layers.toArray),
            createModule,
            logger
          )
        })

      loadingPool.shutdown()

      // If no unmanaged, create it
      val unmanaged =
        if (declared.isEmpty || declared.lastOption.exists(_.namespace.nonEmpty))
          Seq(
            new MetadataPackage(
              this,
              None,
              declared,
              workspace,
              ArraySeq.empty,
              createModule,
              logger
            )
          )
        else
          Seq.empty
      ArraySeq.unsafeWrapArray((unmanaged ++ declared ++ platform).toArray)
    }

    def rootModule: Option[Module] = {
      packages.find(_.modules.nonEmpty).map(_.modules.last)
    }
  }

  trait Package extends TriPackage {
    override val org: Index

    def namespaceAsString: String = namespace.map(_.value).getOrElse("")
  }

  /* Module capabilities, not all of these will be supported by all module types, e.g. platform types don't have
   * paths. */
  trait Module extends TriModule {
    override val pkg: Package

    /* Find a type declaration from a type name. The name format is not as flexible as found in Apex, you can not
     * use whitespace or array subscripts. In general namespaces should be used although System and Schema references
     * can be resolved without a namespace. If there are TypeArguments they are resolved against the module, this
     * means they can not be relative type names that must be resolved against a specific type. See TypeFinder
     * for relative type resolving. */
    def findExactTypeId(name: String): Option[IModuleTypeDeclaration] = {
      UnresolvedTypeRef(name).toOption.flatMap(typeRef => {
        // Pre-resolve segment type arguments within the typeRef, this is required so that generic types
        // can be constructed without the need for a recursive call back to this module for type resolution.
        typeRef.typeNameSegments.foreach(segment => {
          val args = segment.getArguments
          val newArgs = args.flatMap {
            case unref: UnresolvedTypeRef => findExactTypeId(unref.toString, unref)
            case other                    => Some(other)
          }
          if (args.nonEmpty && args.length == newArgs.length)
            segment.replaceArguments(newArgs)
        })
        findExactTypeId(name, typeRef)
      })
    }

    /* Implementation for exact finding, the name & typeRef refer to the same type name, both are provided to
     * allow modules to use whichever is most suitable. */
    def findExactTypeId(name: String, typeRef: UnresolvedTypeRef): Option[IModuleTypeDeclaration]

    /* Find all types for which path contributes to the type declaration. THere should be at most one
     * result for a path but uses a Seq just in case that changes. Paths are considered case-insensitive. */
    def findTypesByPath(path: String): Seq[IModuleTypeDeclaration]

    /* Find all types for which the path prefix matches a path which contributes to the type declaration. */
    def fuzzyFindTypesByPath(pathPrefix: String): Seq[IModuleTypeDeclaration]

    /* Find all types for which the predicate evaluates to true for a path that contributes to the type declaration. */
    def findTypesByPathPredicate(predicate: String => Boolean): Seq[IModuleTypeDeclaration]

    /* Find a type declaration that starts with the passed name. The name should include the namespace. Returns
     * the first found. */
    def fuzzyFindTypeId(name: String): Option[IModuleTypeDeclaration]

    /* Find all type declarations that starts with the passed name. The name should include the namespace. */
    def fuzzyFindTypeIds(name: String): Seq[IModuleTypeDeclaration]

    def accumFuzzyFindTypeIds(name: String, accum: mutable.Map[Name, IModuleTypeDeclaration]): Unit

    /* Find all type declarations within a namespaces that starts with the passed string. The namespace match is
     * case insensitive.*/
    def findTypeIdsByNamespace(namespacePrefix: String): Seq[IModuleTypeDeclaration]

    def accumFindTypeIdsByNamespace(
      namespacePrefix: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit
  }

  class MetadataPackage(
    override val org: Index,
    override val namespace: Option[Name],
    override val basePackages: ArraySeq[Package],
    workspace: Workspace,
    layers: ArraySeq[ModuleLayer],
    mdlFactory: (Package, DocumentIndex, ArraySeq[Module]) => Module,
    logger: IssueLogger
  ) extends Package {

    val modules: ArraySeq[Module] =
      layers
        .foldLeft(ArraySeq[Module]())((acc, layer) => {
          val issuesAndIndex = workspace.indexes(layer)
          logger.logAll(issuesAndIndex.issues)
          val module = mdlFactory(this, issuesAndIndex.value, acc)
          acc :+ module
        })
  }

  class MetadataModule(
    override val pkg: Package,
    override val dependents: ArraySeq[Module],
    val index: DocumentIndex,
    loadingPool: ExecutorService
  ) extends Module
      with TypeFinder {

    private final val lowerNames       = mutable.TreeSet[String]()
    private[workspace] final val types = mutable.Map[Name, IModuleTypeDeclaration]()

    loadClasses()

    private def loadClasses(): Unit = {
      val namespace = pkg.namespace
      val classes = new ApexClassLoader(loadingPool, ModuleClassFactory)
        .loadClasses(index.get(ApexNature), pkg.org.issues)
      classes.foreach { docAndType =>
        markModule(docAndType._2)
        insertClass(docAndType._1.typeName(namespace).toString, docAndType._2)
      }
      classes.foreach { docAndType =>
        typeResolve(docAndType._2)
        docAndType._2.innerTypes.foreach(typeResolve)
      }
    }

    private def markModule(decl: TypeDeclaration): Unit = {
      decl match {
        case scoped: IModuleTypeDeclaration =>
          decl.innerTypes.foreach(markModule)
          scoped.module = this
        case _ => ()
      }
    }

    private def typeResolve(decl: TypeDeclaration): Unit = {
      def resolveSignature(body: Signature): Unit = {
        body.typeRef = findType(body.typeRef, decl).getOrElse(body.typeRef)
        body match {
          case sfpl: SignatureWithParameterList => resolveParameterList(sfpl.formalParameterList)
          case _                                =>
        }
      }

      def resolveParameterList(fpl: FormalParameterList): Unit = {
        fpl.formalParameters.foreach(fp => {
          fp.typeRef = fp.typeRef.flatMap(tr => findType(tr, decl)).orElse(fp.typeRef)
        })
      }

      decl._extendsTypeRef = Option(decl.extendsTypeRef) match {
        case Some(etr) => findType(etr, decl).orNull
        case None      => null
      }
      Option(decl.implementsTypeList).foreach(tl => {
        tl.typeRefs.mapInPlace(tr => findType(tr, decl).getOrElse(tr))
      })
      decl.constructors.foreach(c => resolveParameterList(c.formalParameterList))
      decl.properties.foreach(resolveSignature)
      decl.fields.foreach(resolveSignature)
      decl.methods.foreach(resolveSignature)
    }

    private def insertClass(name: String, decl: TypeDeclaration): Unit = {
      lowerNames.add(name.toLowerCase)
      types.put(Name(name), decl.asInstanceOf[IModuleTypeDeclaration])

      decl match {
        case outer: ClassTypeDeclaration =>
          outer.innerTypes.foreach(inner => {
            val innerName = s"$name.${inner.id}"
            lowerNames.add(name.toLowerCase)
            types.put(Name(innerName), inner.asInstanceOf[IModuleTypeDeclaration])
          })
        case _ => ()
      }
    }

    override def isVisibleFile(path: PathLike): Boolean = {
      index.isVisibleFile(path)
    }

    override def findExactTypeId(
      name: String,
      typeRef: UnresolvedTypeRef
    ): Option[IModuleTypeDeclaration] = {
      types
        .get(Name(name))
        .orElse(baseModules.headOption.flatMap(_.findExactTypeId(name)))
        .orElse(
          basePackages.headOption
            .flatMap(_.orderedModules.headOption.flatMap(_.findExactTypeId(name)))
        )
    }

    def fuzzyFindTypeId(name: String): Option[IModuleTypeDeclaration] = {
      if (name != null && name.nonEmpty) {
        val lower = name.toLowerCase
        lowerNames
          .rangeFrom(lower)
          .take(1)
          .find(_.startsWith(lower))
          .flatMap(name => types.get(Name(name)))
          .orElse(baseModules.headOption.flatMap(_.fuzzyFindTypeId(name)))
          .orElse(
            basePackages.headOption
              .flatMap(_.orderedModules.headOption.flatMap(_.fuzzyFindTypeId(name)))
          )
      } else {
        None
      }
    }

    def fuzzyFindTypeIds(name: String): Seq[IModuleTypeDeclaration] = {
      if (name != null && name.nonEmpty) {
        val accum = new mutable.HashMap[Name, IModuleTypeDeclaration]()
        accumFuzzyFindTypeIds(name, accum)
        accum.keys.toSeq.sortBy(_.value.length).flatMap(accum.get)
      } else {
        Seq.empty
      }
    }

    def accumFuzzyFindTypeIds(
      name: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit = {
      // Accumulate lower layers first
      if (baseModules.isEmpty) {
        basePackages.headOption.foreach(
          _.orderedModules.headOption.foreach(_.accumFuzzyFindTypeIds(name, accum))
        )
      } else {
        baseModules.headOption.foreach(_.accumFuzzyFindTypeIds(name, accum))
      }

      // Add/Overwrite with this module
      val lower = name.toLowerCase
      lowerNames
        .rangeFrom(lower)
        .iterator
        .filter(_.startsWith(lower))
        .foreach(typeName => {
          val name = Name(typeName)
          types.get(name).foreach(accum.put(name, _))
        })
    }

    def findTypeIdsByNamespace(namespacePrefix: String): Seq[IModuleTypeDeclaration] = {
      if (namespacePrefix != null) {
        val accum = new mutable.HashMap[Name, IModuleTypeDeclaration]()
        accumFindTypeIdsByNamespace(namespacePrefix, accum)
        accum.keys.toSeq.sortBy(_.value.length).flatMap(accum.get)
      } else {
        Seq.empty
      }
    }

    def accumFindTypeIdsByNamespace(
      namespacePrefix: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit = {
      basePackages.headOption.foreach(
        _.orderedModules.headOption.foreach(_.accumFindTypeIdsByNamespace(namespacePrefix, accum))
      )

      val namespaceMatches =
        if (namespacePrefix.isEmpty)
          pkg.namespace.isEmpty
        else
          pkg.namespace.exists(ns => ns.value.toLowerCase.startsWith(namespacePrefix.toLowerCase))

      if (namespaceMatches) {
        baseModules.headOption.foreach(_.accumFindTypeIdsByNamespace(namespacePrefix, accum))
        accum.addAll(types)
      }
    }

    def findTypesByPath(path: String): Seq[IModuleTypeDeclaration] = {
      findTypesByPathPredicate(t => t.equalsIgnoreCase(path))
    }

    def fuzzyFindTypesByPath(path: String): Seq[IModuleTypeDeclaration] = {
      findTypesByPathPredicate(t => t.toLowerCase.startsWith(path.toLowerCase))
    }

    def findTypesByPathPredicate(predicate: String => Boolean): Seq[IModuleTypeDeclaration] = {
      var typesForPath = types.values.filter(t => t.paths.exists(p => predicate(p)))
      if (typesForPath.nonEmpty) return typesForPath.toSeq

      typesForPath = baseModules.headOption
        .map(_.findTypesByPathPredicate(predicate))
        .getOrElse(Seq.empty)
      if (typesForPath.nonEmpty) return typesForPath.toSeq

      typesForPath = basePackages.headOption
        .flatMap(_.orderedModules.headOption.map(_.findTypesByPathPredicate(predicate)))
        .getOrElse(Seq.empty)
      typesForPath.toSeq
    }

    override def toString: String = s"Module(${index.path})"
  }

  class PlatformPackage(
    override val org: Index,
    _namespace: Name,
    override val basePackages: ArraySeq[Package]
  ) extends Package {

    override val namespace: Option[Name] = Some(_namespace)

    val modules: ArraySeq[Module] = ArraySeq(new PlatformModule(this))
  }

  class PlatformModule(override val pkg: PlatformPackage) extends Module {

    override val dependents: ArraySeq[Module] = ArraySeq.empty

    override def isVisibleFile(path: PathLike): Boolean = false

    override def findExactTypeId(
      name: String,
      typeRef: UnresolvedTypeRef
    ): Option[IModuleTypeDeclaration] = {
      if (
        typeRef.typeNameSegments.head.id.id.contents.equalsIgnoreCase(namespace.get.value)
        && typeRef.typeNameSegments.head.typeArguments.isEmpty
      ) {
        PlatformTypeDeclaration.get(this, typeRef)
      } else {
        None
      }
    }

    def findTypesByPath(path: String): Seq[IModuleTypeDeclaration] = Seq.empty

    def fuzzyFindTypesByPath(path: String): Seq[IModuleTypeDeclaration] = Seq.empty

    def findTypesByPathPredicate(predicate: String => Boolean): Seq[IModuleTypeDeclaration] =
      Seq.empty

    def fuzzyFindTypeId(name: String): Option[IModuleTypeDeclaration] = None

    def fuzzyFindTypeIds(name: String): Seq[IModuleTypeDeclaration] = Seq.empty

    def accumFuzzyFindTypeIds(
      name: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit = {}

    def findTypeIdsByNamespace(namespacePrefix: String): Seq[IModuleTypeDeclaration] = Seq.empty

    def accumFindTypeIdsByNamespace(
      namespacePrefix: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit = {}
  }

}
