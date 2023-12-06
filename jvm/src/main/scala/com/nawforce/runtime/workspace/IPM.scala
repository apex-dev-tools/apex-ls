/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.types._
import com.financialforce.types.base.{TypeNameSegment, TypeRef, UnresolvedTypeRef}
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents.{ApexNature, DocumentIndex, SObjectNature}
import com.nawforce.pkgforce.names.TypeName.ambiguousAliasMap
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.pkgs.TriHierarchy
import com.nawforce.pkgforce.workspace.{ModuleLayer, Workspace}
import com.nawforce.runtime.types.platform.{PlatformTypeDeclaration, SObjectTypeDeclaration}

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
      Workspace(path, issues).getOrElse(new Workspace(issues, Seq()))
    }

    override val packages: ArraySeq[Package] = {

      val loadingPool = Executors.newFixedThreadPool(2)

      def createModule(
        pkg: Package,
        index: DocumentIndex,
        isGulped: Boolean,
        dependencies: ArraySeq[Module]
      ): Module = {
        new MetadataModule(pkg, dependencies, isGulped, index, loadingPool)
      }

      val logger = new CatchingLogger

      // Fold over platform namespaces to create packages for each, these form a chain
      val platform =
        PlatformTypeDeclaration.namespaces.reverse.foldLeft(ArraySeq[Package]())((acc, ns) => {
          new PlatformPackage(
            this,
            ns,
            acc.headOption.map(pkg => ArraySeq(pkg)).getOrElse(ArraySeq.empty)
          ) +: acc
        })

      // Fold over layers to create packages - with any package(namespace) dependencies linked to each package
      // The workspace layers form a deploy ordering, so each is dependent on all previously created
      // Note: We could chain these (as per platform namespaces) but modules need multiple dependents for 2GP so we
      // choose to be consistent and model as a set of dependents
      val declared =
        workspace.layers.foldLeft(ArraySeq[Package]())((acc, pkgLayer) => {
          acc :+ new MetadataPackage(
            this,
            pkgLayer.namespace,
            if (acc.isEmpty) ArraySeq(platform.head) else acc,
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
      ArraySeq.unsafeWrapArray((platform ++ declared ++ unmanaged).toArray)
    }

    def rootModule: Option[Module] = {
      packages.findLast(_.modules.nonEmpty).map(_.modules.last)
    }
  }

  trait Package extends TriPackage {
    override val org: Index

    // Helper to save from dealing with Option in Java
    def namespaceAsString: String = namespace.map(_.value).getOrElse("")
  }

  /* Module capabilities, not all of these will be supported by all module types, e.g. platform types don't have
   * paths. */
  trait Module extends TriModule {
    override val pkg: Package

    // Helper to save from dealing with Option in Java
    def namespaceAsString: String = pkg.namespaceAsString

    /* Find a type declaration from a type name. The name format is not as flexible as found in Apex, you can not
     * use whitespace or array subscripts. In general namespaces should be used although System and Schema references
     * can be resolved without a namespace. If there are TypeArguments they are resolved against the module, this
     * means they can not be relative type names that must be resolved against a specific type. See TypeFinder
     * for relative type resolving. */
    def findExactTypeId(name: String): Option[IModuleTypeDeclaration] = {
      assert(!name.contains(" ")) // FUTURE: improve UnresolvedTypeRef parsing to remove this!

      UnresolvedTypeRef(name).toOption.flatMap(typeRef => {
        // Pre-resolve segment type arguments within the typeRef, this is required so that generic types
        // can be constructed without the need for a recursive call back to this module for type resolution.
        val resolvedSegments = typeRef.typeNameSegments.map(segment => {
          val args = segment.typeArguments
          val newArgs = args.flatMap {
            case unref: UnresolvedTypeRef =>
              findExactTypeId(unref.toString, unref)
                .orElse(findExactTypeId(unref.toString))
            case other => Some(other)
          }
          if (args.nonEmpty && args.length == newArgs.length)
            segment.replaceArguments(newArgs)
          else
            segment
        })
        findExactTypeId(name, UnresolvedTypeRef(resolvedSegments, 0))
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

    /* Find all type declarations that start with the passed name. The name should include the namespace. */
    def fuzzyFindTypeIds(name: String): Seq[IModuleTypeDeclaration] = {
      if (name != null && name.nonEmpty) {
        val accum = new mutable.HashMap[Name, IModuleTypeDeclaration]()
        accumFuzzyFindTypeIds(name, accum)
        accum.keys.toSeq.sortBy(_.value.length).flatMap(accum.get)
      } else {
        Seq.empty
      }
    }

    /* Accumulate all type declarations that start with the passed name. The name should include the namespace. */
    def accumFuzzyFindTypeIds(name: String, accum: mutable.Map[Name, IModuleTypeDeclaration]): Unit

    /* Find all type declarations within a namespaces that starts with the passed string. The namespace match is
     * case insensitive.*/
    def findTypeIdsByNamespace(namespacePrefix: String): Seq[IModuleTypeDeclaration] = {
      if (namespacePrefix != null) {
        val accum = new mutable.HashMap[Name, IModuleTypeDeclaration]()
        accumFindTypeIdsByNamespace(namespacePrefix, accum)
        accum.keys.toSeq.sortBy(_.value.length).flatMap(accum.get)
      } else {
        Seq.empty
      }
    }

    /* Accumulate all type declarations within a namespaces that starts with the passed string. The namespace match is
     * case insensitive.*/
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
    mdlFactory: (Package, DocumentIndex, Boolean, ArraySeq[Module]) => Module,
    logger: IssueLogger
  ) extends Package {

    val modules: ArraySeq[Module] =
      layers
        .foldLeft(ArraySeq[Module]())((acc, layer) => {
          acc :+ mdlFactory(this, workspace.indexes(layer), layer.isGulped, acc)
        })
  }

  class MetadataModule(
    override val pkg: Package,
    override val dependents: ArraySeq[Module],
    override val isGulped: Boolean,
    val index: DocumentIndex,
    loadingPool: ExecutorService
  ) extends Module {
    val isPlatformExtension: Boolean = false

    private final val lowerNames = mutable.TreeSet[String]()
    private final val types      = mutable.Map[Name, IModuleTypeDeclaration]()

    loadSObjects()
    loadClasses()

    private def loadSObjects(): Unit = {
      val namespace = pkg.namespace

      index
        .getControllingDocuments(SObjectNature)
        .foreach(md => {
          val td           = SObjectTypeDeclaration(this, md)
          val absoluteName = md.typeName(namespace).toString
          val name         = md.name.value
          insertSObject(absoluteName, td)
          insertSObject(name, td)
        })
    }

    private def loadClasses(): Unit = {
      val namespace = pkg.namespace
      val classes = new ApexClassLoader(loadingPool, this, ModuleClassFactory)
        .loadClasses(index.getControllingDocuments(ApexNature).iterator, pkg.org.issues)
      classes.foreach { docAndType =>
        insertClass(docAndType._1.typeName(namespace).toString, docAndType._2)
      }
      classes.foreach { docAndType =>
        typeResolve(docAndType._2)
        docAndType._2.innerTypes.foreach(typeResolve)
      }
    }

    private def typeResolve(decl: IMutableModuleTypeDeclaration): Unit = {
      def resolve(typeRef: TypeRef): Option[ITypeDeclaration] = {
        TypeFinder.get(this, typeRef, decl)
      }

      def resolveSignature(body: IVariable): Unit = {
        body.typeRef = resolve(body.typeRef).getOrElse(body.typeRef)

        if (body.typeRef.isInstanceOf[UnresolvedTypeRef]) {
          resolve(body.typeRef)
        }
      }

      def resolveMethods(methodDeclaration: IMethodDeclaration): Unit = {
        if (methodDeclaration.typeRef.nonEmpty) {
          val isVoid = methodDeclaration.typeRef.get.fullName.equalsIgnoreCase(Names.Void.value)
          methodDeclaration.typeRef =
            if (isVoid) None
            else resolve(methodDeclaration.typeRef.get).orElse(methodDeclaration.typeRef)
        }
        resolveParameterList(methodDeclaration.formalParameters)
      }

      def resolveParameterList(fpl: ArraySeq[IFormalParameter]): Unit = {
        fpl.foreach(fp => {
          fp.typeRef = resolve(fp.typeRef).getOrElse(fp.typeRef)
        })
      }

      decl.setExtends(Option(decl.extendsTypeRef) match {
        case Some(etr) => resolve(etr).orNull
        case None      => null
      })
      Option(decl.implementsTypeList).foreach(tl => {
        decl.setImplements(tl.map(tr => resolve(tr).getOrElse(tr)))
      })
      decl.constructors.foreach(c => resolveParameterList(c.formalParameters))
      decl.properties.foreach(resolveSignature)
      decl.fields.foreach(resolveSignature)
      decl.methods.foreach(resolveMethods)
    }

    private def insertClass(name: String, decl: IMutableModuleTypeDeclaration): Unit = {
      lowerNames.add(name.toLowerCase)
      types.put(Name(name), decl)

      decl match {
        case outer: ClassTypeDeclaration =>
          outer.innerTypes.foreach(inner => {
            val innerName = s"$name.${inner.id}"
            lowerNames.add(name.toLowerCase)
            types.put(Name(innerName), inner.asInstanceOf[IMutableModuleTypeDeclaration])
          })
        case _ => ()
      }
    }

    private def insertSObject(name: String, decl: SObjectTypeDeclaration): Unit = {
      // lowerNames.add(name.toLowerCase)
      // types.put(Name(name), decl)
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
        .orElse(nextModule.flatMap(_.findExactTypeId(name, typeRef)))
    }

    override def fuzzyFindTypeId(name: String): Option[IModuleTypeDeclaration] = {
      if (name != null && name.nonEmpty) {
        val lower = name.toLowerCase
        lowerNames
          .rangeFrom(lower)
          .take(1)
          .find(_.startsWith(lower))
          .flatMap(name => types.get(Name(name)))
          .orElse(nextModule.flatMap(_.fuzzyFindTypeId(name)))
      } else {
        None
      }
    }

    override def accumFuzzyFindTypeIds(
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

    override def accumFindTypeIdsByNamespace(
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

    override def findTypesByPath(path: String): Seq[IModuleTypeDeclaration] = {
      findTypesByPathPredicate(t => t.equalsIgnoreCase(path))
    }

    override def fuzzyFindTypesByPath(path: String): Seq[IModuleTypeDeclaration] = {
      findTypesByPathPredicate(t => t.toLowerCase.startsWith(path.toLowerCase))
    }

    override def findTypesByPathPredicate(
      predicate: String => Boolean
    ): Seq[IModuleTypeDeclaration] = {
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

    val modules: ArraySeq[Module] = ArraySeq(PlatformModule(_namespace, this))
  }

  /* Module for platform types. Only exact searching is supported on platform types. */
  class PlatformModule(override val pkg: PlatformPackage) extends Module {

    val isPlatformExtension: Boolean = false

    private final val types = mutable.Map[Name, Option[IModuleTypeDeclaration]]()
    /* We use a Weak Map here as the type arguments may be refreshed so that new UnresolvedTypeRefs are used */
    private final val genericTypes =
      mutable.WeakHashMap[UnresolvedTypeRef, Option[IModuleTypeDeclaration]]()
    private final val defaultNamespace =
      namespace.contains(Names.System) || namespace.contains(Names.Schema)

    override val dependents: ArraySeq[Module] = ArraySeq.empty

    override val isGulped: Boolean = false

    override def findExactTypeId(
      name: String,
      typeRef: UnresolvedTypeRef
    ): Option[IModuleTypeDeclaration] = {

      // Short-cut to next module if could not possibly match
      if (
        !defaultNamespace && !typeRef.typeNameSegments.head.id.lowerCaseName
          .equalsIgnoreCase(namespace.get.value)
      )
        return nextModule.flatMap(_.findExactTypeId(name, typeRef))

      // Default namespace if needed and get declaration
      val defaultedNameAndRef = defaultName(name, typeRef)
      val isGeneric           = typeRef.typeNameSegments.exists(_.typeArguments.nonEmpty)
      val result =
        if (!isGeneric) {
          types.getOrElseUpdate(
            Name(defaultedNameAndRef._1), {
              PlatformTypeDeclaration.get(this, defaultedNameAndRef._2)
            }
          )
        } else {
          // Generics are handled separately as we key off typeRef, IModuleTypeDeclaration uses IdentityEquality
          // so that type arguments can be quickly compared and mutability is not a concern
          genericTypes.getOrElseUpdate(
            defaultedNameAndRef._2, {
              PlatformTypeDeclaration.get(this, defaultedNameAndRef._2)
            }
          )
        }

      result.orElse {
        // Continue search in next module
        nextModule.flatMap(_.findExactTypeId(name, typeRef))
      }
    }

    private def defaultName(
      name: String,
      typeRef: UnresolvedTypeRef
    ): (String, UnresolvedTypeRef) = {
      // This will stop defaulting names for ambiguous names so we can resolve them correctly in TypeFinder
      val isNameAmbiguous = ambiguousAliasMap.contains(TypeName(Name(name)))
      if (
        !defaultNamespace || isNameAmbiguous ||
        (typeRef.typeNameSegments.length > 1 &&
          typeRef.typeNameSegments.head.id.lowerCaseName.equalsIgnoreCase(namespace.get.value))
      ) {
        (name, typeRef)
      } else {
        (
          namespace.get.value + "." + name,
          UnresolvedTypeRef(
            TypeNameSegment(namespace.get.value) +: typeRef.typeNameSegments,
            typeRef.arraySubscripts
          )
        )
      }
    }

    /* Platform types do not support types so these functions are no-ops */

    override def isVisibleFile(path: PathLike): Boolean = false

    override def findTypesByPath(path: String): Seq[IMutableModuleTypeDeclaration] = Seq.empty

    override def fuzzyFindTypesByPath(path: String): Seq[IMutableModuleTypeDeclaration] = Seq.empty

    override def findTypesByPathPredicate(
      predicate: String => Boolean
    ): Seq[IMutableModuleTypeDeclaration] =
      Seq.empty

    /* Fuzzy searching has not been implemented for platform types */

    override def fuzzyFindTypeId(name: String): Option[IMutableModuleTypeDeclaration] = None

    override def accumFuzzyFindTypeIds(
      name: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit = {}

    override def accumFindTypeIdsByNamespace(
      namespacePrefix: String,
      accum: mutable.Map[Name, IModuleTypeDeclaration]
    ): Unit = {}
  }

  class SchemaPlatformModule(override val pkg: PlatformPackage) extends PlatformModule(pkg) {
    // TODO:
  }

  object PlatformModule {
    def apply(namespace: Name, pkg: PlatformPackage): PlatformModule = {
      namespace match {
        case Names.Schema => new SchemaPlatformModule(pkg)
        case _            => new PlatformModule(pkg)
      }
    }
  }

}
