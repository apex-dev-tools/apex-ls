/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

import com.nawforce.apexlink.api._
import com.nawforce.apexlink.finding.TypeResolver.TypeCache
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.opcst.OutlineParserFullDeclaration
import com.nawforce.apexlink.types.apex.{FullDeclaration, SummaryApex, TriggerDeclaration}
import com.nawforce.apexlink.types.other._
import com.nawforce.apexlink.types.platform.PlatformTypes
import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.names._
import com.nawforce.pkgforce.stream._

import java.util.concurrent.{
  Callable,
  ConcurrentHashMap,
  ConcurrentLinkedQueue,
  Executors,
  ForkJoinPool,
  Future,
  ThreadFactory
}
import scala.collection.immutable.ArraySeq
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.{BufferedIterator, mutable}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.util.hashing.MurmurHash3

/** 'Deploy' a module from a stream of PackageEvents. Deploying here really means constructing a set
  * of TypeDeclarations and validating them against each other. This process mutates the passed
  * types Map for compatibility with dependency analysis code. The class handling code here is
  * performance sensitive so this may also aid with efficiency.
  *
  * FUTURE: Remove Module dependency.
  */
class StreamDeployer(
  module: OPM.Module,
  events: Iterator[PackageEvent],
  types: TypeDeclarationCache
) {
  load()

  private def load(): Unit = {
    val start          = java.lang.System.currentTimeMillis()
    val basicTypesSize = types.size

    // Process package events, these must follow the publishing order from pkgforce
    PlatformTypes.withLoadingObserver(module.schemaSObjectType) {
      val bufferedIterator = events.buffered
      consumeLabels(bufferedIterator)
      val components = consumeComponents(bufferedIterator)
      val pages      = consumePages(bufferedIterator)
      consumeFlows(bufferedIterator)
      consumeSObjects(bufferedIterator)
      consumeClasses(bufferedIterator)
      consumeTriggers(bufferedIterator)
      components.safeValidate()
      pages.safeValidate()
    }

    // Run plugins over loaded types DependentTypes
    // This has to be done post loading to allow dependencies to be established
    LoggerOps.infoTime("Closed plugins (unused analysis)", types.size > basicTypesSize) {
      module.pkg.org.pluginsManager.closePlugins()
    }

    // Report progress and tidy up
    if (types.size > basicTypesSize) {
      val total = (java.lang.System.currentTimeMillis() - start).toDouble
      val avg   = total / types.size
      LoggerOps.info(f"$module loaded ${types.size} types in ${total}ms, average $avg%.1f ms/type")
    }
  }

  private def consumeLabels(events: BufferedIterator[PackageEvent]): Unit = {
    val labelRelatedEvents = bufferEvents(Set(classOf[LabelFileEvent], classOf[LabelEvent]), events)
    val labelFileEvents    = labelRelatedEvents.collect { case e: LabelFileEvent => e }
    val labelEvents        = labelRelatedEvents.collect { case e: LabelEvent => e }
    val labels             = LabelDeclaration(module).merge(labelFileEvents, labelEvents)
    types.put(labels)
    types.put(labels, Some(TypeName(labels.name)))
  }

  private def consumeComponents(events: BufferedIterator[PackageEvent]): ComponentDeclaration = {
    val componentDeclaration =
      ComponentDeclaration(module).merge(bufferEvents[ComponentEvent](events))
    types.put(componentDeclaration)
    componentDeclaration
  }

  private def consumePages(events: BufferedIterator[PackageEvent]): PageDeclaration = {
    val pageDeclaration = PageDeclaration(module).merge(bufferEvents[PageEvent](events))
    types.put(pageDeclaration)
    pageDeclaration
  }

  private def consumeFlows(events: BufferedIterator[PackageEvent]): Unit = {
    types.put(InterviewDeclaration(module).merge(bufferEvents[FlowEvent](events)))
  }

  private def consumeSObjects(events: BufferedIterator[PackageEvent]): Unit = {
    // Create and register SObjects
    val deployer = new SObjectDeployer(module)
    val sobjects = deployer.createSObjects(events)
    sobjects.foreach(sobject => {
      types.put(sobject)
      module.schemaSObjectType.add(sobject.typeName.name, hasFieldSets = true)
    })

    // Run custom validation to setup dependencies
    sobjects.foreach(_.safeValidate())
  }

  /** Consume Apex class events, this is a bit more involved as we try and load first via cache and
    * then fallback to reading the source and parsing.
    */
  private def consumeClasses(events: BufferedIterator[PackageEvent]): Unit = {
    val docs = bufferEvents[ApexEvent](events).map(e => ApexClassDocument(e.path))

    // Load summary classes from the cache
    LoggerOps.debugTime(s"Loaded summary classes", docs.nonEmpty) {
      validateSummaryClasses(loadClassesFromCache(docs))
    }

    // Load any classes not found via cache or that have been rejected
    val missingClasses =
      docs.filterNot(doc => types.contains(TypeName(doc.name).withNamespace(module.namespace)))
    LoggerOps.debug(s"${missingClasses.length} of ${docs.length} classes not available from cache")

    val failures = loadClassesWithOutlineParser(module.pkg.org.getParserType, missingClasses)
    if (failures.nonEmpty)
      parseAndValidateClasses(failures)
  }

  /** Parse a collection of Apex classes, insert them and validate them. */
  private def parseAndValidateClasses(docs: ArraySeq[ClassDocument]): Unit = {
    LoggerOps.debugTime(s"Parsed ${docs.length} classes", docs.nonEmpty) {
      val decls = docs
        .flatMap(doc =>
          doc.path.readSourceData() match {
            case Left(_) => None
            case Right(data) =>
              LoggerOps.debugTime(s"Parsed ${doc.path}") {
                FullDeclaration
                  .create(module, doc, data, forceConstruct = false)
                  .map(td => {
                    types.put(td)
                    td
                  })
              }
          }
        )

      // Validate the classes, this must be last due to mutual dependence
      decls.foreach { _.safeValidate() }
    }
  }

  /** Validate summary classes & log diagnostics, those with any invalid dependents are discarded.
    */
  private def validateSummaryClasses(summaryClasses: Iterator[SummaryApex]): Unit = {

    val classes       = summaryClasses.toArray
    val rejected      = mutable.Set[SummaryApex]()
    val typeCache     = new TypeCache()
    var hasRejections = true
    var rejectCycles  = 0

    // Collect rejected in a set, we have to multi-pass to make sure all are found
    while (hasRejections) {
      val rejects = classes.filterNot(cls =>
        rejected.contains(cls) || cls.declaration.hasValidDependencies(typeCache)
      )

      // Tidy up rejected so they can't be found
      rejects.foreach(reject => {
        val typeName = reject.declaration.typeName
        types.remove(typeName)
        typeCache.remove((typeName, module))
        LoggerOps.info(s"Cached type $typeName rejected due to invalid dependencies")
      })
      rejected.addAll(rejects)
      hasRejections = rejects.nonEmpty
      rejectCycles += 1
    }
    if (rejectCycles > 1)
      LoggerOps.info(s"Used $rejectCycles rejection cycles")

    // For those not rejected, complete processing
    val survivors = classes.filterNot(rejected.contains)
    survivors.foreach(cls => {
      // Re-establish dependencies
      cls.declaration.propagateOuterDependencies(typeCache)
      cls.declaration.propagateDependencies()

      // Report any (existing) diagnostics
      val path = cls.declaration.location.path
      cls.diagnostics
        .filter(diagnostic =>
          module.pkg.org.unusedEnabled || diagnostic.category != UNUSED_CATEGORY
        )
        .foreach(diagnostic => module.pkg.org.issues.add(Issue(path, diagnostic)))
    })

    // Seed cache-loaded classes into the plugin manager so holder-based unused analysis is
    // recomputed for them in this workspace (UnusedPlugin.onSummaryValidated) during closePlugins,
    // rather than replaying the workspace-dependent cached result (issue #477).
    if (survivors.nonEmpty) {
      survivors.foreach(cls => module.pkg.org.pluginsManager.createPlugin(cls.declaration))
      LoggerOps.debug(s"Seeded ${survivors.length} cache-loaded classes for unused recompute")
    }
  }

  /** Load classes from the code cache as types returning TypeNames of those available. Benchmarking
    * has shown running this in parallel helps performance quite a bit with SSDs.
    */
  private def loadClassesFromCache(classes: ArraySeq[ApexClassDocument]): Iterator[SummaryApex] = {
    module.pkg.org.parsedCache
      .map(parsedCache => {

        val pkgContext = module.pkg.packageContext
        val localAccum = new ConcurrentHashMap[TypeName, SummaryApex]()

        classes.par.foreach(doc => {
          val value = parsedCache.get(pkgContext, doc.name.value, contentHash(doc))
          val ad    = value.map(v => SummaryApex(doc.path, module, v))
          if (ad.nonEmpty && !ad.get.diagnostics.exists(_.category == MISSING_CATEGORY)) {
            localAccum.put(ad.get.declaration.typeName, ad.get)
          }
        })

        localAccum.entrySet.forEach(kv => {
          types.put(kv.getKey, kv.getValue.declaration)
        })

        localAccum.values().iterator().asScala
      })
      .getOrElse(Iterator())
  }

  private def contentHash(doc: ApexClassDocument): Int = {
    ParsedCache.classMetaHash(
      doc.path.parent.join(s"${doc.name.toString}.cls-meta.xml"),
      MurmurHash3.bytesHash(doc.path.readBytes().getOrElse(Array.empty))
    )
  }

  private def loadClassesWithOutlineParser(
    selectedParser: AvailableParser,
    classes: ArraySeq[ClassDocument]
  ): ArraySeq[ClassDocument] = {

    val localAccum      = new ConcurrentHashMap[TypeName, FullDeclaration]()
    val docsByType      = new ConcurrentHashMap[TypeName, ClassDocument]()
    val failedDocuments = new ConcurrentLinkedQueue[ClassDocument]()

    def parse(cls: ClassDocument): Unit = {
      cls.path.readSourceData() match {
        case Left(error) =>
          LoggerOps.info(s"Failed reading source $error")
        case Right(srcData) =>
          LoggerOps.debugTime(s"Parsed ${cls.path}") {
            val td = OutlineParserFullDeclaration
              .toFullDeclaration(cls, srcData, module)
              .map(td => {
                localAccum.put(td.typeName, td)
                docsByType.put(td.typeName, cls)
              })
            if (td.isEmpty) failedDocuments.add(cls)
          }
      }
    }

    LoggerOps.debugTime(s"Parsed ${classes.length} classes", classes.nonEmpty) {
      if (selectedParser == OutlineParserSingleThreaded) classes.foreach(parse)
      else StreamDeployer.parseInParallel(classes, parse)
      localAccum.entrySet.forEach(kv => {
        types.put(kv.getKey, kv.getValue)
      })
      StreamDeployer.validateWithBlockPrefetch(
        localAccum.values().asScala.toArray,
        td => {
          if (!td.tryValidate()) {
            types.remove(td.typeName)
            Option(docsByType.get(td.typeName)).foreach(failedDocuments.add)
          }
        }
      )
    }
    ArraySeq.from(failedDocuments.asScala.toSeq)
  }

  /** Consume trigger events, these could be cached, but they don't consume much time to for we load
    * from disk and parse each time.
    */
  private def consumeTriggers(events: BufferedIterator[PackageEvent]): Unit = {
    val docs = bufferEvents[TriggerEvent](events).map(e => ApexTriggerDocument(e.path))
    LoggerOps.debugTime(s"Parsed ${docs.length} triggers", docs.nonEmpty) {
      docs
        .flatMap(doc => {
          doc.path.readSourceData() match {
            case Left(_) => None
            case Right(data) =>
              TriggerDeclaration
                .create(module, doc.path, data)
                .map(td => {
                  types.put(td)
                  td.safeValidate()
                })
          }
        })
    }
  }
}

object StreamDeployer {

  /** Threads to parse on. The per file cost of parsing grows with concurrency, so wall clock stops
    * improving after a few threads while CPU use keeps climbing. Take the part of the win that is
    * real and leave the rest of the machine to whatever else the developer is running.
    *
    * `scala.concurrent.context.maxThreads`, which bounds the parallel collection pool this used to
    * run on, still sets the level explicitly when it holds a plain integer.
    */
  private val parseThreads: Int = {
    val requested = Option(System.getProperty("scala.concurrent.context.maxThreads"))
      .flatMap(_.toIntOption)
      .getOrElse(Math.min(Runtime.getRuntime.availableProcessors(), 4))
    Math.max(requested, 1)
  }

  /** Shared by every module deployed in this process, so that a workspace of many modules parses on
    * parseThreads rather than that many threads per module. The pool's workers are daemon threads
    * that retire when they have been idle, so it does not need shutting down.
    */
  private lazy val parseTaskSupport = new ForkJoinTaskSupport(new ForkJoinPool(parseThreads))

  /** Declarations to keep parsed ahead of the validation cursor, per prefetch thread. File sizes
    * are uneven, so the window has to cover a run of large files to stop the cursor waiting on
    * one. Measured on a large workspace, 16 per thread removes nearly all of the waiting and 32
    * removes the rest.
    */
  private final val PrefetchWindowPerThread = 32

  private val prefetchThreadFactory = new ThreadFactory {
    private val count = new java.util.concurrent.atomic.AtomicInteger()
    override def newThread(r: Runnable): Thread = {
      val thread = new Thread(r, s"apex-ls-block-prefetch-${count.incrementAndGet()}")
      thread.setDaemon(true)
      thread
    }
  }

  /** Validate declarations in order, parsing the method bodies of those still to come on a small
    * pool.
    *
    * Method bodies are parsed lazily on first verify, so a plain sequential walk performs that
    * parsing inline even though it carries no ordering constraint. Each task holds the statements
    * it parsed, which are otherwise only weakly reachable, so they survive until the declaration
    * that needs them has been validated. Extra memory is therefore bounded by the window rather
    * than by the size of the workspace.
    */
  private def validateWithBlockPrefetch(
    declarations: Array[FullDeclaration],
    validate: FullDeclaration => Unit
  ): Unit = {
    val threads = ServerOps.getBlockPrefetchThreads
    if (threads <= 0 || declarations.length < 2) {
      declarations.foreach(validate)
      return
    }

    val window   = threads * PrefetchWindowPerThread
    val pool     = Executors.newFixedThreadPool(threads, prefetchThreadFactory)
    val prefetch = new Array[Future[AnyRef]](declarations.length)

    def submit(index: Int): Unit = {
      if (index < declarations.length) {
        val declaration = declarations(index)
        prefetch(index) = pool.submit(new Callable[AnyRef] {
          override def call(): AnyRef = declaration.deferredBlocks.flatMap(_.preParse())
        })
      }
    }

    try {
      (0 until Math.min(window, declarations.length)).foreach(submit)
      declarations.indices.foreach(index => {
        submit(index + window)
        try {
          prefetch(index).get()
        } catch {
          // Leave the parse to the validation itself, it repeats the work but reports as normal
          case NonFatal(ex) =>
            LoggerOps.debug(s"Block prefetch failed for ${declarations(index).typeName}: $ex")
        }
        validate(declarations(index))
        // Release only once validated, the statements are weakly held and would otherwise be
        // collectable before the declaration that needs them has run
        prefetch(index) = null
      })
    } finally {
      pool.shutdownNow()
    }
  }

  /** Parse on a bounded pool. A parallel collection defaults to the global execution context, which
    * would use every core, and iterating one gives a sequential splitter rather than parallelism.
    */
  private def parseInParallel[T](documents: ArraySeq[T], parse: T => Unit): Unit = {
    val parallel = documents.par
    parallel.tasksupport = parseTaskSupport
    parallel.foreach(parse)
  }
}
