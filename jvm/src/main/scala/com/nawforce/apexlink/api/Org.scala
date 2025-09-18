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

package com.nawforce.apexlink.api

import com.nawforce.apexlink.org.{OPM, RefreshListener}
import com.nawforce.apexlink.plugins.{PluginsManager, UnusedPlugin}
import com.nawforce.apexlink.rpc.{
  BombScore,
  ClassTestItem,
  CompletionItemLink,
  DependencyGraph,
  HoverItem,
  LocationLink,
  MethodTestItem,
  OpenOptions,
  Rename,
  TargetLocation
}
import io.github.apexdevtools.apexls.api.IssuesCollection
import com.nawforce.pkgforce.diagnostics.{IssuesManager, LoggerOps}
import com.nawforce.pkgforce.names.TypeIdentifier
import com.nawforce.pkgforce.path.{PathLike, PathLocation}
import com.nawforce.pkgforce.workspace.{ProjectConfig, Workspace}
import com.nawforce.runtime.platform.{Environment, Path}

/** A virtual Org used to present the analysis functionality in a familiar way.
  *
  * All analysis works within the context of a virtual Org. You can manage multiple of these at the
  * same time but most use cases just need one creating, see Org.newOrg(). The Org functions as a
  * container of multiple [[Package]] objects and maintains a set of discovered issues from the
  * analysis of the package metadata. All orgs have at least one 'unmanaged' package identifiable by
  * having no namespace. At any point you can list of current issues with the packages from
  * getIssues. When you create an Org the metadata from the provided workspace directory will be
  * loaded automatically, honouring the settings in sfdx-project.json & .forceignore files if present.
  *
  * Changes made to the workspace metadata files are automatically handled, although there can be some
  * lag in the file watching. You can also prompt for changes to be handled via [[Package.refresh]]
  * to have better control over handling.
  *
  * Orgs and Packages are not thread safe, serialise all calls to them.
  */
trait Org {

  /** Get the current workspace config for the org */
  def getProjectConfig(): Option[ProjectConfig]

  /** Get array of current packages. */
  def getPackages(): Array[Package]

  /** Force syncing of org metadata to the cache when not using automatic flushing (see ServerOps).
    *
    * When using manual flushing this should be called periodically to ensure the cache is kept upto
    * date after metadata changes. If it returns true then getIssues should be called to retrieved
    * the latest set of issues.
    */
  def flush(): Boolean

  /** Test if all metadata changes have been processed.
    *
    * The Package refresh function queues changes so that they may be processed in batches either
    * when you call flush() or via the automatic flushing mechanism. You can use this function to
    * determine if the queue of changes to be processed is empty.
    */
  def isDirty(): Boolean

  /** Add or remove a listener which is called when all metadata changes have been processed.
    *
    * Similar to polling until isDirty = false, though the action will run on the same thread as
    * the flusher and block it until completed. Use with caution.
    */
  def setRefreshListener(rl: Option[RefreshListener]): Unit

  /** Collection of all current issues reported against this org.
    */
  def issues: IssuesCollection

  /** Get the package containing the path.
    *
    * Returns null if no package handling this file is found or the file is not a recognised
    * metadata type.
    */
  def getPackageForPath(path: String): Package

  /** Get a list of type identifiers available in the org across all packages. This is not all
    * available type identifiers, but just those that will make most sense to list in an IDE for
    * selection.
    *
    * Returns an array which may be empty.
    */
  def getTypeIdentifiers(apexOnly: Boolean): Array[TypeIdentifier]

  /** Get Apex type dependency map for all types in the Org.
    *
    * This is intended to be only used to support exporting of the map for secondary analysis.
    */
  def getDependencies: java.util.Map[String, Array[String]]

  /** Find the location of some form of identifier.
    *
    * Currently this supports locating Outer & Inner classes and Triggers by name. These must
    * include a namespace using the usual conventions to find a location for the identifier. Returns
    * the file & position within that file if the identifier is found, otherwise returns null.
    */
  def getIdentifierLocation(identifier: TypeIdentifier): PathLocation

  /** Get a dependency graph for a type identifier.
    *
    * Depth should be a positive integer that indicates how far to search from the starting node for
    * the passed TypeIdentifier. The root node of the search is always returned if it can be found.
    * Depths > 0 will include additional nodes.
    */
  def getDependencyGraph(
    identifier: Array[TypeIdentifier],
    depth: Integer,
    apexOnly: Boolean,
    ignoring: Array[TypeIdentifier]
  ): DependencyGraph

  /** Locate a type definition given source file position of the type name to search for.
    *
    * This will attempt to locate the type definition of a type name at the provided line & offset
    * in the path. The returned location provides information of the extent of the symbols used for
    * the search as well as the file and extent of the found definition. If no symbol can be found
    * that links to a definition it returns an empty array. If content is null, path will be used to
    * load the source code. It is not necessary for the file being searched from to be free of
    * errors, but errors may impact the ability to locate inner classes within that file.
    */
  def getDefinition(path: String, line: Int, offset: Int, content: String): Array[LocationLink]

  /** Locate the implementations of interfaces and abstract methods and classes given the position
    *
    * This will attempt to locate the concrete implementation of the type definition or method at
    * the provided line & offset in the path. The returned location are calculated based on the
    * direct dependents and transitive dependents to find a concrete class that either has a super
    * type that includes the type or a method signature from the path. If no location is found an
    * empty array is returned. If content is null, path will be used to load the source code. It is
    * not necessary for the file being searched from to be free of errors, but errors may impact the
    * ability to locate inner classes within that file.
    */
  def getImplementation(path: String, line: Int, offset: Int, content: String): Array[LocationLink]

  /** Retrieve the header for methods, classes and constructors given the position of the hovered
    * code.
    *
    * This will attempt to locate the type definition of a type name at the provided line & offset
    * in the path. The returned hover item contains the header of the type declaration for a method,
    * constructor or class, as well as the position for the hover popover. If no type declaration
    * can be found for the hovered content or it is an unsupported type, an empty HoverItem is
    * returned. If content is null, path will be used to load the source code. It is not necessary
    * for the file being searched from to be free of errors, but errors may impact the ability to
    * locate inner classes within that file.
    */
  def getHover(path: String, line: Int, offset: Int, content: String): HoverItem

  /** Retrieve a list of files and the edits for each to rename a symbol, given the location of one
    * of the uses of the symbol.
    *
    * This will attempt to locate all occurrence of a symbol provided the line & offset of one such
    * occurrence within a class. The returned array contains each file where changes are to be made,
    * and an array for each file containing locations where changes are due. If no type declaration
    * can be found for the provided symbol or it is an unsupported type, an empty array is returned.
    * If content is null, path will be used to load the source code. It is not necessary for the
    * file being searched from to be free of errors, but errors may impact the ability to locate
    * inner classes within that file.
    */
  def getRenameLocations(path: String, line: Int, offset: Int, content: String): Array[Rename]

  /** Locate the references given the location and offset.
    *
    * This will attempt to find a body declaration (methods, fields or classes) at the given line &
    * offset to find any blocks of code that is uses the found body declaration. This will return an
    * array of locations at the point where the code is being used. If no references are found an
    * empty array will be returned.
    */
  def getReferences(path: String, line: Int, offset: Int): Array[TargetLocation]

  /** Get a array of completion suggestion given a source file contents and a position.
    *
    * This will attempt to provide a list of possible completion suggestions at the position of the
    * provided source file details. If none can be found it will return an empty array. Both the
    * path and content must be provided for this. The path determines the scope of the search while
    * the content is needed a we assume the on-disk source is not current. It is not necessary for
    * the source content to be free of errors, but errors may impact the results.
    */
  def getCompletionItems(
    path: String,
    line: Int,
    offset: Int,
    content: String
  ): Array[CompletionItemLink]

  /** Calculate an ordered list of classes which are having a big impact on classes dependencies,
    * aka the 'Bombs'.
    *
    * The calculation takes into account the number of incoming and outgoing dependencies from each
    * classes. High scoring classes must have a large number of both to be considered 'Bombs'. These
    * classes are scored on a scale 0-100 and ranked highest->lowest. The first 'count' os these are
    * returned for your ridicule.
    */
  def getDependencyBombs(count: Int): Array[BombScore]

  /** Find test class names that can be used to test the passed set of files.
    *
    * This calculates a set of tests that should be run when the passed files have been changed. The
    * input files must be Apex classes for this to find results. The results include all tests that
    * directly reference one of the input classes and tests that reference any supertype or
    * interface of an input class.
    *
    * Class namespaces are included.
    */
  def getTestClassNames(paths: Array[String]): Array[String]

  /** Find all available test class items or those for given paths.
    *
    * This provides a summary of available test classes, the path and location within the file. It
    * does not provide test methods. Use a follow-up call to getTestMethodItems to retrieve method
    * locations for a set of paths. Pass an empty array to retrieve all.
    *
    * Class namespaces are NOT included.
    */
  def getTestClassItems(paths: Array[String]): Array[ClassTestItem]

  /** Find test class items related to a given set of files.
    *
    * Similar to getTestClassNames, however it additionally provides the test class path and
    * location.
    *
    * Class namespaces are NOT included.
    */
  def getTestClassItemsChanged(paths: Array[String]): Array[ClassTestItem]

  /** Find all test method items or those for given paths.
    *
    * This provides a summary of available test methods, class name and the path and location within
    * the file. Pass an empty array to retrieve all.
    *
    * Class namespaces are NOT included.
    */
  def getTestMethodItems(paths: Array[String]): Array[MethodTestItem]

}

object Org {

  /** Create a new virtual org for a workspace
    * @param path workspace directory
    */
  def newOrg(path: String): Org = {
    newOrg(Path(path))
  }

  /** Create a new virtual org for a workspace
    * @param path workspace directory
    */
  def newOrg(path: PathLike): Org = {
    newOrg(path, OpenOptions.default())
  }

  /** Create a new virtual org for a workspace
    * @param path workspace directory
    * @param options org options
    */
  def newOrg(path: PathLike, options: OpenOptions): Org = {
    // All should be options on the org, some are cached when the org is created
    options.loggingLevel.foreach(LoggerOps.setLoggingLevel)
    options.parser.foreach(ServerOps.setCurrentParser)
    options.externalAnalysisMode.foreach(mode =>
      ServerOps.setExternalAnalysis(ExternalAnalysisConfiguration(mode._1, mode._2))
    )
    options.cacheDirectory.foreach(path => {
      Environment.setCacheDirOverride(Some(Some(Path(path))))
      ServerOps.setAutoFlush(true)
    })
    options.indexerConfiguration.foreach(values =>
      ServerOps.setIndexerConfiguration(IndexerConfiguration(values._1, values._2))
    )
    options.autoFlush.foreach(enabled => ServerOps.setAutoFlush(enabled))
    options.cache.foreach(enabled =>
      if (!enabled) {
        Environment.setCacheDirOverride(Some(None))
        ServerOps.setAutoFlush(false)
      }
    )
    options.unused.foreach(enabled =>
      if (!enabled) PluginsManager.removePlugins(Seq(classOf[UnusedPlugin]))
    )

    LoggerOps.infoTime(
      s"Org created",
      show = true,
      s" with autoFlush = ${ServerOps.isAutoFlushEnabled}, build = ${BuildInfo.implementationBuild}"
    ) {
      val issueManager = new IssuesManager()
      val ws           = Workspace(path, issueManager)
      val org          = new OPM.OrgImpl(path, issueManager, ws)
      org
    }
  }
}
