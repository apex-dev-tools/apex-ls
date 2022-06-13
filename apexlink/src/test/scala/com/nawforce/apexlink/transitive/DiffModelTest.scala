/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.transitive

import com.nawforce.apexlink.api.{Org, ServerOps}
import com.nawforce.apexlink.org.OPM
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents.ApexNature
import com.nawforce.pkgforce.names.TypeIdentifier

/* Compare dependency graph transitives against recursive analysis of type dependencies (as ApexFlow currently uses) */
object DiffModelTest {

  def main(args: Array[String]): Unit = {
    ServerOps.setAutoFlush(false)
    LoggerOps.setLoggingLevel(LoggerOps.INFO_LOGGING)

    if (args.isEmpty) {
      System.err.println(s"No workspace directory argument provided.")
      return
    }
    if (args.length > 1) {
      System.err.println(
        s"Multiple arguments provided, expected workspace directory, '${args.mkString(", ")}'}"
      )
      return
    }

    val org = Org.newOrg(args.head).asInstanceOf[OPM.OrgImpl]
    if (org.issues.issuesForFiles(null, includeWarnings = false, 0).length > 0)
      System.err.println("Warning: workspace has errors")

    val apexPkgAndFiles =
      org.packages.flatten(
        pkg =>
          pkg.modules
            .flatMap(module => module.index.get(ApexNature).map(doc => (pkg, doc.path.toString)))
      )
    println(s"Comparing output for ${apexPkgAndFiles.length} apex files")

    var failed = 0
    apexPkgAndFiles.foreach(apexPkgAndFile => {
      val (pkg, path) = apexPkgAndFile
      val identifier  = pkg.getTypeOfPath(path)
      if (identifier == null) {
        System.err.println(s"No package identifier found for path '$path', exiting")
        System.exit(1)
      }

      val manualTransitives    = getTransitiveDependencies(org, identifier)
      val graphTransitiveCount = getGraphTransitiveDependencies(org, identifier)
      val countTransitives = org
        .getDependencyCounts(Array(path), excludeTestClasses = false)
        .find(_._1 == path)
        .map(_._2)
        .getOrElse(-1)

      if (path.contains("ContractsServiceImpl")) {
        println(manualTransitives.map(_.toString).mkString(", "))
      }

      val result =
        if (
          manualTransitives.length != graphTransitiveCount || manualTransitives.length != countTransitives
        ) {
          failed += 1
          "FAILED"
        } else {
          "OK"
        }

      println(
        s"$result $identifier Manual=${manualTransitives.length} Graph=$graphTransitiveCount Count=$countTransitives"
      )
    })
    println(s"Compare failed for $failed apex files")
  }

  private def getGraphTransitiveDependencies(org: Org, identifier: TypeIdentifier): Integer = {
    val graph =
      org.getDependencyGraph(Array(identifier), 0, apexOnly = true, Array())
    if (graph.nodeData.length == 1) {
      graph.nodeData.head.transitiveCount
    } else {
      -1
    }
  }

  /* A simple walk of the getDependencies() results */
  private def getTransitiveDependencies(
    org: Org,
    identifier: TypeIdentifier
  ): Array[TypeIdentifier] = {
    def getDeps(id: TypeIdentifier) = {
      getDependencies(org, id).filter(id => id.namespace == identifier.namespace)
    }

    var deps     = getDeps(identifier)
    var len: Int = 0
    do {
      len = deps.length
      val newDeps = deps.flatMap(getDeps)
      deps = (deps ++ newDeps).distinct
    } while (len < deps.length)
    deps.filterNot(_ == identifier)
  }

  private def getDependencies(org: Org, identifier: TypeIdentifier): Array[TypeIdentifier] = {
    org
      .getPackages()
      .flatMap(
        pkg =>
          Option(pkg.getDependencies(identifier, outerInheritanceOnly = false, apexOnly = true))
            .getOrElse(Array())
      )
  }
}
