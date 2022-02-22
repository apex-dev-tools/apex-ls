/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.api;

import com.financialforce.oparser.TypeDeclaration;
import com.nawforce.pkgforce.diagnostics.IssuesManager;
import com.nawforce.pkgforce.path.PathLike;
import com.nawforce.runtime.platform.Path;
import com.nawforce.runtime.workspace.IPM;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.OptionConverters;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.LinkedList;
import java.util.Optional;
import java.util.stream.Collectors;

public class MDIndex implements IssuesCollection {
    private final IPM.Index index;
    private final Optional<IPM.Module> rootModule;

    public MDIndex(String path) {
        this(Path.apply(path));
    }

    public MDIndex(PathLike path) {
        index = new IPM.Index(path);
        rootModule = OptionConverters.toJava(index.rootModule());
    }

    public TypeDeclaration findExactTypeId(String name) {
        return rootModule
                .flatMap(module -> OptionConverters.toJava(module.findExactTypeId(name)))
                .orElse(null);
    }

    public TypeDeclaration fuzzyFindTypeId(String name) {
        return rootModule
                .flatMap(module -> OptionConverters.toJava(module.fuzzyFindTypeId(name)))
                .orElse(null);
    }

    public List<TypeDeclaration> fuzzyFindTypeIds(String name) {
        return rootModule
                .map(module -> CollectionConverters.asJava(module.fuzzyFindTypeIds(name)))
                .orElse(new LinkedList<>());
    }

    public List<TypeDeclaration> findTypeIdsByNamespace(String namespace) {
        return rootModule
                .map(module -> CollectionConverters.asJava(module.findTypeIdsByNamespace(namespace)))
                .orElse(new LinkedList<>());
    }

    public List<String> getFilesWithErrors() {
        return Arrays.stream(issues().issuesForFiles(null, false, 1))
                .map(Issue::filePath).collect(Collectors.toList());
    }

    public ApexResourceFile getResourceFile(String uriFilename) {

        final String filename = URIToPath(uriFilename);

        return rootModule
                .map(module -> CollectionConverters.asJava(module.getTypesByPath(filename)))
                .filter(types -> !types.isEmpty())
                .map(types -> new ApexResourceFile(types.get(0).path(), types, issuesForFile(filename).length > 0))
                .orElse(null);
    }

    public ApexResourceFile findResourceFile(String uriFilename) {

        final String filename = URIToPath(uriFilename);

        return rootModule
                .map(module -> CollectionConverters.asJava(module.findTypesByPath(filename)))
                .filter(types -> !types.isEmpty())
                .map(types -> new ApexResourceFile(types.get(0).path(), types, issuesForFile(filename).length > 0))
                .orElse(null);
    }

    public List<ApexResourceFile> fuzzyFindResourceFile(String uriFilename) {

        final String filename = URIToPath(uriFilename);

        return rootModule
                .map(module -> CollectionConverters.asJava(module.fuzzyFindTypesByPath(filename))
                        .stream()
                        .collect(Collectors.groupingBy(TypeDeclaration::path)))
                        .map(groups -> groups.entrySet()
                                .stream()
                                .map(group -> new ApexResourceFile(group.getKey(), group.getValue(), issuesForFile(group.getKey()).length > 0))
                                .sorted(Comparator.comparing(ApexResourceFile::getFilename))
                                .collect(Collectors.toList())
                )
                .orElse(new LinkedList<>());
    }

    private String URIToPath(String uriFilename) {
        try {
            URI uri = new URI(uriFilename);
            return uri.getPath();
        }
        catch(URISyntaxException ex) {
            // ignored
        }
        return uriFilename;
    }

    private IssuesManager issues() {
        return index.issues();
    }

    @Override
    public String[] hasUpdatedIssues() {
        return issues().hasUpdatedIssues();
    }

    @Override
    public void ignoreUpdatedIssues(String path) {
        issues().ignoreUpdatedIssues(path);
    }

    @Override
    public Issue[] issuesForFile(String path) {
        return issues().issuesForFile(path);
    }

    @Override
    public Issue[] issuesForFileLocation(String path, IssueLocation location) {
        return issues().issuesForFileLocation(path, location);
    }

    @Override
    public Issue[] issuesForFiles(String[] paths, boolean includeWarnings, int maxErrorsPerFile) {
        return issues().issuesForFiles(paths, includeWarnings, maxErrorsPerFile);
    }
}
