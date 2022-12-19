/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls.api;

import com.nawforce.pkgforce.diagnostics.IssuesManager;
import com.nawforce.pkgforce.path.PathLike;
import io.github.apexdevtools.apexls.types.ApexTypeAdapter;
import com.nawforce.runtime.platform.Path;
import com.nawforce.runtime.workspace.IModuleTypeDeclaration;
import com.nawforce.runtime.workspace.IPM;
import io.github.apexdevtools.api.Issue;
import io.github.apexdevtools.api.IssueLocation;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.OptionConverters;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
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

    public ApexType findExactTypeId(String name) {
        return rootModule
                .flatMap(module -> OptionConverters.toJava(module.findExactTypeId(name)))
                .map(ApexTypeAdapter::new)
                .orElse(null);
    }

    public ApexType fuzzyFindTypeId(String name) {
        return rootModule
                .flatMap(module -> OptionConverters.toJava(module.fuzzyFindTypeId(name)))
                .map(ApexTypeAdapter::new)
                .orElse(null);
    }

    public List<ApexType> fuzzyFindTypeIds(String name) {
        return rootModule
                .map(module -> CollectionConverters.asJava(module.fuzzyFindTypeIds(name)))
                .map(types -> types.stream().map(type -> (ApexType) new ApexTypeAdapter(type)).collect(Collectors.toList()))
                .orElse(new LinkedList<>());
    }

    public List<ApexType> findTypeIdsByNamespace(String namespace) {
        return rootModule
                .map(module -> CollectionConverters.asJava(module.findTypeIdsByNamespace(namespace)))
                .map(types -> types.stream().map(type -> (ApexType) new ApexTypeAdapter(type)).collect(Collectors.toList()))
                .orElse(new LinkedList<>());
    }

    public List<String> getFilesWithErrors() {
        return Arrays.stream(issues().issuesForFiles(null, false, 1))
                .map(Issue::filePath).collect(Collectors.toList());
    }

    public ApexResourceFile getResourceFile(String uriFilename) {

        final String filename = URIToPath(uriFilename);
        List<IModuleTypeDeclaration> allTypes = rootModule
                .map(module -> CollectionConverters.asJava(module.findTypesByPath(filename)))
                .filter(types -> !types.isEmpty())
                .orElse(null);

        if (allTypes == null)
            return null;

        // Use primary path for the type
        String typePath = allTypes.get(0).paths()[0];
        return new ApexResourceFile(typePath,
                allTypes.stream().map(type -> (ApexType) new ApexTypeAdapter(type)).collect(Collectors.toList()),
                issuesForFile(typePath).length > 0);
    }

    public ApexResourceFile findResourceFile(String uriFilename) {
        final String filename = URIToPath(uriFilename);
        List<IModuleTypeDeclaration> allTypes = rootModule
                .map(module -> CollectionConverters.asJava(module.findTypesByPath(filename)))
                .filter(types -> !types.isEmpty())
                .orElse(null);

        if (allTypes == null || allTypes.size() != 1)
            return null;

        // Use primary path for the type
        String typePath = allTypes.get(0).paths()[0];
        return new ApexResourceFile(typePath,
                allTypes.stream().map(type -> (ApexType) new ApexTypeAdapter(type)).collect(Collectors.toList()),
                issuesForFile(typePath).length > 0);
    }

    public List<ApexResourceFile> fuzzyFindResourceFile(String uriFilename) {

        final String filename = URIToPath(uriFilename);

        return rootModule
                .map(module -> CollectionConverters.asJava(module.fuzzyFindTypesByPath(filename))
                        .stream()
                        // Use primary path for the type
                        .collect(Collectors.groupingBy(td -> td.paths()[0])))
                        .map(groups -> groups.entrySet()
                                .stream()
                                .map(group -> new ApexResourceFile(group.getKey(),
                                        group.getValue().stream()
                                                .map(type -> (ApexType) new ApexTypeAdapter(type))
                                                .collect(Collectors.toList()),
                                        issuesForFile(group.getKey()).length > 0))
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
