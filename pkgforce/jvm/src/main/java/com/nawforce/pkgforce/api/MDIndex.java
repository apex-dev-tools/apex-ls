/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.api;

import com.financialforce.oparser.TypeDeclaration;
import com.nawforce.pkgforce.diagnostics.IssuesManager;
import com.nawforce.pkgforce.path.PathLike;
import com.nawforce.runtime.platform.Path;
import com.nawforce.runtime.workspace.IPM;
import scala.jdk.javaapi.OptionConverters;

import java.util.Optional;

public class MDIndex implements IssuesCollection {
    private final IPM.Index index;
    private final Optional<IPM.Module> rootModule;

    public MDIndex(String path) {
        this(Path.apply(path));
    }

    public MDIndex(PathLike path) {
        index = new IPM.Index(path);
        rootModule =  OptionConverters.toJava(index.rootModule());
    }

    public TypeDeclaration findExactTypeId(String name) {
        return rootModule.flatMap(module -> OptionConverters.toJava(module.findExactTypeId(name))).orElse(null);
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
