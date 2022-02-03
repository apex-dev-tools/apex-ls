/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.api;

import com.nawforce.pkgforce.diagnostics.IssuesAnd;
import com.nawforce.pkgforce.workspace.Workspace;
import com.nawforce.runtime.platform.Path;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Optional;

public class MDIndex {
    private final Workspace ws;

    /**
     * Create a new MDIndex from a workspace path. Creation may fail if there are fatal errors in the workspace
     * configuration. Errors & Warnings are automatically logged in the singleton IssueLog.
     */
    public static Optional<MDIndex> create(String path) {
        IssuesAnd<Option<Workspace>> wsAndIssues = Workspace.apply(Path.apply(path));

        IssuesLog log = IssuesLog.getInstance();
        CollectionConverters.asJava(wsAndIssues.issues()).forEach(log::add);

        if (wsAndIssues.value().nonEmpty())
            return Optional.of(new MDIndex(wsAndIssues.value().get()));
        return Optional.empty();
    }

    private MDIndex(Workspace ws) {
        this.ws = ws;
    }
}
