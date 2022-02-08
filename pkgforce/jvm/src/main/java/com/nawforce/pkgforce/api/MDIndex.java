/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package com.nawforce.pkgforce.api;

import com.nawforce.pkgforce.diagnostics.IssuesAnd;
import com.nawforce.pkgforce.path.PathLike;
import com.nawforce.pkgforce.workspace.IPM;
import com.nawforce.pkgforce.workspace.Workspace;
import com.nawforce.runtime.platform.Path;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Optional;

public class MDIndex {
    private IPM.Index index;

    /**
     * Create a new MDIndex from a workspace path. Creation may fail if there are fatal errors in the workspace
     * configuration. Errors & Warnings are automatically logged in the singleton IssueLog.
     */
    public static MDIndex create(String path) {
        return create(Path.apply(path));
    }

    public static MDIndex create(PathLike path) {
        IssuesAnd<Option<Workspace>> wsAndIssues = Workspace.apply(path);

        IssuesLog log = IssuesLog.getInstance();
        CollectionConverters.asJava(wsAndIssues.issues()).forEach(log::add);

        return new MDIndex(path, wsAndIssues.value());
    }

    private MDIndex(PathLike path, Option<Workspace> ws) {
        index = new IPM.Index(path, ws);
    }
}
