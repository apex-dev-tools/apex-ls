/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.api;

public class IssuesLog {
    private static IssuesLog instance = null;

    public static IssuesLog getInstance() {
        if (instance == null)
            instance = new IssuesLog();
        return instance;
    }

    private IssuesLog() {
    }

    public void add(Issue issue) {
        // TODO
    }

}
