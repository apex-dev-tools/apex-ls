/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.spi;

import com.nawforce.pkgforce.api.Issue;

/* Service provider for external analysis that can return Issues */
public interface AnalysisProvider {
    Issue[] collectIssues(String[] files);
}
