/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package io.github.apexdevtools.apexls.spi;

import io.github.apexdevtools.apexls.api.Issue;

import java.nio.file.Path;

/* Service provider for an external analysis that can return Issues */
public interface AnalysisProvider {

    /* Return an identifier for the provider, these need to be unique across all providers. */
    String getProviderId();

    /* Return issues for the set of files. */
    Issue[] collectIssues(Path workspacePath, Path[] files);
}
