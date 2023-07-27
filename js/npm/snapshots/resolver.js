/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
module.exports = {
  // resolves from test to snapshot path
  resolveSnapshotPath: (testPath, snapshotExtension) =>
    testPath.replace("src/__tests__", "snapshots") + snapshotExtension,

  // resolves from snapshot to test path
  resolveTestPath: (snapshotFilePath, snapshotExtension) =>
    snapshotFilePath
      .replace("snapshots", "src/__tests__")
      .slice(0, -snapshotExtension.length),

  // Example test path, used for preflight consistency check of the implementation above
  testPathForConsistencyCheck: "src/__tests__/example.test.ts",
};
