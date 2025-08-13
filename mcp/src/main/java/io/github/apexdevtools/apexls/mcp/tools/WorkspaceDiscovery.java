/*
Copyright (c) 2025 Kevin Jones, All rights reserved.
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

package io.github.apexdevtools.apexls.mcp.tools;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Utility for discovering Salesforce workspace directories by traversing up the file system to find
 * the nearest sfdx-project.json file.
 */
public class WorkspaceDiscovery {

  /**
   * Finds the workspace directory containing sfdx-project.json by traversing up from the given file
   * path.
   *
   * @param filePath the file path to start searching from
   * @return the workspace directory path, or null if no workspace is found
   */
  public static String findWorkspace(String filePath) {
    if (filePath == null || filePath.trim().isEmpty()) {
      return null;
    }

    Path currentPath = Paths.get(filePath).toAbsolutePath();

    // If the path is a file, start from its parent directory
    File currentFile = currentPath.toFile();
    if (currentFile.isFile()) {
      currentPath = currentPath.getParent();
    }

    // Traverse up the directory tree looking for sfdx-project.json
    while (currentPath != null) {
      File sfdxProject = new File(currentPath.toFile(), "sfdx-project.json");
      if (sfdxProject.exists() && sfdxProject.isFile()) {
        return currentPath.toString();
      }
      currentPath = currentPath.getParent();
    }

    return null;
  }

  /**
   * Validates that a workspace directory is valid for Apex analysis.
   *
   * @param workspace the workspace path to validate
   * @return true if workspace is valid, false otherwise
   */
  public static boolean isValidWorkspace(String workspace) {
    if (workspace == null || workspace.trim().isEmpty()) {
      return false;
    }

    File workspaceDir = new File(workspace);
    if (!workspaceDir.exists() || !workspaceDir.isDirectory()) {
      return false;
    }

    File sfdxProject = new File(workspaceDir, "sfdx-project.json");
    return sfdxProject.exists() && sfdxProject.isFile();
  }
}
