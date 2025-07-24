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

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class WorkspaceDiscoveryTest {

  @Test
  public void testFindWorkspaceFromFileInWorkspaceRoot(@TempDir Path tempDir) throws IOException {
    // Create sfdx-project.json in workspace root
    Path sfdxProject = tempDir.resolve("sfdx-project.json");
    Files.write(sfdxProject, "{}".getBytes());

    // Create a file in the workspace root
    Path testFile = tempDir.resolve("test.cls");
    Files.write(testFile, "public class Test {}".getBytes());

    String workspace = WorkspaceDiscovery.findWorkspace(testFile.toString());
    
    assertEquals(tempDir.toString(), workspace);
  }

  @Test
  public void testFindWorkspaceFromNestedFile(@TempDir Path tempDir) throws IOException {
    // Create sfdx-project.json in workspace root
    Path sfdxProject = tempDir.resolve("sfdx-project.json");
    Files.write(sfdxProject, "{}".getBytes());

    // Create nested directory structure
    Path forceAppMain = tempDir.resolve("force-app").resolve("main").resolve("default");
    Files.createDirectories(forceAppMain);
    
    Path classesDir = forceAppMain.resolve("classes");
    Files.createDirectories(classesDir);
    
    Path testFile = classesDir.resolve("MyClass.cls");
    Files.write(testFile, "public class MyClass {}".getBytes());

    String workspace = WorkspaceDiscovery.findWorkspace(testFile.toString());
    
    assertEquals(tempDir.toString(), workspace);
  }

  @Test
  public void testFindWorkspaceFromDirectory(@TempDir Path tempDir) throws IOException {
    // Create sfdx-project.json in workspace root
    Path sfdxProject = tempDir.resolve("sfdx-project.json");
    Files.write(sfdxProject, "{}".getBytes());

    // Create nested directory
    Path classesDir = tempDir.resolve("force-app").resolve("main").resolve("default").resolve("classes");
    Files.createDirectories(classesDir);

    String workspace = WorkspaceDiscovery.findWorkspace(classesDir.toString());
    
    assertEquals(tempDir.toString(), workspace);
  }

  @Test
  public void testFindWorkspaceWhenNoSfdxProjectExists(@TempDir Path tempDir) throws IOException {
    // Create a file without sfdx-project.json anywhere
    Path testFile = tempDir.resolve("test.cls");
    Files.write(testFile, "public class Test {}".getBytes());

    String workspace = WorkspaceDiscovery.findWorkspace(testFile.toString());
    
    assertNull(workspace);
  }

  @Test
  public void testFindWorkspaceWithNullPath() {
    String workspace = WorkspaceDiscovery.findWorkspace(null);
    assertNull(workspace);
  }

  @Test
  public void testFindWorkspaceWithEmptyPath() {
    String workspace = WorkspaceDiscovery.findWorkspace("");
    assertNull(workspace);
  }

  @Test
  public void testIsValidWorkspaceWithValidWorkspace(@TempDir Path tempDir) throws IOException {
    // Create sfdx-project.json
    Path sfdxProject = tempDir.resolve("sfdx-project.json");
    Files.write(sfdxProject, "{}".getBytes());

    boolean isValid = WorkspaceDiscovery.isValidWorkspace(tempDir.toString());
    
    assertTrue(isValid);
  }

  @Test
  public void testIsValidWorkspaceWithoutSfdxProject(@TempDir Path tempDir) {
    boolean isValid = WorkspaceDiscovery.isValidWorkspace(tempDir.toString());
    
    assertFalse(isValid);
  }

  @Test
  public void testIsValidWorkspaceWithNonExistentPath() {
    boolean isValid = WorkspaceDiscovery.isValidWorkspace("/non/existent/path");
    
    assertFalse(isValid);
  }

  @Test
  public void testIsValidWorkspaceWithNullPath() {
    boolean isValid = WorkspaceDiscovery.isValidWorkspace(null);
    
    assertFalse(isValid);
  }
}