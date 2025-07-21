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

import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for workspace validation across all MCP tools.
 * Tests the WorkspaceValidator helper class and its usage in tools.
 */
class WorkspaceValidationTest extends BaseMCPToolTest {
    
    @TempDir
    Path tempDir;
    
    private SfdxCodeDiagnosticsTool sfdxDiagnosticsTool;
    private ApexFindReferencesTool findReferencesTool;
    private ApexGotoDefinitionTool gotoDefinitionTool;
    
    @BeforeEach 
    void setUpTools() {
        sfdxDiagnosticsTool = new SfdxCodeDiagnosticsTool(bridge);
        findReferencesTool = new ApexFindReferencesTool(bridge);
        gotoDefinitionTool = new ApexGotoDefinitionTool(bridge);
    }
    
    @Test
    @DisplayName("WorkspaceValidator should reject null workspace")
    void workspaceValidatorShouldRejectNullWorkspace() {
        CallToolResult result = WorkspaceValidator.validateWorkspace(null);
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("workspace argument is required"));
    }
    
    @Test
    @DisplayName("WorkspaceValidator should reject empty workspace")
    void workspaceValidatorShouldRejectEmptyWorkspace() {
        CallToolResult result = WorkspaceValidator.validateWorkspace("");
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("workspace argument is required"));
    }
    
    @Test
    @DisplayName("WorkspaceValidator should reject whitespace-only workspace")
    void workspaceValidatorShouldRejectWhitespaceOnlyWorkspace() {
        CallToolResult result = WorkspaceValidator.validateWorkspace("   ");
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("workspace argument is required"));
    }
    
    @Test
    @DisplayName("WorkspaceValidator should reject non-existent directory")
    void workspaceValidatorShouldRejectNonExistentDirectory() {
        String nonExistentPath = tempDir.resolve("does-not-exist").toString();
        CallToolResult result = WorkspaceValidator.validateWorkspace(nonExistentPath);
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("directory does not exist"));
        assertTrue(getContentAsString(result).contains(nonExistentPath));
    }
    
    @Test
    @DisplayName("WorkspaceValidator should reject file instead of directory")
    void workspaceValidatorShouldRejectFileInsteadOfDirectory() throws IOException {
        Path filePath = tempDir.resolve("not-a-directory.txt");
        Files.write(filePath, "test content".getBytes());
        
        CallToolResult result = WorkspaceValidator.validateWorkspace(filePath.toString());
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("path is not a directory"));
    }
    
    @Test
    @DisplayName("WorkspaceValidator should reject directory without sfdx-project.json")
    void workspaceValidatorShouldRejectDirectoryWithoutSfdxProject() throws IOException {
        Path emptyDir = tempDir.resolve("empty-workspace");
        Files.createDirectory(emptyDir);
        
        CallToolResult result = WorkspaceValidator.validateWorkspace(emptyDir.toString());
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("must contain sfdx-project.json"));
    }
    
    @Test
    @DisplayName("WorkspaceValidator should accept valid workspace")
    void workspaceValidatorShouldAcceptValidWorkspace() throws IOException {
        Path validWorkspace = tempDir.resolve("valid-workspace");
        Files.createDirectory(validWorkspace);
        Files.write(validWorkspace.resolve("sfdx-project.json"), "{\"packageDirectories\": []}".getBytes());
        
        CallToolResult result = WorkspaceValidator.validateWorkspace(validWorkspace.toString());
        
        assertNull(result); // null means validation passed
    }
    
    @Test
    @DisplayName("SfdxCodeDiagnosticsTool should validate workspace")
    void sfdxCodeDiagnosticsToolShouldValidateWorkspace() throws Exception {
        Map<String, Object> args = createArguments(null);
        CallToolResult result = executeTool(sfdxDiagnosticsTool, args);
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("workspace argument is required"));
    }
    
    @Test
    @DisplayName("ApexFindReferencesTool should validate workspace from path")
    void apexFindReferencesToolShouldValidateWorkspaceFromPath() throws Exception {
        Map<String, Object> args = createArgumentsMap(
            "path", "/non/existent/path/file.cls", 
            "line", 0, 
            "offset", 10);
        CallToolResult result = executeTool(findReferencesTool, args);
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("Could not find workspace directory containing sfdx-project.json"));
    }
    
    @Test
    @DisplayName("ApexGotoDefinitionTool should validate workspace from path")
    void apexGotoDefinitionToolShouldValidateWorkspaceFromPath() throws Exception {
        Map<String, Object> args = createArgumentsMap(
            "path", "/non/existent/path/file.cls", 
            "line", 0, 
            "offset", 10);
        CallToolResult result = executeTool(gotoDefinitionTool, args);
        
        assertNotNull(result);
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("Could not find workspace directory containing sfdx-project.json"));
    }
    
    @Test
    @DisplayName("All tools should validate workspace before processing")
    void allToolsShouldValidateWorkspaceBeforeProcessing() throws Exception {
        // Create invalid workspace (directory without sfdx-project.json)
        Path invalidWorkspace = tempDir.resolve("invalid-workspace");
        Files.createDirectory(invalidWorkspace);
        String invalidPath = invalidWorkspace.toString();
        
        // Test static analysis tool
        Map<String, Object> staticArgs = createArguments(invalidPath);
        CallToolResult staticResult = executeTool(sfdxDiagnosticsTool, staticArgs);
        assertNotNull(staticResult);
        assertTrue(staticResult.isError());
        assertTrue(getContentAsString(staticResult).contains("sfdx-project.json"));
        
        // Test find references tool
        Map<String, Object> referencesArgs = createArgumentsMap(
            "path", invalidPath + "/some/file.cls", "line", 0, "offset", 10);
        CallToolResult referencesResult = executeTool(findReferencesTool, referencesArgs);
        assertNotNull(referencesResult);
        assertTrue(referencesResult.isError());
        assertTrue(getContentAsString(referencesResult).contains("sfdx-project.json"));
        
        // Test goto definition tool
        Map<String, Object> definitionArgs = createArgumentsMap(
            "path", invalidPath + "/some/file.cls", "line", 0, "offset", 10);
        CallToolResult definitionResult = executeTool(gotoDefinitionTool, definitionArgs);
        assertNotNull(definitionResult);
        assertTrue(definitionResult.isError());
        assertTrue(getContentAsString(definitionResult).contains("sfdx-project.json"));
    }
    
    @Test
    @DisplayName("Valid workspace should pass validation in all tools")
    void validWorkspaceShouldPassValidationInAllTools() throws Exception {
        // Use the test workspace from resources which has sfdx-project.json
        String validWorkspace = testWorkspacePath;
        
        // Test static analysis tool (should pass validation but may fail at bridge level)
        Map<String, Object> staticArgs = createArguments(validWorkspace);
        CallToolResult staticResult = executeTool(sfdxDiagnosticsTool, staticArgs);
        assertNotNull(staticResult);
        // Should not have workspace validation errors
        assertFalse(getContentAsString(staticResult).contains("workspace argument is required"));
        assertFalse(getContentAsString(staticResult).contains("directory does not exist"));
        assertFalse(getContentAsString(staticResult).contains("sfdx-project.json"));
        
        // Test find references tool (should pass validation but may fail at bridge level)
        Map<String, Object> referencesArgs = createArgumentsMap(
            "path", validWorkspace + "/force-app/main/default/classes/TestClass.cls", 
            "line", 0, "offset", 10);
        CallToolResult referencesResult = executeTool(findReferencesTool, referencesArgs);
        assertNotNull(referencesResult);
        // Should not have workspace validation errors
        assertFalse(getContentAsString(referencesResult).contains("workspace argument is required"));
        assertFalse(getContentAsString(referencesResult).contains("directory does not exist"));
        assertFalse(getContentAsString(referencesResult).contains("Could not find workspace directory containing sfdx-project.json"));
        
        // Test goto definition tool (should pass validation but may fail at bridge level)
        Map<String, Object> definitionArgs = createArgumentsMap(
            "path", validWorkspace + "/force-app/main/default/classes/TestClass.cls", 
            "line", 0, "offset", 10);
        CallToolResult definitionResult = executeTool(gotoDefinitionTool, definitionArgs);
        assertNotNull(definitionResult);
        // Should not have workspace validation errors
        assertFalse(getContentAsString(definitionResult).contains("workspace argument is required"));
        assertFalse(getContentAsString(definitionResult).contains("directory does not exist"));
        assertFalse(getContentAsString(definitionResult).contains("Could not find workspace directory containing sfdx-project.json"));
    }
}