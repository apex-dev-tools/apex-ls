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

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test class for ApexFindImpactedTestsTool.
 * Tests both single and multi-workspace scenarios.
 */
class ApexFindImpactedTestsToolTest extends BaseMCPToolTest {

    @Test
    @DisplayName("Should create tool successfully")
    void shouldCreateToolSuccessfully() {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        assertNotNull(tool);
        
        var specification = tool.getSpecification();
        assertNotNull(specification);
        assertEquals("apex_find_impacted_tests", specification.tool().name());
        assertEquals("Find test classes that should be run based on changes to specific Apex source files", 
                     specification.tool().description());
    }

    @Test
    @DisplayName("Should require changed_paths argument")
    void shouldRequireChangedPathsArgument() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        // Test with missing changed_paths
        Map<String, Object> argsWithoutPaths = createArgumentsMap();
        CallToolResult result = executeTool(tool, argsWithoutPaths);
        
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("changed_paths argument is required"));
    }

    @Test
    @DisplayName("Should reject empty changed_paths array")
    void shouldRejectEmptyChangedPathsArray() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        Map<String, Object> argsWithEmptyArray = createArgumentsMap("changed_paths", Arrays.asList());
        CallToolResult result = executeTool(tool, argsWithEmptyArray);
        
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("changed_paths cannot be empty"));
    }

    @Test
    @DisplayName("Should reject non-array changed_paths")
    void shouldRejectNonArrayChangedPaths() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        Map<String, Object> argsWithString = createArgumentsMap("changed_paths", "not_an_array");
        CallToolResult result = executeTool(tool, argsWithString);
        
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("changed_paths must be an array"));
    }

    @Test
    @DisplayName("Should reject non-string items in changed_paths")
    void shouldRejectNonStringItemsInChangedPaths() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        Map<String, Object> argsWithNonStrings = createArgumentsMap("changed_paths", 
            Arrays.asList("valid_path.cls", 123, "another_valid_path.cls"));
        CallToolResult result = executeTool(tool, argsWithNonStrings);
        
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("All paths in changed_paths must be strings"));
    }

    @Test
    @DisplayName("Should handle single workspace with valid paths")
    void shouldHandleSingleWorkspaceWithValidPaths() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        List<String> testPaths = Arrays.asList(
            getTestFilePath("TestClass.cls"),
            getTestFilePath("AnotherClass.cls")
        );
        
        Map<String, Object> args = createArgumentsMap("changed_paths", testPaths);
        CallToolResult result = executeTool(tool, args);
        
        assertFalse(result.isError());
        String response = getContentAsString(result);
        assertNotNull(response);
        
        // Should contain expected JSON structure
        assertTrue(response.contains("apex_find_impacted_tests"));
        assertTrue(response.contains("status"));
        assertTrue(response.contains("impacted_test_files"));
        assertTrue(response.contains("counts"));
    }

    @Test
    @DisplayName("Should handle paths outside any workspace")
    void shouldHandlePathsOutsideAnyWorkspace() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        List<String> invalidPaths = Arrays.asList(
            "/tmp/NonExistentFile.cls",
            "/another/invalid/path.cls"
        );
        
        Map<String, Object> args = createArgumentsMap("changed_paths", invalidPaths);
        CallToolResult result = executeTool(tool, args);
        
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("No valid workspaces found"));
    }

    @Test
    @DisplayName("Should handle mixed valid and invalid paths")
    void shouldHandleMixedValidAndInvalidPaths() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        List<String> mixedPaths = Arrays.asList(
            getTestFilePath("TestClass.cls"),  // Valid
            "/tmp/invalid.cls",                // Invalid
            getTestFilePath("AnotherClass.cls") // Valid
        );
        
        Map<String, Object> args = createArgumentsMap("changed_paths", mixedPaths);
        CallToolResult result = executeTool(tool, args);
        
        // Should succeed with valid paths and skip invalid ones
        assertFalse(result.isError());
        String response = getContentAsString(result);
        
        // Should mention that some files were skipped
        assertTrue(response.contains("1 files skipped - no workspace found") || 
                  response.contains("files skipped"));
    }

    @Test
    @DisplayName("Should validate JSON response structure")
    void shouldValidateJsonResponseStructure() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        List<String> testPaths = Arrays.asList(getTestFilePath("TestClass.cls"));
        Map<String, Object> args = createArgumentsMap("changed_paths", testPaths);
        CallToolResult result = executeTool(tool, args);
        
        assertFalse(result.isError());
        String response = getContentAsString(result);
        
        // Validate JSON structure contains required fields
        assertTrue(response.contains("\"tool\": \"apex_find_impacted_tests\""));
        assertTrue(response.contains("\"status\": \"completed\""));
        assertTrue(response.contains("\"summary\":"));
        assertTrue(response.contains("\"counts\":"));
        assertTrue(response.contains("\"total_impacted_tests\":"));
        assertTrue(response.contains("\"changed_files_analyzed\":"));
        assertTrue(response.contains("\"workspaces_analyzed\":"));
        assertTrue(response.contains("\"impacted_test_files\":"));
    }

    @Test
    @DisplayName("Should handle bridge errors gracefully")
    void shouldHandleBridgeErrorsGracefully() throws Exception {
        // Create tool with closed bridge to simulate bridge failure
        bridge.close();
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        List<String> testPaths = Arrays.asList(getTestFilePath("TestClass.cls"));
        Map<String, Object> args = createArgumentsMap("changed_paths", testPaths);
        CallToolResult result = executeTool(tool, args);
        
        assertTrue(result.isError());
        assertTrue(getContentAsString(result).contains("Error finding impacted tests"));
    }

    @Test
    @DisplayName("Should handle workspace discovery correctly")
    void shouldHandleWorkspaceDiscoveryCorrectly() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        // Use absolute paths within the test workspace
        List<String> testPaths = Arrays.asList(
            getTestFilePath("TestClass.cls"),
            getTestFilePath("AnotherClass.cls")
        );
        
        Map<String, Object> args = createArgumentsMap("changed_paths", testPaths);
        CallToolResult result = executeTool(tool, args);
        
        assertFalse(result.isError());
        String response = getContentAsString(result);
        
        // Should indicate 1 workspace analyzed since all files are in same workspace
        assertTrue(response.contains("\"workspaces_analyzed\": 1"));
        assertTrue(response.contains("\"changed_files_analyzed\": 2"));
    }

    @Test
    @DisplayName("Should handle file paths with various formats")
    void shouldHandleFilePathsWithVariousFormats() throws Exception {
        ApexFindImpactedTestsTool tool = new ApexFindImpactedTestsTool(bridge);
        
        // Mix of absolute and relative-looking paths (all should resolve to test workspace)
        List<String> testPaths = Arrays.asList(
            getTestFilePath("TestClass.cls"),
            getTestFilePath("AnotherClass.cls").replace("\\", "/") // Normalize path separators
        );
        
        Map<String, Object> args = createArgumentsMap("changed_paths", testPaths);
        CallToolResult result = executeTool(tool, args);
        
        assertFalse(result.isError());
        String response = getContentAsString(result);
        assertNotNull(response);
        assertTrue(response.contains("apex_find_impacted_tests"));
    }
}