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

import io.github.apexdevtools.apexls.mcp.bridge.ApexLsBridge;
import io.github.apexdevtools.apexls.mcp.bridge.EmbeddedApexLsBridge;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.io.File;
import java.net.URL;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Simple integration test that verifies MCP tools can be created and basic functionality works.
 * This is a minimal test to ensure the integration is working without complex MCP SDK interactions.
 */
class SimpleMCPToolTest {
    
    private ApexLsBridge bridge;
    private String testWorkspacePath;
    
    @BeforeEach
    void setUp() throws Exception {
        // Get the test workspace path from resources
        URL testWorkspaceUrl = getClass().getClassLoader().getResource("test-workspace");
        if (testWorkspaceUrl == null) {
            throw new RuntimeException("Test workspace not found in resources");
        }
        testWorkspacePath = new File(testWorkspaceUrl.toURI()).getAbsolutePath();
        
        // Initialize the bridge
        bridge = new EmbeddedApexLsBridge();
        bridge.initialize();
        
        // Give the bridge a moment to initialize
        Thread.sleep(500);
    }
    
    @AfterEach
    void tearDown() throws Exception {
        if (bridge != null) {
            bridge.close();
        }
    }
    
    @Test
    @DisplayName("Should create all MCP tools successfully")
    void shouldCreateAllMCPToolsSuccessfully() {
        // Test that all tools can be instantiated
        SfdxCodeDiagnosticsTool sfdxDiagnosticsTool = new SfdxCodeDiagnosticsTool(bridge);
        ApexFindUsagesTool findUsagesTool = new ApexFindUsagesTool(bridge);
        ApexGotoDefinitionTool gotoDefinitionTool = new ApexGotoDefinitionTool(bridge);
        
        assertNotNull(sfdxDiagnosticsTool);
        assertNotNull(findUsagesTool);
        assertNotNull(gotoDefinitionTool);
        
        // Test that tool specifications can be created
        assertNotNull(sfdxDiagnosticsTool.getSpecification());
        assertNotNull(findUsagesTool.getSpecification());
        assertNotNull(gotoDefinitionTool.getSpecification());
        
        // Verify tool names
        assertEquals("sfdx_code_diagnostics", sfdxDiagnosticsTool.getSpecification().tool().name());
        assertEquals("apex_find_usages", findUsagesTool.getSpecification().tool().name());
        assertEquals("apex_goto_definition", gotoDefinitionTool.getSpecification().tool().name());
    }
    
    @Test
    @DisplayName("Bridge should work with real workspace")
    void bridgeShouldWorkWithRealWorkspace() throws Exception {
        assertTrue(bridge.isReady(), "Bridge should be ready");
        
        // Test version call
        CompletableFuture<String> versionFuture = bridge.getVersion();
        String version = versionFuture.get(10, TimeUnit.SECONDS);
        assertNotNull(version);
        assertFalse(version.trim().isEmpty());
        
        // Test workspace analysis (this will take longer)
        CompletableFuture<String> issuesFuture = bridge.getIssues(testWorkspacePath, false, 100);
        String issues = issuesFuture.get(30, TimeUnit.SECONDS);
        assertNotNull(issues);
        assertFalse(issues.trim().isEmpty());
        
        // Should contain either issues found or analysis passed
        assertTrue(issues.contains("issue") || issues.contains("analysis") || issues.contains("found"),
                  "Issues result should be meaningful: " + issues);
    }
    
    @Test
    @DisplayName("Bridge should handle find usages")
    void bridgeShouldHandleFindUsages() throws Exception {
        String testFilePath = testWorkspacePath + "/force-app/main/default/classes/TestClass.cls";
        
        CompletableFuture<String> usagesFuture = bridge.findUsages(testWorkspacePath, testFilePath, 1, 15);
        String usages = usagesFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(usages);
        assertFalse(usages.trim().isEmpty());
        assertTrue(usages.contains("usage") || usages.contains("found"));
    }
    
    @Test
    @DisplayName("Bridge should handle get definition")
    void bridgeShouldHandleGetDefinition() throws Exception {
        String testFilePath = testWorkspacePath + "/force-app/main/default/classes/AnotherClass.cls";
        
        CompletableFuture<String> definitionFuture = bridge.getDefinition(testWorkspacePath, testFilePath, 3, 10);
        String definition = definitionFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(definition);
        assertFalse(definition.trim().isEmpty());
        assertTrue(definition.contains("definition") || definition.contains("found"));
    }
    
    @Test
    @DisplayName("Bridge should handle workspace info")
    void bridgeShouldHandleWorkspaceInfo() throws Exception {
        CompletableFuture<String> workspaceInfoFuture = bridge.getWorkspaceInfo(testWorkspacePath);
        String workspaceInfo = workspaceInfoFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(workspaceInfo);
        assertFalse(workspaceInfo.trim().isEmpty());
        
        // Should be JSON-like format
        assertTrue(workspaceInfo.contains("workspace") || workspaceInfo.contains("{"));
    }
    
    @Test
    @DisplayName("Bridge should cache workspace instances efficiently")
    void bridgeShouldCacheWorkspaceInstancesEfficiently() throws Exception {
        // Make multiple calls - subsequent calls should be faster due to caching
        // Use nanoTime for more precise timing measurements
        long start1 = System.nanoTime();
        CompletableFuture<String> issues1 = bridge.getIssues(testWorkspacePath, false, 100);
        String result1 = issues1.get(30, TimeUnit.SECONDS);
        long time1Nanos = System.nanoTime() - start1;
        long time1 = time1Nanos / 1_000_000; // Convert to milliseconds
        
        long start2 = System.nanoTime();
        CompletableFuture<String> issues2 = bridge.getIssues(testWorkspacePath, true, 50);
        String result2 = issues2.get(30, TimeUnit.SECONDS);
        long time2Nanos = System.nanoTime() - start2;
        long time2 = time2Nanos / 1_000_000; // Convert to milliseconds
        
        assertNotNull(result1);
        assertNotNull(result2);
        
        // Both calls should complete (timing should be positive, but handle sub-millisecond precision)
        assertTrue(time1Nanos > 0, "First call should take some time");
        assertTrue(time2Nanos > 0, "Second call should take some time");
        
        System.out.println("First call time: " + time1 + "ms, Second call time: " + time2 + "ms");
        
        // The second call should be faster in nanoseconds (more precise than milliseconds)
        // If caching is working, second call should be significantly faster
        assertTrue(time2Nanos < time1Nanos * 0.8, 
                  String.format("Second call (%d ns) should be faster than first call (%d ns)", time2Nanos, time1Nanos));
    }
}