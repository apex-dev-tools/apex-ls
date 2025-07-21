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

package io.github.apexdevtools.apexls.mcp.bridge;

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
 * Integration tests for EmbeddedApexLsBridge using a real test workspace.
 */
class EmbeddedApexLsBridgeTest {
    
    private EmbeddedApexLsBridge bridge;
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
        Thread.sleep(100);
    }
    
    @AfterEach
    void tearDown() throws Exception {
        if (bridge != null) {
            bridge.close();
        }
    }
    
    @Test
    @DisplayName("Should initialize successfully")
    void shouldInitializeSuccessfully() {
        assertTrue(bridge.isReady(), "Bridge should be ready after initialization");
    }
    
    @Test
    @DisplayName("Should get version information")
    void shouldGetVersionInformation() throws Exception {
        CompletableFuture<String> versionFuture = bridge.getVersion();
        
        assertNotNull(versionFuture);
        String version = versionFuture.get(10, TimeUnit.SECONDS);
        
        assertNotNull(version);
        assertFalse(version.trim().isEmpty(), "Version should not be empty");
    }
    
    @Test
    @DisplayName("Should get issues for workspace")
    void shouldGetIssuesForWorkspace() throws Exception {
        CompletableFuture<String> issuesFuture = bridge.getIssues(testWorkspacePath, false, 100);
        
        assertNotNull(issuesFuture);
        String issues = issuesFuture.get(30, TimeUnit.SECONDS); // Allow more time for workspace analysis
        
        assertNotNull(issues);
        assertFalse(issues.trim().isEmpty(), "Issues result should not be empty");
        assertTrue(issues.contains("issue") || issues.contains("analysis passed"), 
                  "Issues result should mention issues or success: " + issues);
    }
    
    @Test
    @DisplayName("Should get issues with warnings enabled")
    void shouldGetIssuesWithWarningsEnabled() throws Exception {
        CompletableFuture<String> issuesFuture = bridge.getIssues(testWorkspacePath, true, 100);
        
        assertNotNull(issuesFuture);
        String issues = issuesFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(issues);
        assertFalse(issues.trim().isEmpty());
    }
    
    @Test
    @DisplayName("Should get issues with limited max issues per file")
    void shouldGetIssuesWithLimitedMaxIssuesPerFile() throws Exception {
        CompletableFuture<String> issuesFuture = bridge.getIssues(testWorkspacePath, false, 50);
        
        assertNotNull(issuesFuture);
        String issues = issuesFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(issues);
        assertFalse(issues.trim().isEmpty());
    }
    
    @Test
    @DisplayName("Should find references")
    void shouldFindReferences() throws Exception {
        String testFilePath = testWorkspacePath + "/force-app/main/default/classes/TestClass.cls";
        
        CompletableFuture<String> referencesFuture = bridge.findReferences(testWorkspacePath, testFilePath, 1, 15);
        
        assertNotNull(referencesFuture);
        String references = referencesFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(references);
        assertFalse(references.trim().isEmpty(), "References result should not be empty");
        assertTrue(references.contains("reference") || references.contains("found"), 
                  "References result should mention references: " + references);
    }
    
    @Test
    @DisplayName("Should get definition")
    void shouldGetDefinition() throws Exception {
        String testFilePath = testWorkspacePath + "/force-app/main/default/classes/AnotherClass.cls";
        
        CompletableFuture<String> definitionFuture = bridge.getDefinition(testWorkspacePath, testFilePath, 3, 10);
        
        assertNotNull(definitionFuture);
        String definition = definitionFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(definition);
        assertFalse(definition.trim().isEmpty(), "Definition result should not be empty");
        assertTrue(definition.contains("definition") || definition.contains("found"), 
                  "Definition result should mention definition: " + definition);
    }
    
    @Test
    @DisplayName("Should get workspace info")
    void shouldGetWorkspaceInfo() throws Exception {
        CompletableFuture<String> workspaceInfoFuture = bridge.getWorkspaceInfo(testWorkspacePath);
        
        assertNotNull(workspaceInfoFuture);
        String workspaceInfo = workspaceInfoFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(workspaceInfo);
        assertFalse(workspaceInfo.trim().isEmpty(), "Workspace info should not be empty");
        
        // Should be JSON-like format
        assertTrue(workspaceInfo.contains("workspace") || workspaceInfo.contains("{"), 
                  "Workspace info should be JSON format: " + workspaceInfo);
    }
    
    @Test
    @DisplayName("Should handle invalid workspace path gracefully")
    void shouldHandleInvalidWorkspacePathGracefully() throws Exception {
        CompletableFuture<String> issuesFuture = bridge.getIssues("/invalid/workspace/path", false, 100);
        
        assertNotNull(issuesFuture);
        
        // Should either complete with error message or fail
        try {
            String issues = issuesFuture.get(10, TimeUnit.SECONDS);
            // If it completes, should contain error information
            assertNotNull(issues);
        } catch (Exception ex) {
            // It's acceptable for this to fail with an exception
            assertTrue(ex.getCause() != null || ex.getMessage() != null);
        }
    }
    
    @Test
    @DisplayName("Should handle multiple concurrent requests")
    void shouldHandleMultipleConcurrentRequests() throws Exception {
        CompletableFuture<String> versionFuture1 = bridge.getVersion();
        CompletableFuture<String> versionFuture2 = bridge.getVersion();
        CompletableFuture<String> issuesFuture = bridge.getIssues(testWorkspacePath, false, 100);
        
        // All should complete successfully
        String version1 = versionFuture1.get(10, TimeUnit.SECONDS);
        String version2 = versionFuture2.get(10, TimeUnit.SECONDS);
        String issues = issuesFuture.get(30, TimeUnit.SECONDS);
        
        assertNotNull(version1);
        assertNotNull(version2);
        assertNotNull(issues);
        
        // Versions should be the same
        assertEquals(version1, version2);
    }
    
    @Test
    @DisplayName("Should cache workspace instances")
    void shouldCacheWorkspaceInstances() throws Exception {
        // Make multiple calls to the same workspace
        CompletableFuture<String> issues1 = bridge.getIssues(testWorkspacePath, false, 100);
        CompletableFuture<String> issues2 = bridge.getIssues(testWorkspacePath, true, 50);
        
        String result1 = issues1.get(30, TimeUnit.SECONDS);
        String result2 = issues2.get(30, TimeUnit.SECONDS);
        
        assertNotNull(result1);
        assertNotNull(result2);
        
        // Both should complete successfully (testing that caching doesn't break functionality)
    }
    
    @Test
    @DisplayName("Should handle bridge not ready state")
    void shouldHandleBridgeNotReadyState() throws Exception {
        // Create a new bridge without initializing it
        EmbeddedApexLsBridge uninitializedBridge = new EmbeddedApexLsBridge();
        
        assertFalse(uninitializedBridge.isReady());
        
        // Calls should fail gracefully
        CompletableFuture<String> versionFuture = uninitializedBridge.getVersion();
        
        try {
            versionFuture.get(5, TimeUnit.SECONDS);
            fail("Should have failed for uninitialized bridge");
        } catch (Exception ex) {
            // Expected to fail
            assertTrue(ex.getCause() instanceof IllegalStateException || 
                      ex.getMessage().contains("not initialized"));
        }
        
        uninitializedBridge.close();
    }
}