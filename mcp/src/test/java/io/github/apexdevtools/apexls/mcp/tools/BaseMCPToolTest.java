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

import io.github.apexdevtools.apexls.mcp.MCPServerConfig;
import io.github.apexdevtools.apexls.mcp.bridge.ApexLsBridge;
import io.github.apexdevtools.apexls.mcp.bridge.EmbeddedApexLsBridge;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;

import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.lang.reflect.Method;

/**
 * Base class for MCP tool tests that provides common setup and utilities.
 * Uses a real test workspace instead of mocking for more realistic testing.
 */
public abstract class BaseMCPToolTest {
    
    protected ApexLsBridge bridge;
    protected String testWorkspacePath;
    
    /** Helper method to create default test configuration. */
    protected MCPServerConfig createTestConfig() {
        return new MCPServerConfig("none", false);
    }
    
    @BeforeEach
    void setUp() throws Exception {
        // Get the test workspace path from resources
        URL testWorkspaceUrl = getClass().getClassLoader().getResource("test-workspace");
        if (testWorkspaceUrl == null) {
            throw new RuntimeException("Test workspace not found in resources");
        }
        testWorkspacePath = new File(testWorkspaceUrl.toURI()).getAbsolutePath();
        
        // Initialize the bridge
        bridge = new EmbeddedApexLsBridge(createTestConfig());
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
    
    /**
     * Helper method to create tool arguments for testing (with workspace parameter).
     */
    protected Map<String, Object> createArguments(String workspace, Object... keyValuePairs) {
        Map<String, Object> args = new HashMap<>();
        args.put("workspace", workspace);
        
        for (int i = 0; i < keyValuePairs.length; i += 2) {
            if (i + 1 < keyValuePairs.length) {
                args.put((String) keyValuePairs[i], keyValuePairs[i + 1]);
            }
        }
        
        return args;
    }

    /**
     * Helper method to create tool arguments for testing (without workspace parameter).
     */
    protected Map<String, Object> createArgumentsMap(Object... keyValuePairs) {
        Map<String, Object> args = new HashMap<>();
        
        for (int i = 0; i < keyValuePairs.length; i += 2) {
            if (i + 1 < keyValuePairs.length) {
                args.put((String) keyValuePairs[i], keyValuePairs[i + 1]);
            }
        }
        
        return args;
    }
    
    /**
     * Helper method to get the full path to a test file.
     */
    protected String getTestFilePath(String relativePath) {
        return testWorkspacePath + "/force-app/main/default/classes/" + relativePath;
    }
    
    /**
     * Helper method to execute a tool directly using reflection.
     * This avoids the complexity of the MCP SDK API for testing.
     */
    protected CallToolResult executeTool(Object tool, Map<String, Object> arguments) throws Exception {
        Method executeMethod = tool.getClass().getDeclaredMethod("execute", Object.class, Map.class);
        executeMethod.setAccessible(true);
        return (CallToolResult) executeMethod.invoke(tool, null, arguments);
    }
    
    /**
     * Helper method to get the content as a string from CallToolResult.
     */
    protected String getContentAsString(CallToolResult result) {
        if (result.content() == null || result.content().isEmpty()) {
            return "";
        }
        // For now, just return the string representation
        // The MCP SDK structure varies, so we'll use toString() as a fallback
        var content = result.content().get(0);
        return content.toString();
    }
}