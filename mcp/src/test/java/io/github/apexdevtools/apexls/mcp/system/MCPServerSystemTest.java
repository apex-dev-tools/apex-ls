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

package io.github.apexdevtools.apexls.mcp.system;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import java.util.UUID;
import java.util.Map;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;

/**
 * System tests for the MCP server using direct MCP protocol communication.
 * These tests verify that the MCP server can start up correctly and respond
 * to MCP protocol requests via STDIO.
 */
class MCPServerSystemTest {
    
    private static final Logger logger = LoggerFactory.getLogger(MCPServerSystemTest.class);
    
    private Process mcpServerProcess;
    private BufferedWriter serverInput;
    private BufferedReader serverOutput;
    private ObjectMapper objectMapper;
    private String testWorkspacePath;
    
    @BeforeEach
    void setUp() throws Exception {
        // Get the test workspace path from resources
        URL testWorkspaceUrl = getClass().getClassLoader().getResource("test-workspace");
        if (testWorkspaceUrl == null) {
            throw new RuntimeException("Test workspace not found in resources");
        }
        testWorkspacePath = new File(testWorkspaceUrl.toURI()).getAbsolutePath();
        
        objectMapper = new ObjectMapper();
    }
    
    @AfterEach
    void tearDown() throws Exception {
        if (mcpServerProcess != null && mcpServerProcess.isAlive()) {
            mcpServerProcess.destroyForcibly();
            if (!mcpServerProcess.waitFor(5, TimeUnit.SECONDS)) {
                logger.warn("MCP server process did not terminate gracefully");
            }
        }
        
        if (serverInput != null) {
            try {
                serverInput.close();
            } catch (IOException ex) {
                logger.warn("Error closing server input", ex);
            }
        }
        
        if (serverOutput != null) {
            try {
                serverOutput.close();
            } catch (IOException ex) {
                logger.warn("Error closing server output", ex);
            }
        }
    }
    
    @Test
    @DisplayName("Should start MCP server process successfully")
    @Timeout(30)
    void shouldStartMCPServerProcessSuccessfully() throws Exception {
        startMCPServer();
        
        assertTrue(mcpServerProcess.isAlive(), "MCP server process should be running");
        
        // Send initialize request
        Map<String, Object> initRequest = createMCPRequest("initialize", Map.of(
            "protocolVersion", "2024-11-05",
            "clientInfo", Map.of(
                "name", "test-client",
                "version", "1.0.0"
            )
        ));
        
        sendMCPRequest(initRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "Initialize should return result");
        
        logger.info("Successfully started MCP server and received initialize response");
    }
    
    @Test
    @DisplayName("Should list tools via MCP protocol")
    @Timeout(30)
    void shouldListToolsViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send list_tools request
        Map<String, Object> listToolsRequest = createMCPRequest("tools/list", Map.of());
        sendMCPRequest(listToolsRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "List tools should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("tools"), "Result should contain tools array");
        
        JsonNode tools = result.get("tools");
        assertTrue(tools.isArray(), "Tools should be an array");
        assertTrue(tools.size() > 0, "Should have at least one tool");
        
        // Check for expected tools
        boolean hasStaticAnalysis = false;
        boolean hasFindReferences = false;
        boolean hasGotoDefinition = false;
        
        for (JsonNode tool : tools) {
            String toolName = tool.get("name").asText();
            switch (toolName) {
                case "sfdx_code_diagnostics":
                    hasStaticAnalysis = true;
                    break;
                case "apex_find_usages":
                    hasFindReferences = true;
                    break;
                case "apex_find_definition":
                    hasGotoDefinition = true;
                    break;
            }
        }
        
        assertTrue(hasStaticAnalysis, "Should have sfdx_code_diagnostics tool");
        assertTrue(hasFindReferences, "Should have apex_find_usages tool");
        assertTrue(hasGotoDefinition, "Should have apex_find_definition tool");
        
        logger.info("Successfully listed {} tools via MCP protocol", tools.size());
    }
    
    @Test
    @DisplayName("Should list resources via MCP protocol")
    @Timeout(30)
    void shouldListResourcesViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send list_resources request
        Map<String, Object> listResourcesRequest = createMCPRequest("resources/list", Map.of());
        sendMCPRequest(listResourcesRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "List resources should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("resources"), "Result should contain resources array");
        
        JsonNode resources = result.get("resources");
        assertTrue(resources.isArray(), "Resources should be an array");
        assertTrue(resources.size() > 0, "Should have at least one resource");
        
        logger.info("Successfully listed {} resources via MCP protocol", resources.size());
    }
    
    @Test
    @DisplayName("Should execute sfdx_code_diagnostics tool via MCP protocol")
    @Timeout(60)
    void shouldExecuteSfdxCodeDiagnosticsToolViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send tool call request
        Map<String, Object> toolCallRequest = createMCPRequest("tools/call", Map.of(
            "name", "sfdx_code_diagnostics",
            "arguments", Map.of(
                "workspace", testWorkspacePath,
                "includeWarnings", false,
                "includeUnused", false
            )
        ));
        
        sendMCPRequest(toolCallRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "Tool call should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("content"), "Result should contain content");
        
        JsonNode content = result.get("content");
        assertTrue(content.isArray(), "Content should be an array");
        assertTrue(content.size() > 0, "Content should not be empty");
        
        String contentText = content.get(0).get("text").asText();
        assertNotNull(contentText);
        assertFalse(contentText.trim().isEmpty());
        assertTrue(contentText.contains("analysis") || contentText.contains("issue") || contentText.contains("passed"), 
                  "Response should indicate analysis was performed: " + contentText);
        
        logger.info("Successfully executed sfdx_code_diagnostics tool, response: {}", contentText);
    }
    
    @Test
    @DisplayName("Should execute apex_find_usages tool via MCP protocol")
    @Timeout(60)
    void shouldExecuteApexFindUsagesToolViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        String testFilePath = testWorkspacePath + "/force-app/main/default/classes/TestClass.cls";
        
        // Send tool call request
        Map<String, Object> toolCallRequest = createMCPRequest("tools/call", Map.of(
            "name", "apex_find_usages",
            "arguments", Map.of(
                "path", testFilePath,
                "line", 1,
                "offset", 15
            )
        ));
        
        sendMCPRequest(toolCallRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "Tool call should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("content"), "Result should contain content");
        
        JsonNode content = result.get("content");
        assertTrue(content.isArray(), "Content should be an array");
        assertTrue(content.size() > 0, "Content should not be empty");
        
        String contentText = content.get(0).get("text").asText();
        assertNotNull(contentText);
        assertFalse(contentText.trim().isEmpty());
        assertTrue(contentText.contains("reference") || contentText.contains("found") || contentText.contains("location"), 
                  "Response should indicate references were searched: " + contentText);
        
        logger.info("Successfully executed apex_find_usages tool");
    }
    
    @Test
    @DisplayName("Should execute apex_find_definition tool via MCP protocol")
    @Timeout(60)
    void shouldExecuteApexGotoDefinitionToolViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        String testFilePath = testWorkspacePath + "/force-app/main/default/classes/AnotherClass.cls";
        
        // Send tool call request
        Map<String, Object> toolCallRequest = createMCPRequest("tools/call", Map.of(
            "name", "apex_find_definition",
            "arguments", Map.of(
                "path", testFilePath,
                "line", 3,
                "offset", 10
            )
        ));
        
        sendMCPRequest(toolCallRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "Tool call should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("content"), "Result should contain content");
        
        JsonNode content = result.get("content");
        assertTrue(content.isArray(), "Content should be an array");
        assertTrue(content.size() > 0, "Content should not be empty");
        
        String contentText = content.get(0).get("text").asText();
        assertNotNull(contentText);
        assertFalse(contentText.trim().isEmpty());
        assertTrue(contentText.contains("definition") || contentText.contains("found") || contentText.contains("location"), 
                  "Response should indicate definition was searched: " + contentText);
        
        logger.info("Successfully executed apex_find_definition tool");
    }
    
    @Test
    @DisplayName("Should execute apex_find_impacted_tests tool via MCP protocol")
    @Timeout(30)
    void shouldExecuteApexFindImpactedTestsToolViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        String testFilePath1 = testWorkspacePath + "/force-app/main/default/classes/TestClass.cls";
        String testFilePath2 = testWorkspacePath + "/force-app/main/default/classes/AnotherClass.cls";
        
        // Send tool call request
        Map<String, Object> toolCallRequest = createMCPRequest("tools/call", Map.of(
            "name", "apex_find_impacted_tests",
            "arguments", Map.of(
                "changed_paths", java.util.List.of(testFilePath1, testFilePath2)
            )
        ));
        
        sendMCPRequest(toolCallRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "Tool call should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("content"), "Result should contain content");
        
        JsonNode content = result.get("content");
        assertTrue(content.isArray(), "Content should be an array");
        assertTrue(content.size() > 0, "Content should not be empty");
        
        String contentText = content.get(0).get("text").asText();
        assertNotNull(contentText);
        assertFalse(contentText.trim().isEmpty());
        
        // Validate JSON structure of response
        ObjectMapper mapper = new ObjectMapper();
        JsonNode responseJson = mapper.readTree(contentText);
        
        assertTrue(responseJson.has("tool"), "Response should contain tool field");
        assertEquals("apex_find_impacted_tests", responseJson.get("tool").asText());
        
        assertTrue(responseJson.has("status"), "Response should contain status field");
        assertEquals("completed", responseJson.get("status").asText());
        
        assertTrue(responseJson.has("impacted_test_files"), "Response should contain impacted_test_files");
        assertTrue(responseJson.get("impacted_test_files").isArray(), "impacted_test_files should be array");
        
        assertTrue(responseJson.has("counts"), "Response should contain counts");
        JsonNode counts = responseJson.get("counts");
        assertTrue(counts.has("total_impacted_tests"), "Counts should contain total_impacted_tests");
        assertTrue(counts.has("changed_files_analyzed"), "Counts should contain changed_files_analyzed");
        assertTrue(counts.has("workspaces_analyzed"), "Counts should contain workspaces_analyzed");
        
        logger.info("Successfully executed apex_find_impacted_tests tool");
    }
    
    @Test
    @DisplayName("Should handle tool call with invalid arguments")
    @Timeout(30)
    void shouldHandleToolCallWithInvalidArguments() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send tool call request with missing required arguments
        Map<String, Object> toolCallRequest = createMCPRequest("tools/call", Map.of(
            "name", "sfdx_code_diagnostics",
            "arguments", Map.of(
                // Missing workspace argument
                "includeWarnings", false
            )
        ));
        
        sendMCPRequest(toolCallRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        
        // Should either have error or result with error content
        assertTrue(response.has("error") || 
                  (response.has("result") && response.get("result").has("content")), 
                  "Should handle invalid arguments gracefully");
        
        logger.info("Successfully handled tool call with invalid arguments");
    }
    
    @Test
    @DisplayName("Should handle call to non-existent tool")
    @Timeout(30)
    void shouldHandleCallToNonExistentTool() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send tool call request for non-existent tool
        Map<String, Object> toolCallRequest = createMCPRequest("tools/call", Map.of(
            "name", "non_existent_tool",
            "arguments", Map.of()
        ));
        
        sendMCPRequest(toolCallRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        
        // Should return an error for non-existent tool
        assertTrue(response.has("error"), "Should return error for non-existent tool");
        
        JsonNode error = response.get("error");
        assertTrue(error.has("code"), "Error should have code");
        assertTrue(error.has("message"), "Error should have message");
        
        logger.info("Successfully handled call to non-existent tool");
    }
    
    @Test
    @DisplayName("Should read workspace resource via MCP protocol")
    @Timeout(30)
    void shouldReadWorkspaceResourceViaMCPProtocol() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // First, list resources to get a valid URI
        Map<String, Object> listResourcesRequest = createMCPRequest("resources/list", Map.of());
        sendMCPRequest(listResourcesRequest);
        
        JsonNode listResponse = readMCPResponse();
        assertNotNull(listResponse);
        assertTrue(listResponse.has("result"));
        
        JsonNode resources = listResponse.get("result").get("resources");
        assertTrue(resources.size() > 0, "Should have at least one resource");
        
        // Get the first resource URI
        String resourceUri = resources.get(0).get("uri").asText();
        assertNotNull(resourceUri);
        assertFalse(resourceUri.trim().isEmpty());
        
        // Now read the resource
        Map<String, Object> readResourceRequest = createMCPRequest("resources/read", Map.of(
            "uri", resourceUri
        ));
        
        sendMCPRequest(readResourceRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        assertTrue(response.has("result"), "Resource read should return result");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("contents"), "Result should contain contents");
        
        JsonNode contents = result.get("contents");
        assertTrue(contents.isArray(), "Contents should be an array");
        assertTrue(contents.size() > 0, "Contents should not be empty");
        
        logger.info("Successfully read resource: {}", resourceUri);
    }
    
    @Test
    @DisplayName("Should handle read of non-existent resource")
    @Timeout(30)
    void shouldHandleReadOfNonExistentResource() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Try to read a non-existent resource
        Map<String, Object> readResourceRequest = createMCPRequest("resources/read", Map.of(
            "uri", "workspace://non-existent-file.cls"
        ));
        
        sendMCPRequest(readResourceRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        
        // Should return an error for non-existent resource
        assertTrue(response.has("error"), "Should return error for non-existent resource");
        
        JsonNode error = response.get("error");
        assertTrue(error.has("code"), "Error should have code");
        assertTrue(error.has("message"), "Error should have message");
        
        logger.info("Successfully handled read of non-existent resource");
    }
    
    @Test
    @DisplayName("Should verify tool specifications have required fields")
    @Timeout(30)
    void shouldVerifyToolSpecificationsHaveRequiredFields() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send list_tools request
        Map<String, Object> listToolsRequest = createMCPRequest("tools/list", Map.of());
        sendMCPRequest(listToolsRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        JsonNode tools = response.get("result").get("tools");
        
        // Verify each tool has required fields
        for (JsonNode tool : tools) {
            assertTrue(tool.has("name"), "Tool should have name");
            assertTrue(tool.has("description"), "Tool should have description");
            assertTrue(tool.has("inputSchema"), "Tool should have inputSchema");
            
            String toolName = tool.get("name").asText();
            String description = tool.get("description").asText();
            
            assertFalse(toolName.trim().isEmpty(), "Tool name should not be empty");
            assertFalse(description.trim().isEmpty(), "Tool description should not be empty");
            
            // Verify inputSchema is a valid JSON schema object
            JsonNode inputSchema = tool.get("inputSchema");
            assertTrue(inputSchema.isObject(), "inputSchema should be an object");
            assertTrue(inputSchema.has("type"), "inputSchema should have type");
            assertEquals("object", inputSchema.get("type").asText(), "inputSchema type should be object");
            
            logger.info("Verified tool specification: {}", toolName);
        }
        
        logger.info("All tool specifications have required fields");
    }
    
    @Test
    @DisplayName("Should verify resource specifications have required fields")
    @Timeout(30)
    void shouldVerifyResourceSpecificationsHaveRequiredFields() throws Exception {
        startMCPServer();
        initializeMCPConnection();
        
        // Send list_resources request
        Map<String, Object> listResourcesRequest = createMCPRequest("resources/list", Map.of());
        sendMCPRequest(listResourcesRequest);
        
        // Read response
        JsonNode response = readMCPResponse();
        assertNotNull(response);
        JsonNode resources = response.get("result").get("resources");
        
        
        // Verify each resource has required fields
        for (JsonNode resource : resources) {
            assertTrue(resource.has("uri"), "Resource should have uri");
            
            String uri = resource.get("uri").asText();
            assertFalse(uri.trim().isEmpty(), "Resource URI should not be empty");
            
            // URI should be well-formed
            assertTrue(uri.contains("://"), "Resource URI should be well-formed with scheme");
            
            // Check for optional fields that might be present
            if (resource.has("name")) {
                String name = resource.get("name").asText();
                assertFalse(name.trim().isEmpty(), "Resource name should not be empty if present");
                logger.info("Verified resource: {} -> {}", name, uri);
            } else if (resource.has("description")) {
                String description = resource.get("description").asText();
                logger.info("Verified resource with description: {} -> {}", description, uri);
            } else {
                logger.info("Verified resource (URI only): {}", uri);
            }
        }
        
        logger.info("All resource specifications have required fields");
    }
    
    /**
     * Start the MCP server process.
     */
    private void startMCPServer() throws IOException {
        String javaCommand = System.getProperty("java.home") + "/bin/java";
        String classpath = System.getProperty("java.class.path");
        
        ProcessBuilder processBuilder = new ProcessBuilder(
            javaCommand,
            "-cp", classpath,
            "io.github.apexdevtools.apexls.mcp.MCPServer"
        );
        
        processBuilder.redirectErrorStream(false);
        mcpServerProcess = processBuilder.start();
        
        serverInput = new BufferedWriter(new OutputStreamWriter(mcpServerProcess.getOutputStream()));
        serverOutput = new BufferedReader(new InputStreamReader(mcpServerProcess.getInputStream()));
        
        // Give the server a moment to start
        try {
            Thread.sleep(1000);
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Interrupted while waiting for server to start", ex);
        }
        
        if (!mcpServerProcess.isAlive()) {
            throw new RuntimeException("MCP server process failed to start");
        }
    }
    
    /**
     * Initialize MCP connection by sending initialize request.
     */
    private void initializeMCPConnection() throws IOException {
        Map<String, Object> initRequest = createMCPRequest("initialize", Map.of(
            "protocolVersion", "2024-11-05",
            "clientInfo", Map.of(
                "name", "test-client",
                "version", "1.0.0"
            )
        ));
        
        sendMCPRequest(initRequest);
        
        // Read and verify initialize response
        JsonNode response = readMCPResponse();
        if (response == null || !response.has("result")) {
            throw new RuntimeException("Failed to initialize MCP connection");
        }
        
        // Send initialized notification
        Map<String, Object> initializedNotification = Map.of(
            "jsonrpc", "2.0",
            "method", "notifications/initialized",
            "params", Map.of()
        );
        
        sendMCPRequest(initializedNotification);
    }
    
    /**
     * Create an MCP request with the given method and params.
     */
    private Map<String, Object> createMCPRequest(String method, Map<String, Object> params) {
        Map<String, Object> request = new HashMap<>();
        request.put("jsonrpc", "2.0");
        request.put("id", UUID.randomUUID().toString());
        request.put("method", method);
        request.put("params", params);
        return request;
    }
    
    /**
     * Send an MCP request to the server.
     */
    private void sendMCPRequest(Map<String, Object> request) throws IOException {
        String jsonRequest = objectMapper.writeValueAsString(request);
        logger.debug("Sending MCP request: {}", jsonRequest);
        
        serverInput.write(jsonRequest);
        serverInput.newLine();
        serverInput.flush();
    }
    
    /**
     * Read an MCP response from the server.
     */
    private JsonNode readMCPResponse() throws IOException {
        String response = serverOutput.readLine();
        if (response == null) {
            return null;
        }
        
        logger.debug("Received MCP response: {}", response);
        return objectMapper.readTree(response);
    }
}