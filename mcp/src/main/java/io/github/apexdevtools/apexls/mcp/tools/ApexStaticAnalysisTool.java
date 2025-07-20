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
import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.spec.McpSchema.Tool;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * MCP tool for performing static analysis on Apex code to find issues.
 * Uses the bridge to communicate with the apex-ls core (Java 8) from this Java 17 MCP server.
 */
public class ApexStaticAnalysisTool {
    
    private final ApexLsBridge bridge;
    
    public ApexStaticAnalysisTool(ApexLsBridge bridge) {
        this.bridge = bridge;
    }
    
    public McpServerFeatures.SyncToolSpecification getSpecification() {
        String schema = "{\n" +
            "  \"type\": \"object\",\n" +
            "  \"properties\": {\n" +
            "    \"workspace\": {\n" +
            "      \"type\": \"string\",\n" +
            "      \"description\": \"Path to the Apex workspace directory\"\n" +
            "    },\n" +
            "    \"includeWarnings\": {\n" +
            "      \"type\": \"boolean\",\n" +
            "      \"description\": \"Include warning-level issues in results\",\n" +
            "      \"default\": false\n" +
            "    },\n" +
            "    \"includeUnused\": {\n" +
            "      \"type\": \"boolean\",\n" +
            "      \"description\": \"Include unused code analysis in results\",\n" +
            "      \"default\": false\n" +
            "    }\n" +
            "  },\n" +
            "  \"required\": [\"workspace\"]\n" +
            "}";
        
        Tool tool = new Tool(
            "apex_static_analysis",
            "Perform static analysis on Apex code to find errors, warnings, and unused code",
            schema
        );
        
        return new McpServerFeatures.SyncToolSpecification(tool, this::execute);
    }
    
    private CallToolResult execute(Object exchange, Map<String, Object> arguments) {
        try {
            String workspace = (String) arguments.get("workspace");
            boolean includeWarnings = arguments.get("includeWarnings") != null ? 
                (Boolean) arguments.get("includeWarnings") : false;
            boolean includeUnused = arguments.get("includeUnused") != null ? 
                (Boolean) arguments.get("includeUnused") : false;
            
            // Execute static analysis via bridge
            CompletableFuture<String> future = 
                bridge.getIssues(workspace, includeWarnings, includeUnused);
            String issuesJson = future.join();
            
            // The bridge returns JSON, so we can either parse it and reformat,
            // or return it directly. For now, let's return it directly.
            if (issuesJson != null && !issuesJson.trim().isEmpty()) {
                return new CallToolResult(issuesJson, false);
            } else {
                return new CallToolResult("No issues found - code analysis passed successfully", false);
            }
            
        } catch (Exception ex) {
            return new CallToolResult("Error during static analysis: " + ex.getMessage(), true);
        }
    }
}