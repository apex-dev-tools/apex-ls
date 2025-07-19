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

import io.github.apexdevtools.apexls.mcp.ScalaBridge;
import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.spec.McpSchema.Tool;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.github.apexdevtools.api.Issue;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * MCP tool for performing static analysis on Apex code to find issues.
 * Maps to the existing check API operation.
 */
public class ApexStaticAnalysisTool {
    
    private final ScalaBridge scalaBridge;
    
    public ApexStaticAnalysisTool(ScalaBridge scalaBridge) {
        this.scalaBridge = scalaBridge;
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
            
            // Execute static analysis
            CompletableFuture<Issue[]> future = 
                scalaBridge.checkWorkspace(workspace, includeWarnings, includeUnused);
            Issue[] issues = future.join();
            
            // Format the results for MCP
            if (issues.length > 0) {
                StringBuilder content = new StringBuilder();
                content.append("Found ").append(issues.length).append(" issue(s):\n\n");
                
                for (int i = 0; i < issues.length; i++) {
                    Issue issue = issues[i];
                    content.append(String.format("%d. [%s] %s\n",
                        i + 1,
                        issue.rule().name(),
                        issue.message()
                    ));
                    
                    if (issue.fileLocation() != null) {
                        content.append(String.format("   File: %s\n",
                            issue.filePath()
                        ));
                        content.append(String.format("   Line: %d:%d-%d:%d\n",
                            issue.fileLocation().startLineNumber(),
                            issue.fileLocation().startCharOffset(),
                            issue.fileLocation().endLineNumber(),
                            issue.fileLocation().endCharOffset()
                        ));
                    }
                    content.append("\n");
                }
                
                return new CallToolResult(content.toString(), false);
            } else {
                return new CallToolResult("No issues found - code analysis passed successfully", false);
            }
            
        } catch (Exception ex) {
            return new CallToolResult("Error during static analysis: " + ex.getMessage(), true);
        }
    }
}