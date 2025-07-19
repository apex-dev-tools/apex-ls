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
import com.nawforce.apexlink.rpc.TargetLocation;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * MCP tool for finding all references to an Apex identifier.
 * Maps to the existing getReferences API operation.
 */
public class ApexFindReferencesTool {
    
    private final ScalaBridge scalaBridge;
    
    public ApexFindReferencesTool(ScalaBridge scalaBridge) {
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
            "    \"path\": {\n" +
            "      \"type\": \"string\",\n" +
            "      \"description\": \"Path to the Apex file\"\n" +
            "    },\n" +
            "    \"line\": {\n" +
            "      \"type\": \"integer\",\n" +
            "      \"description\": \"Line number (0-based)\"\n" +
            "    },\n" +
            "    \"offset\": {\n" +
            "      \"type\": \"integer\",\n" +
            "      \"description\": \"Character offset within the line\"\n" +
            "    }\n" +
            "  },\n" +
            "  \"required\": [\"workspace\", \"path\", \"line\", \"offset\"]\n" +
            "}";
        
        Tool tool = new Tool(
            "apex_find_references",
            "Find all references to an Apex identifier at a specific position",
            schema
        );
        
        return new McpServerFeatures.SyncToolSpecification(tool, this::execute);
    }
    
    private CallToolResult execute(Object exchange, Map<String, Object> arguments) {
        try {
            String workspace = (String) arguments.get("workspace");
            String path = (String) arguments.get("path");
            int line = ((Number) arguments.get("line")).intValue();
            int offset = ((Number) arguments.get("offset")).intValue();
            
            // Execute the references lookup
            CompletableFuture<TargetLocation[]> future = 
                scalaBridge.getReferences(workspace, path, line, offset);
            TargetLocation[] references = future.join();
            
            // Format the result for MCP
            if (references.length > 0) {
                StringBuilder content = new StringBuilder();
                content.append("Found ").append(references.length).append(" reference(s):\n");
                
                for (int i = 0; i < references.length; i++) {
                    TargetLocation ref = references[i];
                    content.append(String.format("%d. %s (Line %d:%d-%d:%d)\n",
                        i + 1,
                        ref.targetPath(),
                        ref.range().startLine(),
                        ref.range().startPosition(),
                        ref.range().endLine(),
                        ref.range().endPosition()
                    ));
                }
                
                return new CallToolResult(content.toString(), false);
            } else {
                return new CallToolResult("No references found", false);
            }
            
        } catch (Exception ex) {
            return new CallToolResult("Error finding references: " + ex.getMessage(), true);
        }
    }
}