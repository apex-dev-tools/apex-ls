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

import java.io.File;

/**
 * Helper class for validating workspace arguments in MCP tools.
 * Ensures consistent validation across all Apex tools.
 */
public class WorkspaceValidator {
    
    /**
     * Validates that a workspace argument is valid for Apex analysis.
     * 
     * @param workspace the workspace path to validate
     * @return null if validation passes, or a CallToolResult with error details if validation fails
     */
    public static CallToolResult validateWorkspace(String workspace) {
        // Check if workspace argument is provided
        if (workspace == null || workspace.trim().isEmpty()) {
            return new CallToolResult("Error: workspace argument is required", true);
        }
        
        // Check if workspace path exists and is a directory
        File workspaceDir = new File(workspace);
        if (!workspaceDir.exists()) {
            return new CallToolResult("Error: workspace directory does not exist: " + workspace, true);
        }
        
        if (!workspaceDir.isDirectory()) {
            return new CallToolResult("Error: workspace path is not a directory: " + workspace, true);
        }
        
        // Check if workspace contains sfdx-project.json
        File sfdxProject = new File(workspaceDir, "sfdx-project.json");
        if (!sfdxProject.exists()) {
            return new CallToolResult(
                "Error: workspace directory must contain sfdx-project.json file. " +
                "Found: " + workspace + " but missing sfdx-project.json", true);
        }
        
        return null; // Validation passed
    }
}