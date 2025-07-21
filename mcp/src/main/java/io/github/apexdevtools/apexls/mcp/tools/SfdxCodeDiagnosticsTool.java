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
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.Tool;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * MCP tool for analyzing SFDX projects to detect code issues across all Salesforce development
 * artifacts. Uses the bridge to communicate with the apex-ls core (Java 8) from this Java 17 MCP
 * server.
 */
public class SfdxCodeDiagnosticsTool {

  private final ApexLsBridge bridge;

  public SfdxCodeDiagnosticsTool(ApexLsBridge bridge) {
    this.bridge = bridge;
  }

  public McpServerFeatures.SyncToolSpecification getSpecification() {
    String schema =
        "{\n"
            + "  \"type\": \"object\",\n"
            + "  \"properties\": {\n"
            + "    \"workspace\": {\n"
            + "      \"type\": \"string\",\n"
            + "      \"description\": \"Path to the SFDX workspace directory\"\n"
            + "    },\n"
            + "    \"includeWarnings\": {\n"
            + "      \"type\": \"boolean\",\n"
            + "      \"description\": \"Include warning-level issues in results\",\n"
            + "      \"default\": false\n"
            + "    },\n"
            + "    \"maxIssuesPerFile\": {\n"
            + "      \"type\": \"integer\",\n"
            + "      \"description\": \"Maximum number of issues to return per file\",\n"
            + "      \"default\": 100,\n"
            + "      \"minimum\": 1\n"
            + "    }\n"
            + "  },\n"
            + "  \"required\": [\"workspace\"]\n"
            + "}";

    Tool tool =
        new Tool(
            "sfdx_code_diagnostics",
            "Analyzes SFDX projects for code issues, errors, and warnings. Detects problems across all Salesforce development artifacts including Apex classes, triggers, Lightning Web Components, Aura components, and metadata files. Use this tool when you need to validate code quality, find compilation errors, identify warnings, or perform comprehensive project health checks.",
            schema);

    return new McpServerFeatures.SyncToolSpecification(tool, this::execute);
  }

  private CallToolResult execute(Object exchange, Map<String, Object> arguments) {
    try {
      String workspace = (String) arguments.get("workspace");
      boolean includeWarnings =
          arguments.get("includeWarnings") != null
              ? (Boolean) arguments.get("includeWarnings")
              : false;
      int maxIssuesPerFile =
          arguments.get("maxIssuesPerFile") != null
              ? ((Number) arguments.get("maxIssuesPerFile")).intValue()
              : 100;

      // Validate workspace argument
      CallToolResult validationResult = WorkspaceValidator.validateWorkspace(workspace);
      if (validationResult != null) {
        return validationResult;
      }

      // Execute code diagnostics via bridge
      CompletableFuture<String> future =
          bridge.getIssues(workspace, includeWarnings, maxIssuesPerFile);
      String issuesJson = future.join();

      // The bridge returns JSON, so we can either parse it and reformat,
      // or return it directly. For now, let's return it directly.
      if (issuesJson != null && !issuesJson.trim().isEmpty()) {
        return new CallToolResult(issuesJson, false);
      } else {
        return new CallToolResult("No issues found - code analysis passed successfully", false);
      }

    } catch (Exception ex) {
      return new CallToolResult("Error during code diagnostics: " + ex.getMessage(), true);
    }
  }
}