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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.github.apexdevtools.apexls.mcp.bridge.ApexLsBridge;
import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.Tool;
import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * MCP tool for finding test classes that should be run based on changes to specific Apex source
 * files. Handles multiple workspaces by auto-discovering workspaces from file paths and merging
 * results.
 */
public class ApexFindImpactedTestsTool {

  private final ApexLsBridge bridge;
  private static final ObjectMapper objectMapper = new ObjectMapper();

  public ApexFindImpactedTestsTool(ApexLsBridge bridge) {
    this.bridge = bridge;
  }

  public McpServerFeatures.SyncToolSpecification getSpecification() {
    String schema =
        "{\n"
            + "  \"type\": \"object\",\n"
            + "  \"properties\": {\n"
            + "    \"changed_paths\": {\n"
            + "      \"type\": \"array\",\n"
            + "      \"items\": {\n"
            + "        \"type\": \"string\"\n"
            + "      },\n"
            + "      \"description\": \"Array of file paths that have been changed\"\n"
            + "    }\n"
            + "  },\n"
            + "  \"required\": [\"changed_paths\"]\n"
            + "}";

    Tool tool =
        new Tool(
            "apex_find_impacted_tests",
            "Find test classes that should be run based on changes to specific Apex source files",
            schema);

    return new McpServerFeatures.SyncToolSpecification(tool, this::execute);
  }

  private CallToolResult execute(Object exchange, Map<String, Object> arguments) {
    try {
      // Extract changed_paths argument
      Object changedPathsObj = arguments.get("changed_paths");
      if (changedPathsObj == null) {
        return new CallToolResult("Error: changed_paths argument is required", true);
      }

      if (!(changedPathsObj instanceof List)) {
        return new CallToolResult("Error: changed_paths must be an array", true);
      }

      @SuppressWarnings("unchecked")
      List<Object> changedPathsList = (List<Object>) changedPathsObj;

      if (changedPathsList.isEmpty()) {
        return new CallToolResult("Error: changed_paths cannot be empty", true);
      }

      // Convert to String array and validate
      List<String> changedPaths = new ArrayList<>();
      for (Object pathObj : changedPathsList) {
        if (!(pathObj instanceof String)) {
          return new CallToolResult("Error: All paths in changed_paths must be strings", true);
        }
        changedPaths.add((String) pathObj);
      }

      // Group files by workspace
      Map<String, List<String>> workspaceToFiles = new HashMap<>();
      List<String> filesWithoutWorkspace = new ArrayList<>();

      for (String filePath : changedPaths) {
        String workspace = WorkspaceDiscovery.findWorkspace(filePath);
        if (workspace != null) {
          workspaceToFiles.computeIfAbsent(workspace, k -> new ArrayList<>()).add(filePath);
        } else {
          filesWithoutWorkspace.add(filePath);
        }
      }

      if (workspaceToFiles.isEmpty()) {
        return new CallToolResult(
            "Error: No valid workspaces found for any of the provided file paths", true);
      }

      // Process each workspace and collect results
      List<String> allImpactedTests = new ArrayList<>();
      int totalWorkspaces = workspaceToFiles.size();
      int totalFilesAnalyzed = changedPaths.size() - filesWithoutWorkspace.size();

      for (Map.Entry<String, List<String>> entry : workspaceToFiles.entrySet()) {
        String workspace = entry.getKey();
        List<String> workspaceFiles = entry.getValue();

        String[] fileArray = workspaceFiles.toArray(new String[0]);
        CompletableFuture<String> future = bridge.getTestClassItemsChanged(workspace, fileArray);
        String result = future.join();

        // Extract impacted test files from bridge result
        // The bridge returns JSON, so we need to parse it and extract the file paths
        List<String> workspaceImpactedTests = extractTestFilesFromBridgeResult(result);
        allImpactedTests.addAll(workspaceImpactedTests);
      }

      // Create summary message
      StringBuilder summaryBuilder = new StringBuilder();
      summaryBuilder
          .append("Found ")
          .append(allImpactedTests.size())
          .append(" impacted test classes");

      if (totalWorkspaces > 1) {
        summaryBuilder.append(" across ").append(totalWorkspaces).append(" workspaces");
      }

      summaryBuilder.append(" for ").append(totalFilesAnalyzed).append(" changed files");

      if (!filesWithoutWorkspace.isEmpty()) {
        summaryBuilder
            .append(" (")
            .append(filesWithoutWorkspace.size())
            .append(" files skipped - no workspace found)");
      }

      // Build response JSON
      String responseJson =
          buildResponseJson(
              allImpactedTests, totalFilesAnalyzed, totalWorkspaces, summaryBuilder.toString());

      return new CallToolResult(responseJson, false);

    } catch (Exception ex) {
      return new CallToolResult("Error finding impacted tests: " + ex.getMessage(), true);
    }
  }

  private List<String> extractTestFilesFromBridgeResult(String jsonResult) {
    List<String> testFiles = new ArrayList<>();

    if (jsonResult == null || jsonResult.trim().isEmpty() || jsonResult.equals("null")) {
      return testFiles;
    }

    try {
      JsonNode rootNode = objectMapper.readTree(jsonResult);

      // Check if the response indicates an error
      JsonNode statusNode = rootNode.get("status");
      if (statusNode != null && "error".equals(statusNode.asText())) {
        // Log the error but return empty list rather than throwing
        return testFiles;
      }

      // Extract the impacted_test_files array
      JsonNode testFilesNode = rootNode.get("impacted_test_files");
      if (testFilesNode != null && testFilesNode.isArray()) {
        for (JsonNode fileNode : testFilesNode) {
          String filePath = fileNode.asText();
          if (filePath != null && !filePath.trim().isEmpty()) {
            testFiles.add(filePath);
          }
        }
      }

    } catch (Exception ex) {
      // Log error but don't fail the entire operation
      // Return empty list for this workspace
    }

    return testFiles;
  }

  private String buildResponseJson(
      List<String> impactedTests, int filesAnalyzed, int workspaces, String summary) {
    StringBuilder json = new StringBuilder();
    json.append("{\n");
    json.append("  \"tool\": \"apex_find_impacted_tests\",\n");
    json.append("  \"status\": \"completed\",\n");
    json.append("  \"summary\": \"").append(summary).append("\",\n");
    json.append("  \"counts\": {\n");
    json.append("    \"total_impacted_tests\": ").append(impactedTests.size()).append(",\n");
    json.append("    \"changed_files_analyzed\": ").append(filesAnalyzed).append(",\n");
    json.append("    \"workspaces_analyzed\": ").append(workspaces).append("\n");
    json.append("  },\n");
    json.append("  \"impacted_test_files\": [\n");

    for (int i = 0; i < impactedTests.size(); i++) {
      json.append("    \"").append(impactedTests.get(i)).append("\"");
      if (i < impactedTests.size() - 1) {
        json.append(",");
      }
      json.append("\n");
    }

    json.append("  ]\n");
    json.append("}");

    return json.toString();
  }
}
