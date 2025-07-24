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
 * MCP tool for finding all references to an Apex identifier. Uses the bridge to communicate with
 * the apex-ls core (Java 8) from this Java 17 MCP server.
 */
public class ApexFindUsagesTool {

  private final ApexLsBridge bridge;

  public ApexFindUsagesTool(ApexLsBridge bridge) {
    this.bridge = bridge;
  }

  public McpServerFeatures.SyncToolSpecification getSpecification() {
    String schema =
        "{\n"
            + "  \"type\": \"object\",\n"
            + "  \"properties\": {\n"
            + "    \"path\": {\n"
            + "      \"type\": \"string\",\n"
            + "      \"description\": \"Path to the Apex file\"\n"
            + "    },\n"
            + "    \"line\": {\n"
            + "      \"type\": \"integer\",\n"
            + "      \"description\": \"Line number (1-based)\"\n"
            + "    },\n"
            + "    \"offset\": {\n"
            + "      \"type\": \"integer\",\n"
            + "      \"description\": \"Character offset within the line\"\n"
            + "    }\n"
            + "  },\n"
            + "  \"required\": [\"path\", \"line\", \"offset\"]\n"
            + "}";

    Tool tool =
        new Tool(
            "apex_find_usages",
            "Locate all references to any Apex identifier across the workspace - including classes, interfaces, enums, triggers, methods, constructors, fields, properties, variables, parameters, custom objects, custom fields, SObject types, system types, nested/inner classes, method overrides, interface implementations, inheritance relationships, type declarations, method calls, field access, annotations, and Salesforce platform metadata",
            schema);

    return new McpServerFeatures.SyncToolSpecification(tool, this::execute);
  }

  private CallToolResult execute(Object exchange, Map<String, Object> arguments) {
    try {
      // Validate arguments
      CallToolResult validationResult = ArgumentValidator.validateApexToolArguments(arguments);
      if (validationResult != null) {
        return validationResult;
      }

      // Extract validated arguments
      ArgumentValidator.ValidatedArguments args =
          ArgumentValidator.extractValidatedArguments(arguments);

      // Discover workspace from path
      String workspace = WorkspaceDiscovery.findWorkspace(args.path);
      if (workspace == null) {
        return new CallToolResult(
            "Error: Could not find workspace directory containing sfdx-project.json for path: "
                + args.path,
            true);
      }

      // Execute the references lookup via bridge
      CompletableFuture<String> future =
          bridge.findUsages(workspace, args.path, args.line, args.offset);
      String referencesJson = future.join();

      // The bridge returns JSON-formatted results
      if (referencesJson != null && !referencesJson.trim().isEmpty()) {
        return new CallToolResult(referencesJson, false);
      } else {
        return new CallToolResult("No references found", false);
      }

    } catch (Exception ex) {
      return new CallToolResult("Error finding references: " + ex.getMessage(), true);
    }
  }
}
