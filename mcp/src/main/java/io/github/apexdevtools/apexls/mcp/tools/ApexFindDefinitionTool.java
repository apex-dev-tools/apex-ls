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
 * MCP tool for finding the definition of an Apex identifier. Uses the bridge to communicate with
 * the apex-ls core (Java 8) from this Java 17 MCP server.
 */
public class ApexFindDefinitionTool {

  private final ApexLsBridge bridge;

  public ApexFindDefinitionTool(ApexLsBridge bridge) {
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
            "apex_find_definition",
            "Find the definition location for Apex types (classes, interfaces, enums, triggers), members (methods, constructors, fields, properties), variables (local, parameters), and platform objects (standard/custom SObjects and fields)",
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

      // Execute the definition lookup via bridge
      CompletableFuture<String> future =
          bridge.getDefinition(workspace, args.path, args.line, args.offset);
      String definitionJson = future.join();

      // The bridge returns JSON-formatted results
      if (definitionJson != null && !definitionJson.trim().isEmpty()) {
        return new CallToolResult(definitionJson, false);
      } else {
        return new CallToolResult("Definition not found", false);
      }

    } catch (Exception ex) {
      return new CallToolResult("Error finding definition: " + ex.getMessage(), true);
    }
  }
}
