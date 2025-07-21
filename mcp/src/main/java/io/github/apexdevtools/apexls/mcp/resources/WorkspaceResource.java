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

package io.github.apexdevtools.apexls.mcp.resources;

import io.github.apexdevtools.apexls.mcp.bridge.ApexLsBridge;
import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceRequest;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceResult;
import io.modelcontextprotocol.spec.McpSchema.Resource;
import io.modelcontextprotocol.spec.McpSchema.TextResourceContents;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * MCP resource for accessing Apex workspace information and metadata. Uses the bridge to
 * communicate with the apex-ls core (Java 8) from this Java 17 MCP server.
 */
public class WorkspaceResource {

  private final ApexLsBridge bridge;

  public WorkspaceResource(ApexLsBridge bridge) {
    this.bridge = bridge;
  }

  public McpServerFeatures.SyncResourceSpecification getSpecification() {
    Resource resource =
        new Resource(
            "workspace://apex/{workspace_path}",
            null, // name
            "Access Apex workspace information and file contents",
            "application/json",
            null // annotations
            );

    return new McpServerFeatures.SyncResourceSpecification(resource, this::read);
  }

  private ReadResourceResult read(McpSyncServerExchange exchange, ReadResourceRequest request) {
    try {
      String uri = request.uri();

      // Parse workspace path from URI
      // Expected format: workspace://apex/{workspace_path}
      if (!uri.startsWith("workspace://apex/")) {
        TextResourceContents errorContents =
            new TextResourceContents(
                uri,
                "text/plain",
                "Error: Invalid workspace URI format. Expected: workspace://apex/{workspace_path}");
        return new ReadResourceResult(List.of(errorContents));
      }

      String workspacePath = uri.substring("workspace://apex/".length());

      // Get workspace information via bridge
      CompletableFuture<String> future = bridge.getWorkspaceInfo(workspacePath);
      String workspaceInfo = future.join();

      TextResourceContents contents =
          new TextResourceContents(uri, "application/json", workspaceInfo);
      return new ReadResourceResult(List.of(contents));

    } catch (Exception ex) {
      String uri = request.uri();
      TextResourceContents errorContents =
          new TextResourceContents(
              uri, "text/plain", "Error reading workspace resource: " + ex.getMessage());
      return new ReadResourceResult(List.of(errorContents));
    }
  }
}
