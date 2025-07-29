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

package io.github.apexdevtools.apexls.mcp;

import io.github.apexdevtools.apexls.mcp.bridge.ApexLsBridge;
import io.github.apexdevtools.apexls.mcp.bridge.EmbeddedApexLsBridge;
import io.github.apexdevtools.apexls.mcp.resources.WorkspaceResource;
import io.github.apexdevtools.apexls.mcp.tools.ApexFindDefinitionTool;
import io.github.apexdevtools.apexls.mcp.tools.ApexFindImpactedTestsTool;
import io.github.apexdevtools.apexls.mcp.tools.ApexFindUsagesTool;
import io.github.apexdevtools.apexls.mcp.tools.SfdxCodeDiagnosticsTool;
import io.modelcontextprotocol.server.McpServer;
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Main entry point for the Apex Language Server MCP (Model Context Protocol) server.
 *
 * <p>This server runs on Java 17 and uses the official MCP Java SDK to provide MCP protocol
 * support. It communicates with the main apex-ls core (Java 8) through a bridge interface.
 *
 * <p>Usage: java -jar apex-ls-mcp.jar
 *
 * <p>The server communicates via stdin/stdout using the MCP protocol.
 */
public class MCPServer {

  private static final Logger logger = LoggerFactory.getLogger(MCPServer.class);

  private final MCPServerConfig config;
  private ApexLsBridge bridge;
  private SfdxCodeDiagnosticsTool sfdxDiagnosticsTool;
  private ApexFindUsagesTool findUsagesTool;
  private ApexFindDefinitionTool findDefinitionTool;
  private ApexFindImpactedTestsTool findImpactedTestsTool;
  private WorkspaceResource workspaceResource;

  public MCPServer(MCPServerConfig config) {
    this.config = config;
    logger.trace("MCP Server created with config: {}", config);
  }

  /**
   * Initialize and run the MCP server.
   *
   * @throws Exception if server startup fails
   */
  public void run() throws Exception {
    try {
      logger.info("Starting Apex Language Server MCP Server");
      logger.info("Java Version: {}", System.getProperty("java.version"));
      logger.info("Protocol: Model Context Protocol (MCP)");
      logger.info("Transport: STDIO");

      // Initialize bridge and tools
      initializeBridge();
      initializeMCPServer();

      logger.info("MCP Server ready. Waiting for client connections...");

      // Keep server running until shutdown
      Thread.currentThread().join();

    } catch (InterruptedException ex) {
      logger.info("MCP Server interrupted, shutting down...");
      Thread.currentThread().interrupt();
    } catch (Exception ex) {
      logger.error("MCP Server error: {}", ex.getMessage(), ex);
      throw ex;
    } finally {
      if (bridge != null) {
        try {
          bridge.close();
        } catch (Exception ex) {
          logger.warn("Error closing bridge: {}", ex.getMessage(), ex);
        }
      }
    }
  }

  /** Initialize the bridge to apex-ls core. */
  private void initializeBridge() throws Exception {
    logger.info("Initializing bridge to apex-ls core...");
    logger.trace("MCP Server applying config: logging={}, cache={}", 
        config.getLoggingLevel(), config.isCacheEnabled());
    bridge = new EmbeddedApexLsBridge(config);
    bridge.initialize();
    logger.info("Bridge initialized successfully");
  }

  /** Initialize MCP server with tools and resources. */
  private void initializeMCPServer() throws Exception {
    logger.info("Initializing MCP tools and resources...");

    // Create tools with bridge
    sfdxDiagnosticsTool = new SfdxCodeDiagnosticsTool(bridge);
    findUsagesTool = new ApexFindUsagesTool(bridge);
    findDefinitionTool = new ApexFindDefinitionTool(bridge);
    findImpactedTestsTool = new ApexFindImpactedTestsTool(bridge);
    workspaceResource = new WorkspaceResource(bridge);

    logger.info("  - SFDX Code Diagnostics Tool");
    logger.info("  - Apex Find Usages Tool");
    logger.info("  - Apex Find Definition Tool");
    logger.info("  - Apex Find Impacted Tests Tool");
    logger.info("  - Workspace Resource");

    // Create transport provider
    var transportProvider = new StdioServerTransportProvider();

    // Build MCP server with all tools and resources
    var serverSpec =
        McpServer.sync(transportProvider)
            .serverInfo("apex-language-server", "1.0.0")
            .instructions("Apex Language Server with MCP support for code analysis and navigation")
            .tools(
                sfdxDiagnosticsTool.getSpecification(),
                findUsagesTool.getSpecification(),
                findDefinitionTool.getSpecification(),
                findImpactedTestsTool.getSpecification())
            .resources(workspaceResource.getSpecification());

    // Build the server - transport provider handles lifecycle
    var server = serverSpec.build();
    logger.info("MCP Server started with STDIO transport");
  }

  /**
   * Main entry point for the MCP server.
   *
   * @param args command line arguments
   */
  public static void main(String[] args) {
    try {
      // Parse configuration from command line arguments and environment variables
      MCPServerConfig config = MCPServerConfig.fromArgs(args);
      logger.info("MCP Server starting with configuration: {}", config);
      
      new MCPServer(config).run();
    } catch (IllegalArgumentException ex) {
      if ("HELP_REQUESTED".equals(ex.getMessage())) {
        System.out.println(MCPServerConfig.getUsage());
        System.exit(0);
      } else {
        logger.error("Invalid configuration: {}", ex.getMessage());
        System.exit(1);
      }
    } catch (Exception ex) {
      logger.error("Failed to start MCP Server: {}", ex.getMessage(), ex);
      System.exit(1);
    }
  }
}
