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

package io.github.apexdevtools.apexls;

import io.github.apexdevtools.apexls.mcp.ScalaBridge;
import io.github.apexdevtools.apexls.mcp.tools.ApexGotoDefinitionTool;
import io.github.apexdevtools.apexls.mcp.tools.ApexFindReferencesTool;
import io.github.apexdevtools.apexls.mcp.tools.ApexStaticAnalysisTool;
import io.github.apexdevtools.apexls.mcp.resources.WorkspaceResource;
import io.modelcontextprotocol.server.McpServer;
import io.modelcontextprotocol.server.McpSyncServer;
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider;

/**
 * Start apex-ls as an MCP (Model Context Protocol) server.
 * 
 * MCP messages are passed over stdin/stdout using the MCP protocol.
 * Provides tools for Apex code analysis, navigation, and diagnostics.
 */
public class MCPServer {
    
    private final ScalaBridge scalaBridge;
    private final ApexGotoDefinitionTool gotoDefinitionTool;
    private final ApexFindReferencesTool findReferencesTool;
    private final ApexStaticAnalysisTool staticAnalysisTool;
    private final WorkspaceResource workspaceResource;
    
    public MCPServer() {
        this.scalaBridge = new ScalaBridge();
        this.gotoDefinitionTool = new ApexGotoDefinitionTool(scalaBridge);
        this.findReferencesTool = new ApexFindReferencesTool(scalaBridge);
        this.staticAnalysisTool = new ApexStaticAnalysisTool(scalaBridge);
        this.workspaceResource = new WorkspaceResource(scalaBridge);
    }
    
    public void run() throws Exception {
        try {
            System.out.println("Apex Language Server MCP starting...");
            System.out.println("Protocol: Model Context Protocol (MCP)");
            System.out.println("Transport: STDIO");
            
            // Initialize MCP server components
            initializeMCPServer();
            
            System.out.println("MCP Server ready. Waiting for client connections...");
            
            // Keep server running until shutdown
            Thread.currentThread().join();
            
        } catch (InterruptedException ex) {
            System.out.println("MCP Server interrupted, shutting down...");
            Thread.currentThread().interrupt();
        } catch (Exception ex) {
            System.err.println("MCP Server error: " + ex.getMessage());
            ex.printStackTrace();
            System.exit(-1);
        } finally {
            scalaBridge.shutdown();
        }
    }
    
    private void initializeMCPServer() throws Exception {
        System.out.println("Initializing MCP tools:");
        System.out.println("  - Apex Goto Definition Tool");
        System.out.println("  - Apex Find References Tool");
        System.out.println("  - Apex Static Analysis Tool");
        System.out.println("Initializing MCP resources:");
        System.out.println("  - Workspace Resource");
        
        // Create transport provider
        var transportProvider = new StdioServerTransportProvider();
        
        // Build MCP server with all tools and resources
        var serverSpec = McpServer.sync(transportProvider)
            .serverInfo("apex-language-server", "1.0.0")
            .instructions("Apex Language Server with MCP support for code analysis and navigation")
            .tools(
                gotoDefinitionTool.getSpecification(),
                findReferencesTool.getSpecification(),
                staticAnalysisTool.getSpecification()
            )
            .resources(
                workspaceResource.getSpecification()
            );
        
        // Build the server - transport provider handles lifecycle
        var server = serverSpec.build();
        System.out.println("MCP Server started with STDIO transport");
    }
    
    public static void main(String[] args) {
        try {
            new MCPServer().run();
        } catch (Exception ex) {
            System.err.println("Failed to start MCP Server: " + ex.getMessage());
            ex.printStackTrace();
            System.exit(-1);
        }
    }
}