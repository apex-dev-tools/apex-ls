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

import com.nawforce.apexlink.mcp.ScalaBridge;

/**
 * Start apex-ls as an MCP (Model Context Protocol) server.
 * 
 * MCP messages are passed over stdin/stdout using the MCP protocol.
 * Provides tools for Apex code analysis, navigation, and diagnostics.
 */
public class MCPServer {
    
    private final ScalaBridge scalaBridge;
    
    public MCPServer() {
        this.scalaBridge = new ScalaBridge();
    }
    
    public void run() throws Exception {
        try {
            System.out.println("MCP Server starting...");
            
            // TODO: Implement MCP server initialization
            // For now, just create a basic placeholder
            
            // Keep server running until shutdown
            Thread.currentThread().join();
            
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(-1);
        } finally {
            scalaBridge.shutdown();
        }
    }
    
    public static void main(String[] args) {
        try {
            new MCPServer().run();
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(-1);
        }
    }
}