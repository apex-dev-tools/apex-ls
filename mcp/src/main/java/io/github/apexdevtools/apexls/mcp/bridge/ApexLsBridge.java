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

package io.github.apexdevtools.apexls.mcp.bridge;

import java.util.concurrent.CompletableFuture;

/**
 * Bridge interface for communicating between the Java 17 MCP server
 * and the Java 8 apex-ls core functionality.
 * 
 * This abstraction allows for different bridge implementations:
 * - Embedded classpath-based bridge (same JVM)
 * - Process-based bridge (separate JVM)
 * - Future: Network-based bridge
 */
public interface ApexLsBridge extends AutoCloseable {
    
    /**
     * Initialize the bridge and establish connection to apex-ls core.
     * 
     * @throws Exception if bridge initialization fails
     */
    void initialize() throws Exception;
    
    /**
     * Check if the bridge is ready for operations.
     * 
     * @return true if bridge is connected and ready
     */
    boolean isReady();
    
    /**
     * Get static analysis issues for a workspace.
     * 
     * @param workspaceDirectory path to workspace
     * @param includeWarnings include warning-level issues
     * @param includeUnused include unused code analysis
     * @return future containing issues in JSON format
     */
    CompletableFuture<String> getIssues(String workspaceDirectory, boolean includeWarnings, boolean includeUnused);
    
    /**
     * Find references to an identifier at a specific position.
     * 
     * @param workspaceDirectory path to workspace
     * @param filePath path to file
     * @param line line number (0-based)
     * @param offset character offset within line
     * @return future containing references in JSON format
     */
    CompletableFuture<String> findReferences(String workspaceDirectory, String filePath, int line, int offset);
    
    /**
     * Get definition location for an identifier at a specific position.
     * 
     * @param workspaceDirectory path to workspace
     * @param filePath path to file
     * @param line line number (0-based)
     * @param offset character offset within line
     * @return future containing definition location in JSON format
     */
    CompletableFuture<String> getDefinition(String workspaceDirectory, String filePath, int line, int offset);
    
    /**
     * Get workspace information and metadata.
     * 
     * @param workspaceDirectory path to workspace
     * @return future containing workspace info in JSON format
     */
    CompletableFuture<String> getWorkspaceInfo(String workspaceDirectory);
    
    /**
     * Get version information from apex-ls core.
     * 
     * @return future containing version string
     */
    CompletableFuture<String> getVersion();
    
    /**
     * Shutdown the bridge and release resources.
     */
    @Override
    void close() throws Exception;
}