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

package com.nawforce.apexlink.mcp;

import com.nawforce.apexlink.rpc.OrgAPI;
import com.nawforce.apexlink.rpc.OrgAPIImpl;
import com.nawforce.apexlink.rpc.OpenResult;
import com.nawforce.apexlink.rpc.GetIssuesResult;
import com.nawforce.apexlink.rpc.IdentifierRequest;
import com.nawforce.apexlink.rpc.IdentifierLocationResult;
import com.nawforce.apexlink.rpc.TargetLocation;
import scala.concurrent.Future;
import scala.concurrent.ExecutionContext;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Java bridge to access Scala OrgAPI functionality in a thread-safe manner.
 * Converts Scala Future types to Java CompletableFuture for easier Java interop.
 */
public class ScalaBridge {
    
    private final OrgAPI orgAPI;
    private final ExecutorService executorService;
    private final ExecutionContext executionContext;
    
    public ScalaBridge() {
        this.orgAPI = new OrgAPIImpl();
        this.executorService = Executors.newCachedThreadPool(r -> {
            Thread t = new Thread(r, "ScalaBridge-Thread");
            t.setDaemon(true);
            return t;
        });
        this.executionContext = ExecutionContext.fromExecutorService(executorService);
    }
    
    /**
     * Open an Apex workspace directory for analysis
     */
    public CompletableFuture<OpenResult> openWorkspace(String directory) {
        return convertScalaFuture(orgAPI.open(directory));
    }
    
    /**
     * Get static analysis issues for the workspace
     */
    public CompletableFuture<GetIssuesResult> getIssues(boolean includeWarnings, int maxIssuesPerFile) {
        return convertScalaFuture(orgAPI.getIssues(includeWarnings, maxIssuesPerFile));
    }
    
    /**
     * Get definition location for an identifier at a specific position
     */
    public CompletableFuture<IdentifierLocationResult> getDefinition(String path, int line, int offset) {
        // TODO: Convert path/line/offset to TypeIdentifier
        // For now, return a placeholder result
        CompletableFuture<IdentifierLocationResult> future = new CompletableFuture<>();
        future.completeExceptionally(new UnsupportedOperationException("getDefinition not yet implemented"));
        return future;
    }
    
    /**
     * Find all references to an identifier at a specific position
     */
    public CompletableFuture<TargetLocation[]> getReferences(String path, int line, int offset) {
        return convertScalaFuture(orgAPI.getReferences(path, line, offset));
    }
    
    /**
     * Get the version of the language server
     */
    public CompletableFuture<String> getVersion() {
        return convertScalaFuture(orgAPI.version());
    }
    
    /**
     * Refresh analysis for a specific file
     */
    public CompletableFuture<Void> refresh(String path, boolean highPriority) {
        // Handle Scala Unit return type
        return convertScalaFuture(orgAPI.refresh(path, highPriority))
            .thenApply(unit -> null);
    }
    
    /**
     * Convert a Scala Future to a Java CompletableFuture
     */
    private <T> CompletableFuture<T> convertScalaFuture(Future<T> scalaFuture) {
        CompletableFuture<T> javaFuture = new CompletableFuture<>();
        
        scalaFuture.onComplete(result -> {
            if (result.isSuccess()) {
                javaFuture.complete(result.get());
            } else {
                javaFuture.completeExceptionally(result.failed().get());
            }
            return null;
        }, executionContext);
        
        return javaFuture;
    }
    
    /**
     * Shutdown the bridge and release resources
     */
    public void shutdown() {
        executorService.shutdown();
    }
}