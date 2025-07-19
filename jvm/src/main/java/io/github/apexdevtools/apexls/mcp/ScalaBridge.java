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

import com.nawforce.apexlink.rpc.OrgAPI;
import com.nawforce.apexlink.rpc.OrgAPIImpl;
import com.nawforce.apexlink.rpc.GetIssuesResult;
import com.nawforce.apexlink.rpc.IdentifierLocationResult;
import com.nawforce.apexlink.rpc.TargetLocation;
import scala.concurrent.Future;
import scala.concurrent.ExecutionContext;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;

/**
 * Java bridge to access Scala OrgAPI functionality in a thread-safe manner.
 * Supports multiple workspaces by creating OrgAPI instances on-demand.
 * Converts Scala Future types to Java CompletableFuture for easier Java interop.
 */
public class ScalaBridge {

    private final Map<String, OrgAPI> workspaceOrgAPIs;
    private final ExecutorService executorService;
    private final ExecutionContext executionContext;

    public ScalaBridge() {
        this.workspaceOrgAPIs = new ConcurrentHashMap<>();
        this.executorService = Executors.newCachedThreadPool(r -> {
            Thread t = new Thread(r, "ScalaBridge-Thread");
            t.setDaemon(true);
            return t;
        });
        this.executionContext = ExecutionContext.fromExecutorService(executorService);
    }

    /**
     * Get or create an OrgAPI instance for the specified workspace directory
     */
    private OrgAPI getOrCreateOrgAPI(String workspaceDirectory) {
        return workspaceOrgAPIs.computeIfAbsent(workspaceDirectory, dir -> {
            OrgAPI orgAPI = new OrgAPIImpl();
            // Initialize the workspace asynchronously
            orgAPI.open(dir);
            return orgAPI;
        });
    }

    /**
     * Get static analysis issues for the workspace
     */
    public CompletableFuture<GetIssuesResult> getIssues(String workspaceDirectory, boolean includeWarnings, int maxIssuesPerFile) {
        OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);
        return convertScalaFuture(orgAPI.getIssues(includeWarnings, maxIssuesPerFile));
    }

    /**
     * Get definition location for an identifier at a specific position
     */
    public CompletableFuture<IdentifierLocationResult> getDefinition(String workspaceDirectory, String path, int line, int offset) {
        // TODO: Convert path/line/offset to TypeIdentifier
        // For now, return a placeholder result
        CompletableFuture<IdentifierLocationResult> future = new CompletableFuture<>();
        future.completeExceptionally(new UnsupportedOperationException("getDefinition not yet implemented"));
        return future;
    }

    /**
     * Find all references to an identifier at a specific position
     */
    public CompletableFuture<TargetLocation[]> getReferences(String workspaceDirectory, String path, int line, int offset) {
        OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);
        return convertScalaFuture(orgAPI.getReferences(path, line, offset));
    }

    /**
     * Get the version of the language server for a specific workspace
     */
    public CompletableFuture<String> getVersion(String workspaceDirectory) {
        OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);
        return convertScalaFuture(orgAPI.version());
    }

    /**
     * Get the version of the language server (uses a default temporary OrgAPI)
     */
    public CompletableFuture<String> getVersion() {
        // For version info, we can use a temporary OrgAPI instance
        OrgAPI tempOrgAPI = new OrgAPIImpl();
        return convertScalaFuture(tempOrgAPI.version());
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
     * Close and remove a specific workspace
     */
    public void closeWorkspace(String workspaceDirectory) {
        workspaceOrgAPIs.remove(workspaceDirectory);
        // OrgAPI doesn't have an explicit close method, so just remove from map
        // The GC will handle cleanup when no longer referenced
    }

    /**
     * Get all currently managed workspace directories
     */
    public String[] getWorkspaceDirectories() {
        return workspaceOrgAPIs.keySet().toArray(new String[0]);
    }

    /**
     * Shutdown the bridge and release all resources
     */
    public void shutdown() {
        // Clear all workspace OrgAPIs
        workspaceOrgAPIs.clear();
        executorService.shutdown();
    }
}