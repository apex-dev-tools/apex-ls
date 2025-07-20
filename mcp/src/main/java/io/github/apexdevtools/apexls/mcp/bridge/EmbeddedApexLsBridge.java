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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.concurrent.CompletableFuture;

/**
 * Embedded bridge implementation that accesses apex-ls core functionality
 * via reflection within the same JVM process.
 * 
 * This bridge loads the apex-ls JAR on the classpath and uses reflection
 * to call the Scala bridge methods, converting the results to JSON strings
 * for consumption by the Java 17 MCP tools.
 */
public class EmbeddedApexLsBridge implements ApexLsBridge {
    
    private static final Logger logger = LoggerFactory.getLogger(EmbeddedApexLsBridge.class);
    
    private Object scalaBridge;
    private boolean initialized = false;
    
    @Override
    public void initialize() throws Exception {
        try {
            logger.info("Initializing embedded apex-ls bridge...");
            
            // Create OrgAPI directly instead of using ScalaBridge
            Class<?> orgAPIImplClass = Class.forName("com.nawforce.apexlink.rpc.OrgAPIImpl");
            scalaBridge = orgAPIImplClass.getDeclaredConstructor().newInstance();
            
            initialized = true;
            logger.info("Embedded apex-ls bridge initialized successfully");
            
        } catch (Exception ex) {
            logger.error("Failed to initialize embedded apex-ls bridge", ex);
            throw new Exception("Failed to initialize bridge: " + ex.getMessage(), ex);
        }
    }
    
    @Override
    public boolean isReady() {
        return initialized && scalaBridge != null;
    }
    
    @Override
    public CompletableFuture<String> getIssues(String workspaceDirectory, boolean includeWarnings, boolean includeUnused) {
        if (!isReady()) {
            return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
        }
        
        try {
            // First open the workspace
            Method openMethod = scalaBridge.getClass().getMethod("open", String.class);
            openMethod.invoke(scalaBridge, workspaceDirectory);
            
            // Call OrgAPI.getIssues() directly
            Method getIssuesMethod = scalaBridge.getClass().getMethod("getIssues", boolean.class, int.class);
            
            @SuppressWarnings("unchecked")
            CompletableFuture<Object> future = (CompletableFuture<Object>) getIssuesMethod.invoke(
                scalaBridge, includeWarnings, Integer.MAX_VALUE);
            
            // Convert the result to JSON string
            return future.thenApply(this::convertGetIssuesResultToJson);
            
        } catch (Exception ex) {
            logger.error("Error calling getIssues via bridge", ex);
            return CompletableFuture.failedFuture(ex);
        }
    }
    
    @Override
    public CompletableFuture<String> findReferences(String workspaceDirectory, String filePath, int line, int offset) {
        if (!isReady()) {
            return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
        }
        
        try {
            // First open the workspace
            Method openMethod = scalaBridge.getClass().getMethod("open", String.class);
            openMethod.invoke(scalaBridge, workspaceDirectory);
            
            // Call OrgAPI.getReferences() directly
            Method getReferencesMethod = scalaBridge.getClass().getMethod("getReferences", String.class, int.class, int.class);
            
            @SuppressWarnings("unchecked")
            CompletableFuture<Object> future = (CompletableFuture<Object>) getReferencesMethod.invoke(
                scalaBridge, filePath, line, offset);
            
            // Convert the result to JSON string
            return future.thenApply(this::convertTargetLocationsToJson);
            
        } catch (Exception ex) {
            logger.error("Error calling findReferences via bridge", ex);
            return CompletableFuture.failedFuture(ex);
        }
    }
    
    @Override
    public CompletableFuture<String> getDefinition(String workspaceDirectory, String filePath, int line, int offset) {
        if (!isReady()) {
            return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
        }
        
        try {
            // First open the workspace
            Method openMethod = scalaBridge.getClass().getMethod("open", String.class);
            openMethod.invoke(scalaBridge, workspaceDirectory);
            
            // Call OrgAPI.getDefinition() - this method doesn't exist in OrgAPI yet
            // For now, return a placeholder
            CompletableFuture<String> future = new CompletableFuture<>();
            future.complete("Definition lookup not yet implemented in OrgAPI");
            return future;
            
        } catch (Exception ex) {
            logger.error("Error calling getDefinition via bridge", ex);
            return CompletableFuture.failedFuture(ex);
        }
    }
    
    @Override
    public CompletableFuture<String> getWorkspaceInfo(String workspaceDirectory) {
        if (!isReady()) {
            return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
        }
        
        try {
            // First open the workspace
            Method openMethod = scalaBridge.getClass().getMethod("open", String.class);
            openMethod.invoke(scalaBridge, workspaceDirectory);
            
            // Get version info
            Method versionMethod = scalaBridge.getClass().getMethod("version");
            @SuppressWarnings("unchecked")
            CompletableFuture<String> versionFuture = (CompletableFuture<String>) versionMethod.invoke(scalaBridge);
            
            // Build workspace info JSON
            return versionFuture.thenApply(version -> {
                StringBuilder json = new StringBuilder();
                json.append("{\n");
                json.append("  \"workspace\": \"").append(workspaceDirectory).append("\",\n");
                json.append("  \"status\": \"active\",\n");
                json.append("  \"type\": \"apex\",\n");
                json.append("  \"version\": \"").append(version).append("\"\n");
                json.append("}");
                return json.toString();
            });
            
        } catch (Exception ex) {
            logger.error("Error calling getWorkspaceInfo via bridge", ex);
            return CompletableFuture.failedFuture(ex);
        }
    }
    
    @Override
    public CompletableFuture<String> getVersion() {
        if (!isReady()) {
            return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
        }
        
        try {
            // Call OrgAPI.version() via reflection
            Method versionMethod = scalaBridge.getClass().getMethod("version");
            
            @SuppressWarnings("unchecked")
            CompletableFuture<String> future = (CompletableFuture<String>) versionMethod.invoke(scalaBridge);
            
            return future;
            
        } catch (Exception ex) {
            logger.error("Error calling getVersion via bridge", ex);
            return CompletableFuture.failedFuture(ex);
        }
    }
    
    @Override
    public void close() throws Exception {
        if (scalaBridge != null) {
            try {
                Method shutdownMethod = scalaBridge.getClass().getMethod("shutdown");
                shutdownMethod.invoke(scalaBridge);
            } catch (Exception ex) {
                logger.warn("Error shutting down ScalaBridge", ex);
            }
            scalaBridge = null;
        }
        initialized = false;
        logger.info("Embedded apex-ls bridge closed");
    }
    
    /**
     * Convert GetIssuesResult to JSON string representation.
     */
    private String convertGetIssuesResultToJson(Object getIssuesResult) {
        if (getIssuesResult == null) {
            return "No issues found - code analysis passed successfully";
        }
        
        try {
            // Use reflection to get issues() method from GetIssuesResult
            Method issuesMethod = getIssuesResult.getClass().getMethod("issues");
            Object[] issues = (Object[]) issuesMethod.invoke(getIssuesResult);
            
            if (issues.length == 0) {
                return "No issues found - code analysis passed successfully";
            }
            
            StringBuilder json = new StringBuilder();
            json.append("Found ").append(issues.length).append(" issue(s):\\n");
            for (int i = 0; i < issues.length; i++) {
                json.append((i + 1)).append(". ").append(issues[i].toString()).append("\\n");
            }
            return json.toString();
            
        } catch (Exception ex) {
            logger.warn("Error converting GetIssuesResult to JSON", ex);
            return "Error formatting issues: " + ex.getMessage();
        }
    }
    
    /**
     * Convert TargetLocation[] array to JSON string representation.
     */
    private String convertTargetLocationsToJson(Object targetLocationsArray) {
        if (targetLocationsArray == null) {
            return "No references found";
        }
        
        try {
            Object[] locations = (Object[]) targetLocationsArray;
            if (locations.length == 0) {
                return "No references found";
            }
            
            StringBuilder json = new StringBuilder();
            json.append("Found ").append(locations.length).append(" reference(s):\\n");
            for (int i = 0; i < locations.length; i++) {
                json.append((i + 1)).append(". ").append(locations[i].toString()).append("\\n");
            }
            return json.toString();
            
        } catch (Exception ex) {
            logger.warn("Error converting target locations to JSON", ex);
            return "Error formatting references: " + ex.getMessage();
        }
    }
    
    /**
     * Convert IdentifierLocationResult to JSON string representation.
     */
    private String convertIdentifierLocationToJson(Object identifierLocationResult) {
        if (identifierLocationResult == null) {
            return "Definition not found";
        }
        
        try {
            return "Definition found: " + identifierLocationResult.toString();
        } catch (Exception ex) {
            logger.warn("Error converting identifier location to JSON", ex);
            return "Error formatting definition: " + ex.getMessage();
        }
    }
}