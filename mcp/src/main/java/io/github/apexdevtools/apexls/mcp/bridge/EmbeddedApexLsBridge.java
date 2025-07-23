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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.nawforce.apexlink.rpc.GetIssuesResult;
import com.nawforce.apexlink.rpc.LocationLink;
import com.nawforce.apexlink.rpc.OrgAPI;
import com.nawforce.apexlink.rpc.OrgAPIImpl;
import com.nawforce.apexlink.rpc.TargetLocation;
import com.nawforce.pkgforce.diagnostics.Issue;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.concurrent.Future;

/**
 * Embedded bridge implementation that accesses apex-ls core functionality directly within the same
 * JVM process.
 *
 * <p>This bridge uses the OrgAPI interface directly from the apex-ls JAR on the classpath,
 * converting Scala Futures to Java CompletableFutures and results to JSON strings for consumption
 * by the Java 17 MCP tools.
 *
 * <p>OrgAPI instances are cached per workspace to avoid expensive open() calls.
 */
public class EmbeddedApexLsBridge implements ApexLsBridge {

  private static final Logger logger = LoggerFactory.getLogger(EmbeddedApexLsBridge.class);
  private static final ObjectMapper objectMapper = new ObjectMapper();

  private final Map<String, OrgAPI> workspaceCache = new ConcurrentHashMap<>();
  private boolean initialized = false;

  @Override
  public void initialize() throws Exception {
    try {
      logger.info("Initializing embedded apex-ls bridge...");

      // No need to create OrgAPI instances here - they're created on demand
      initialized = true;
      logger.info("Embedded apex-ls bridge initialized successfully");

    } catch (Exception ex) {
      logger.error("Failed to initialize embedded apex-ls bridge", ex);
      throw new Exception("Failed to initialize bridge: " + ex.getMessage(), ex);
    }
  }

  @Override
  public boolean isReady() {
    return initialized;
  }

  /**
   * Get or create an OrgAPI instance for the specified workspace. Instances are cached to avoid
   * expensive open() calls.
   */
  private OrgAPI getOrCreateOrgAPI(String workspaceDirectory) {
    return workspaceCache.computeIfAbsent(
        workspaceDirectory,
        dir -> {
          logger.info("Creating new OrgAPI instance for workspace: {}", dir);
          OrgAPI orgAPI = new OrgAPIImpl();
          orgAPI.open(dir);
          return orgAPI;
        });
  }

  @Override
  public CompletableFuture<String> getIssues(
      String workspaceDirectory, boolean includeWarnings, int maxIssuesPerFile) {
    if (!isReady()) {
      return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
    }

    try {
      // Get or create OrgAPI instance for this workspace
      OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);

      // Call OrgAPI.getIssues() directly with maxIssuesPerFile parameter
      Future<GetIssuesResult> scalaFuture = orgAPI.getIssues(includeWarnings, maxIssuesPerFile);

      // Convert Scala Future to Java CompletableFuture
      CompletableFuture<GetIssuesResult> future = convertScalaFuture(scalaFuture);

      // Convert the result to JSON string
      return future.thenApply(this::convertGetIssuesResultToJson);

    } catch (Exception ex) {
      logger.error("Error calling getIssues via bridge", ex);
      return CompletableFuture.failedFuture(ex);
    }
  }

  @Override
  public CompletableFuture<String> findUsages(
      String workspaceDirectory, String filePath, int line, int offset) {
    if (!isReady()) {
      return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
    }

    try {
      // Get or create OrgAPI instance for this workspace
      OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);

      // Call OrgAPI.getReferences() directly
      // Note: Both MCP tools and OrgAPI use 1-based line numbers
      Future<TargetLocation[]> scalaFuture = orgAPI.getReferences(filePath, line, offset);

      // Convert Scala Future to Java CompletableFuture
      CompletableFuture<TargetLocation[]> future = convertScalaFuture(scalaFuture);

      // Convert the result to JSON string
      return future.thenApply(this::convertTargetLocationsToJson);

    } catch (Exception ex) {
      logger.error("Error calling findUsages via bridge", ex);
      return CompletableFuture.failedFuture(ex);
    }
  }

  @Override
  public CompletableFuture<String> getDefinition(
      String workspaceDirectory, String filePath, int line, int offset) {
    if (!isReady()) {
      return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
    }

    try {
      // Get or create OrgAPI instance for this workspace
      OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);

      // Call OrgAPI.getDefinition() directly
      // Note: Both MCP tools and OrgAPI use 1-based line numbers
      Future<LocationLink[]> scalaFuture =
          orgAPI.getDefinition(filePath, line, offset, scala.Option.empty());

      // Convert Scala Future to Java CompletableFuture
      CompletableFuture<LocationLink[]> future = convertScalaFuture(scalaFuture);

      // Convert the result to JSON string
      return future.thenApply(this::convertLocationLinksToJson);

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
      // Get or create OrgAPI instance for this workspace
      OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);

      // Get version info
      Future<String> scalaVersionFuture = orgAPI.version();
      CompletableFuture<String> versionFuture = convertScalaFuture(scalaVersionFuture);

      // Build workspace info JSON
      return versionFuture.thenApply(
          version -> {
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
      // Use a temporary OrgAPI instance for version info (no workspace needed)
      OrgAPI tempOrgAPI = new OrgAPIImpl();
      Future<String> scalaFuture = tempOrgAPI.version();

      // Convert Scala Future to Java CompletableFuture
      return convertScalaFuture(scalaFuture);

    } catch (Exception ex) {
      logger.error("Error calling getVersion via bridge", ex);
      return CompletableFuture.failedFuture(ex);
    }
  }

  @Override
  public void close() throws Exception {
    // Clear all cached OrgAPI instances
    int cacheSize = workspaceCache.size();
    workspaceCache.clear();
    initialized = false;
    logger.info("Embedded apex-ls bridge closed, cleared {} workspace caches", cacheSize);
  }

  /** Convert a Scala Future to a Java CompletableFuture */
  private <T> CompletableFuture<T> convertScalaFuture(Future<T> scalaFuture) {
    CompletableFuture<T> javaFuture = new CompletableFuture<>();

    scalaFuture.onComplete(
        result -> {
          if (result.isSuccess()) {
            javaFuture.complete(result.get());
          } else {
            javaFuture.completeExceptionally(result.failed().get());
          }
          return null;
        },
        scala.concurrent.ExecutionContext.global());

    return javaFuture;
  }

  /** Convert GetIssuesResult to Enhanced JSON format for AI consumption. */
  private String convertGetIssuesResultToJson(GetIssuesResult getIssuesResult) {
    if (getIssuesResult == null) {
      return createSuccessResponse();
    }

    try {
      // Get issues directly from GetIssuesResult
      Object[] issues = getIssuesResult.issues();

      if (issues.length == 0) {
        return createSuccessResponse();
      }

      // Create enhanced JSON structure
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "sfdx_code_diagnostics");
      response.put("status", "completed");
      response.put("summary", "Found " + issues.length + " issue(s) in the codebase");

      // Categorize issues
      int errors = 0, warnings = 0, info = 0;
      ArrayNode issueList = objectMapper.createArrayNode();

      for (Object issueObj : issues) {
        Issue issue = (Issue) issueObj;
        ObjectNode issueNode = convertIssueToJson(issue);
        issueList.add(issueNode);

        // Count by severity
        String severity = issueNode.get("severity").asText().toLowerCase();
        switch (severity) {
          case "error":
            errors++;
            break;
          case "warning":
            warnings++;
            break;
          default:
            info++;
            break;
        }
      }

      // Add counts
      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", issues.length);
      counts.put("errors", errors);
      counts.put("warnings", warnings);
      counts.put("info", info);
      response.set("counts", counts);

      // Add detailed issues
      response.set("issues", issueList);

      return objectMapper.writeValueAsString(response);

    } catch (Exception ex) {
      logger.warn("Error converting GetIssuesResult to JSON", ex);
      return createErrorResponse("Error formatting issues: " + ex.getMessage());
    }
  }

  /** Convert Issue instance to structured JSON using direct property access. */
  private ObjectNode convertIssueToJson(Issue issue) {
    ObjectNode issueNode = objectMapper.createObjectNode();

    try {
      // Get severity from diagnostic category
      String severity = issue.diagnostic().category().toString().toLowerCase();
      issueNode.put("severity", mapSeverity(severity));

      // Get file path
      issueNode.put("file", issue.path().toString());

      // Get location information from diagnostic
      var location = issue.diagnostic().location();
      issueNode.put("line", location.startLine());
      issueNode.put("column", location.startPosition());

      // Get message from diagnostic
      String message = issue.diagnostic().message();
      issueNode.put("message", message != null ? message : "No message available");

      // Add metadata
      issueNode.put("source", "apex-ls");

    } catch (Exception ex) {
      logger.debug("Error accessing Issue properties", ex);
      // Fallback to string representation
      issueNode.put("severity", "unknown");
      issueNode.put("file", "unknown");
      issueNode.put("line", 0);
      issueNode.put("column", 0);
      issueNode.put("message", issue.toString());
      issueNode.put("source", "apex-ls");
    }

    return issueNode;
  }

  /** Map apex-ls category to standard severity levels. */
  private String mapSeverity(String category) {
    if (category == null) return "unknown";

    category = category.toLowerCase();
    if (category.contains("error")) return "error";
    if (category.contains("warning")) return "warning";
    if (category.contains("info")) return "info";
    if (category.contains("unused")) return "info"; // Map unused categories to info level
    return category; // Return as-is if no mapping found
  }

  /** Create success response for no issues found. */
  private String createSuccessResponse() {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "sfdx_code_diagnostics");
      response.put("status", "success");
      response.put("summary", "No issues found - code analysis passed successfully");

      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", 0);
      counts.put("errors", 0);
      counts.put("warnings", 0);
      counts.put("info", 0);
      response.set("counts", counts);

      response.set("issues", objectMapper.createArrayNode());

      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return "{\"status\":\"success\",\"summary\":\"No issues found - code analysis passed successfully\"}";
    }
  }

  /** Create error response for formatting failures. */
  private String createErrorResponse(String errorMessage) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "sfdx_code_diagnostics");
      response.put("status", "error");
      response.put("summary", errorMessage);
      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return "{\"status\":\"error\",\"summary\":\"" + errorMessage + "\"}";
    }
  }

  /** Create usages response for empty results. */
  private String createUsagesResponse(int count, String message) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_usages");
      response.put("status", "success");
      response.put("summary", message != null ? message : "No usages found");

      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", count);
      response.set("counts", counts);

      response.set("usages", objectMapper.createArrayNode());

      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return "{\"status\":\"success\",\"summary\":\"No usages found\"}";
    }
  }

  /** Create error response for usages formatting failures. */
  private String createUsagesErrorResponse(String errorMessage) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_usages");
      response.put("status", "error");
      response.put("summary", errorMessage);
      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return "{\"status\":\"error\",\"summary\":\"" + errorMessage + "\"}";
    }
  }

  /** Create definition response for empty results. */
  private String createDefinitionResponse(int count, String message) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_definition");
      response.put("status", "success");
      response.put("summary", message != null ? message : "No definitions found");

      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", count);
      response.set("counts", counts);

      response.set("definitions", objectMapper.createArrayNode());

      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return "{\"status\":\"success\",\"summary\":\"No definitions found\"}";
    }
  }

  /** Create error response for definition formatting failures. */
  private String createDefinitionErrorResponse(String errorMessage) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_definition");
      response.put("status", "error");
      response.put("summary", errorMessage);
      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return "{\"status\":\"error\",\"summary\":\"" + errorMessage + "\"}";
    }
  }

  /** Convert TargetLocation instance to structured JSON. */
  private ObjectNode convertTargetLocationToJson(TargetLocation targetLocation) {
    ObjectNode refNode = objectMapper.createObjectNode();

    try {
      // Get file path
      refNode.put("file", targetLocation.targetPath());

      // Get location information
      var range = targetLocation.range();
      refNode.put("line", range.startLine());
      refNode.put("column", range.startPosition());
      refNode.put("endLine", range.endLine());
      refNode.put("endColumn", range.endPosition());

      // Add metadata
      refNode.put("source", "apex-ls");
      refNode.put("type", "reference");

    } catch (Exception ex) {
      logger.debug("Error accessing TargetLocation properties", ex);
      // Fallback to string representation
      refNode.put("file", "unknown");
      refNode.put("line", 0);
      refNode.put("column", 0);
      refNode.put("endLine", 0);
      refNode.put("endColumn", 0);
      refNode.put("source", "apex-ls");
      refNode.put("type", "reference");
      refNode.put("raw", targetLocation.toString());
    }

    return refNode;
  }

  /** Convert LocationLink instance to structured JSON. */
  private ObjectNode convertLocationLinkToJson(LocationLink locationLink) {
    ObjectNode defNode = objectMapper.createObjectNode();

    try {
      // Get file path
      defNode.put("file", locationLink.targetPath());

      // Get location information from target range
      var target = locationLink.target();
      defNode.put("line", target.startLine());
      defNode.put("column", target.startPosition());
      defNode.put("endLine", target.endLine());
      defNode.put("endColumn", target.endPosition());

      // Get target selection information
      var targetSelection = locationLink.targetSelection();
      defNode.put("targetLine", targetSelection.startLine());
      defNode.put("targetColumn", targetSelection.startPosition());
      defNode.put("targetEndLine", targetSelection.endLine());
      defNode.put("targetEndColumn", targetSelection.endPosition());

      // Add metadata
      defNode.put("source", "apex-ls");
      defNode.put("type", "definition");

    } catch (Exception ex) {
      logger.debug("Error accessing LocationLink properties", ex);
      // Fallback to string representation
      defNode.put("file", "unknown");
      defNode.put("line", 0);
      defNode.put("column", 0);
      defNode.put("endLine", 0);
      defNode.put("endColumn", 0);
      defNode.put("targetLine", 0);
      defNode.put("targetColumn", 0);
      defNode.put("targetEndLine", 0);
      defNode.put("targetEndColumn", 0);
      defNode.put("source", "apex-ls");
      defNode.put("type", "definition");
      defNode.put("raw", locationLink.toString());
    }

    return defNode;
  }

  /** Convert TargetLocation[] array to Enhanced JSON format for AI consumption. */
  private String convertTargetLocationsToJson(TargetLocation[] targetLocations) {
    if (targetLocations == null) {
      return createUsagesResponse(0, null);
    }

    try {
      if (targetLocations.length == 0) {
        return createUsagesResponse(0, null);
      }

      // Create enhanced JSON structure
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_usages");
      response.put("status", "completed");
      response.put("summary", "Found " + targetLocations.length + " usage(s)");

      // Add counts
      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", targetLocations.length);
      response.set("counts", counts);

      // Convert each target location to structured JSON
      ArrayNode referenceList = objectMapper.createArrayNode();
      for (TargetLocation targetLocation : targetLocations) {
        ObjectNode refNode = convertTargetLocationToJson(targetLocation);
        referenceList.add(refNode);
      }

      response.set("usages", referenceList);
      return objectMapper.writeValueAsString(response);

    } catch (Exception ex) {
      logger.warn("Error converting target locations to JSON", ex);
      return createUsagesErrorResponse("Error formatting usages: " + ex.getMessage());
    }
  }

  /** Convert LocationLink[] array to Enhanced JSON format for AI consumption. */
  private String convertLocationLinksToJson(LocationLink[] locationLinks) {
    if (locationLinks == null) {
      return createDefinitionResponse(0, null);
    }

    try {
      if (locationLinks.length == 0) {
        return createDefinitionResponse(0, null);
      }

      // Create enhanced JSON structure
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_definition");
      response.put("status", "completed");
      response.put("summary", "Found " + locationLinks.length + " definition(s)");

      // Add counts
      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", locationLinks.length);
      response.set("counts", counts);

      // Convert each location link to structured JSON
      ArrayNode definitionList = objectMapper.createArrayNode();
      for (LocationLink locationLink : locationLinks) {
        ObjectNode defNode = convertLocationLinkToJson(locationLink);
        definitionList.add(defNode);
      }

      response.set("definitions", definitionList);
      return objectMapper.writeValueAsString(response);

    } catch (Exception ex) {
      logger.warn("Error converting location links to JSON", ex);
      return createDefinitionErrorResponse("Error formatting definitions: " + ex.getMessage());
    }
  }
}
