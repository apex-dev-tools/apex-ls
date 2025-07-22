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
  public CompletableFuture<String> findReferences(
      String workspaceDirectory, String filePath, int line, int offset) {
    if (!isReady()) {
      return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
    }

    try {
      // Get or create OrgAPI instance for this workspace
      OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);

      // Call OrgAPI.getReferences() directly
      Future<TargetLocation[]> scalaFuture = orgAPI.getReferences(filePath, line, offset);

      // Convert Scala Future to Java CompletableFuture
      CompletableFuture<TargetLocation[]> future = convertScalaFuture(scalaFuture);

      // Convert the result to JSON string
      return future.thenApply(this::convertTargetLocationsToJson);

    } catch (Exception ex) {
      logger.error("Error calling findReferences via bridge", ex);
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

  /** Convert TargetLocation[] array to JSON string representation. */
  private String convertTargetLocationsToJson(TargetLocation[] targetLocations) {
    if (targetLocations == null) {
      return "No references found";
    }

    try {
      if (targetLocations.length == 0) {
        return "No references found";
      }

      StringBuilder json = new StringBuilder();
      json.append("Found ").append(targetLocations.length).append(" reference(s):\\n");
      for (int i = 0; i < targetLocations.length; i++) {
        json.append((i + 1)).append(". ").append(targetLocations[i].toString()).append("\\n");
      }
      return json.toString();

    } catch (Exception ex) {
      logger.warn("Error converting target locations to JSON", ex);
      return "Error formatting references: " + ex.getMessage();
    }
  }

  /** Convert LocationLink[] array to JSON string representation. */
  private String convertLocationLinksToJson(LocationLink[] locationLinks) {
    if (locationLinks == null || locationLinks.length == 0) {
      return "Definition not found";
    }

    try {
      StringBuilder json = new StringBuilder();
      json.append("Found ").append(locationLinks.length).append(" definition(s):\\n");

      for (int i = 0; i < locationLinks.length; i++) {
        LocationLink link = locationLinks[i];
        json.append((i + 1)).append(". ");
        json.append("File: ").append(link.targetPath()).append("\\n");
        json.append("   Target: ").append(link.target()).append("\\n");
        json.append("   Selection: ").append(link.targetSelection()).append("\\n");
        if (i < locationLinks.length - 1) {
          json.append("\\n");
        }
      }
      return json.toString();

    } catch (Exception ex) {
      logger.warn("Error converting location links to JSON", ex);
      return "Error formatting definitions: " + ex.getMessage();
    }
  }

  /** Convert IdentifierLocationResult to JSON string representation. */
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
