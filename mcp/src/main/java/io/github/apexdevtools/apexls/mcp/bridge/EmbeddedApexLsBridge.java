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
import com.nawforce.apexlink.api.IndexerConfiguration;
import com.nawforce.apexlink.api.ServerOps;
import com.nawforce.apexlink.rpc.*;
import com.nawforce.pkgforce.diagnostics.Issue;
import io.github.apexdevtools.apexls.mcp.MCPServerConfig;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
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

  // Indexer configuration loaded from environment variables
  private static final IndexerConfig indexerConfig = new IndexerConfig();

  private final MCPServerConfig config;
  private final Map<String, OrgAPI> workspaceCache = new ConcurrentHashMap<>();
  private boolean initialized = false;

  public EmbeddedApexLsBridge(MCPServerConfig config) {
    this.config = config;
    logger.trace("Bridge created with MCP config: {}", config);
  }

  @Override
  public void initialize() throws Exception {
    try {
      logger.info("Initializing embedded apex-ls bridge...");

      // Validate indexer configuration parameters
      indexerConfig.validate();

      // Configure global filesystem monitoring to detect changes and auto-refresh
      // Trigger time: filesystem events closer than this value apart will trigger a rescan
      // Quiet period: wait time after the last event before rescanning begins
      try {
        IndexerConfiguration config =
            new IndexerConfiguration(
                indexerConfig.getTriggerMs(), indexerConfig.getQuietPeriodMs());
        ServerOps.setIndexerConfiguration(config);
        logger.info(
            "Configured filesystem monitoring with {}ms trigger, {}ms quiet period",
            config.rescanTriggerTimeMs(),
            config.quietPeriodForRescanMs());
      } catch (Exception configEx) {
        logger.warn(
            "Failed to configure filesystem monitoring, falling back to disabled indexer",
            configEx);
        // Fallback to disabled indexer (0, 0) if configuration fails
        IndexerConfiguration fallbackConfig = new IndexerConfiguration(0, 0);
        ServerOps.setIndexerConfiguration(fallbackConfig);
        logger.info("Filesystem monitoring disabled due to configuration error");
      }

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
          logger.debug("Creating OrgAPI for workspace: {}", dir);

          OrgAPI orgAPI = new OrgAPIImpl();

          // Build OpenOptions with MCP server configuration
          OpenOptions options =
              OpenOptions.create()
                  .withParser("OutlineSingle")
                  .withAutoFlush(config.isCacheEnabled())
                  .withLoggingLevel(config.getLoggingLevel())
                  .withCache(config.isCacheEnabled());

          orgAPI.open(dir, options);
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
      return future.thenApply(
          result -> {
            String json = convertGetIssuesResultToJson(result);
            if (logger.isDebugEnabled()) {
              logger.debug(
                  "Found {} issues for workspace {}", result.issues().length, workspaceDirectory);
            }
            return json;
          });

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
      return future.thenApply(
          result -> {
            String json = convertTargetLocationsToJson(result);
            if (logger.isDebugEnabled()) {
              logger.debug("Found {} usages for {}:{}:{}", result.length, filePath, line, offset);
            }
            return json;
          });

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
          version ->
              "{\n"
                  + "  \"workspace\": \""
                  + workspaceDirectory
                  + "\",\n"
                  + "  \"status\": \"active\",\n"
                  + "  \"type\": \"apex\",\n"
                  + "  \"version\": \""
                  + version
                  + "\"\n"
                  + "}");

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
  public CompletableFuture<String> getTestClassItemsChanged(
      String workspaceDirectory, String[] changedPaths) {
    if (!isReady()) {
      return CompletableFuture.failedFuture(new IllegalStateException("Bridge not initialized"));
    }

    try {
      // Get or create OrgAPI instance for this workspace
      OrgAPI orgAPI = getOrCreateOrgAPI(workspaceDirectory);

      // Call OrgAPI.getTestClassItemsChanged() with the array of changed paths
      Future<?> scalaFuture = orgAPI.getTestClassItemsChanged(changedPaths);

      // Convert Scala Future to Java CompletableFuture and then to JSON
      // Pass the original changed paths for filtering
      return convertScalaFuture(scalaFuture)
          .thenApply(result -> convertTestClassItemsResultToJson(result, changedPaths));

    } catch (Exception ex) {
      logger.error("Error calling getTestClassItemsChanged via bridge", ex);
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

  /** Create standard success response for tools with optional array data. */
  private String createToolResponse(
      String toolName,
      String status,
      String summary,
      int count,
      String arrayName,
      ArrayNode arrayData) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", toolName);
      response.put("status", status);
      response.put("summary", summary);

      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total", count);
      response.set("counts", counts);

      response.set(arrayName, arrayData != null ? arrayData : objectMapper.createArrayNode());

      return objectMapper.writeValueAsString(response);
    } catch (Exception ex) {
      return String.format("{\"status\":\"%s\",\"summary\":\"%s\"}", status, summary);
    }
  }

  /** Create success response for usages tool. */
  private String createUsagesResponse(int count, String message) {
    String summary = message != null ? message : "No usages found";
    return createToolResponse("apex_find_usages", "success", summary, count, "usages", null);
  }

  /** Create error response for usages tool. */
  private String createUsagesErrorResponse(String errorMessage) {
    return createToolResponse("apex_find_usages", "error", errorMessage, 0, "usages", null);
  }

  /** Create success response for definitions tool. */
  private String createDefinitionResponse(int count, String message) {
    String summary = message != null ? message : "No definitions found";
    return createToolResponse(
        "apex_find_definition", "success", summary, count, "definitions", null);
  }

  /** Create error response for definitions tool. */
  private String createDefinitionErrorResponse(String errorMessage) {
    return createToolResponse(
        "apex_find_definition", "error", errorMessage, 0, "definitions", null);
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
    if (targetLocations == null || targetLocations.length == 0) {
      return createUsagesResponse(0, null);
    }

    try {
      // Convert each target location to structured JSON
      ArrayNode referenceList = objectMapper.createArrayNode();
      for (TargetLocation targetLocation : targetLocations) {
        ObjectNode refNode = convertTargetLocationToJson(targetLocation);
        referenceList.add(refNode);
      }

      String summary = "Found " + targetLocations.length + " usage(s)";
      return createToolResponse(
          "apex_find_usages",
          "completed",
          summary,
          targetLocations.length,
          "usages",
          referenceList);

    } catch (Exception ex) {
      logger.warn("Error converting target locations to JSON", ex);
      return createUsagesErrorResponse("Error formatting usages: " + ex.getMessage());
    }
  }

  /** Convert LocationLink[] array to Enhanced JSON format for AI consumption. */
  private String convertLocationLinksToJson(LocationLink[] locationLinks) {
    if (locationLinks == null || locationLinks.length == 0) {
      return createDefinitionResponse(0, null);
    }

    try {
      // Convert each location link to structured JSON
      ArrayNode definitionList = objectMapper.createArrayNode();
      for (LocationLink locationLink : locationLinks) {
        ObjectNode defNode = convertLocationLinkToJson(locationLink);
        definitionList.add(defNode);
      }

      String summary = "Found " + locationLinks.length + " definition(s)";
      return createToolResponse(
          "apex_find_definition",
          "completed",
          summary,
          locationLinks.length,
          "definitions",
          definitionList);

    } catch (Exception ex) {
      logger.warn("Error converting location links to JSON", ex);
      return createDefinitionErrorResponse("Error formatting definitions: " + ex.getMessage());
    }
  }

  /** Convert TestClassItemsResult to simplified JSON format for the impacted tests tool. */
  private String convertTestClassItemsResultToJson(Object result, String[] originalChangedPaths) {
    if (result == null) {
      return createImpactedTestsResponse(new String[0]);
    }

    try {
      // Cast to TestClassItemsResult - this is the return type from getTestClassItemsChanged
      TestClassItemsResult testResult = (TestClassItemsResult) result;
      ClassTestItem[] items = testResult.items();

      if (items == null || items.length == 0) {
        return createImpactedTestsResponse(new String[0]);
      }

      // Extract file paths from ClassTestItem objects and filter out original changed files
      List<String> testFilePaths =
          Arrays.stream(items)
              .map(item -> item.targetLocation().targetPath())
              .collect(Collectors.toList());

      // Filter out test files that correspond to the original changed files
      // This is a workaround to exclude test classes that were in the original input
      List<String> filteredTestPaths =
          filterOutOriginalChangedFiles(testFilePaths, originalChangedPaths);

      return createImpactedTestsResponse(filteredTestPaths.toArray(new String[0]));

    } catch (Exception ex) {
      logger.warn("Error converting test class items to JSON", ex);
      return createImpactedTestsErrorResponse(
          "Error formatting impacted tests: " + ex.getMessage());
    }
  }

  /** Create a success response for impacted tests with simplified JSON format. */
  private String createImpactedTestsResponse(String[] testFilePaths) {
    try {
      ArrayNode testFiles = objectMapper.createArrayNode();
      for (String filePath : testFilePaths) {
        testFiles.add(filePath);
      }

      String summary = "Found " + testFilePaths.length + " impacted test class(es)";
      return createImpactedTestsToolResponse("completed", summary, testFilePaths.length, testFiles);

    } catch (Exception ex) {
      logger.warn("Error creating impacted tests response", ex);
      return createImpactedTestsErrorResponse("Error creating response: " + ex.getMessage());
    }
  }

  /** Create an error response for impacted tests. */
  private String createImpactedTestsErrorResponse(String errorMessage) {
    return createImpactedTestsToolResponse(
        "error", errorMessage, 0, objectMapper.createArrayNode());
  }

  /** Create standard response for impacted tests tool. */
  private String createImpactedTestsToolResponse(
      String status, String summary, int count, ArrayNode testFiles) {
    try {
      ObjectNode response = objectMapper.createObjectNode();
      response.put("tool", "apex_find_impacted_tests");
      response.put("status", status);
      response.put("summary", summary);

      ObjectNode counts = objectMapper.createObjectNode();
      counts.put("total_impacted_tests", count);
      response.set("counts", counts);

      response.set("impacted_test_files", testFiles);

      return objectMapper.writeValueAsString(response);

    } catch (Exception ex) {
      logger.error("Error creating impacted tests response", ex);
      return "{\"tool\":\"apex_find_impacted_tests\",\"status\":\"error\",\"summary\":\"Internal error\"}";
    }
  }

  /**
   * Filter out test files that correspond to the original changed files. This is a workaround for
   * the fact that getTestClassItemsChanged returns test classes that were in the original input,
   * but we only want tests that are impacted by the changes.
   */
  private List<String> filterOutOriginalChangedFiles(
      List<String> testFilePaths, String[] originalChangedPaths) {
    // Extract class names from original changed paths (remove .cls extension and path)
    Set<String> originalClassNames =
        Arrays.stream(originalChangedPaths)
            .map(
                path -> {
                  String fileName = Paths.get(path).getFileName().toString();
                  return fileName.endsWith(".cls")
                      ? fileName.substring(0, fileName.length() - 4)
                      : fileName;
                })
            .collect(Collectors.toSet());

    // Filter out test files that have the same class name as the original changed files
    return testFilePaths.stream()
        .filter(
            testPath -> {
              String testFileName = Paths.get(testPath).getFileName().toString();
              String testClassName =
                  testFileName.endsWith(".cls")
                      ? testFileName.substring(0, testFileName.length() - 4)
                      : testFileName;
              return !originalClassNames.contains(testClassName);
            })
        .collect(Collectors.toList());
  }
}
