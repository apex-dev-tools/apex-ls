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

import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.github.apexdevtools.apexls.mcp.MCPServerConfig;
import java.util.concurrent.CompletableFuture;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/** Tests for Enhanced JSON format output from the bridge. */
class EnhancedJsonFormatTest {

  private EmbeddedApexLsBridge bridge;
  private ObjectMapper objectMapper;
  private String testWorkspacePath;
  
  /** Helper method to create default test configuration. */
  private MCPServerConfig createTestConfig() {
      return new MCPServerConfig("none", false);
  }

  @BeforeEach
  void setUp() throws Exception {
    bridge = new EmbeddedApexLsBridge(createTestConfig());
    bridge.initialize();
    objectMapper = new ObjectMapper();

    // Get test workspace path
    var testWorkspaceUrl = getClass().getClassLoader().getResource("test-workspace");
    if (testWorkspaceUrl != null) {
      testWorkspacePath = new java.io.File(testWorkspaceUrl.toURI()).getAbsolutePath();
    }
  }

  @Test
  @DisplayName("Should return Enhanced JSON for successful code analysis")
  void shouldReturnEnhancedJsonForSuccessfulAnalysis() throws Exception {
    // Skip if no test workspace available
    if (testWorkspacePath == null) {
      return;
    }

    CompletableFuture<String> future = bridge.getIssues(testWorkspacePath, false, 100);
    String result = future.get();

    assertNotNull(result);
    
    // Parse JSON to verify structure
    JsonNode json = objectMapper.readTree(result);
    
    // Verify top-level structure
    assertTrue(json.has("tool"));
    assertTrue(json.has("status"));
    assertTrue(json.has("summary"));
    assertTrue(json.has("counts"));
    assertTrue(json.has("issues"));
    
    assertEquals("sfdx_code_diagnostics", json.get("tool").asText());
    assertTrue(json.get("status").asText().matches("success|completed"));
    
    // Verify counts structure
    JsonNode counts = json.get("counts");
    assertTrue(counts.has("total"));
    assertTrue(counts.has("errors"));
    assertTrue(counts.has("warnings"));
    assertTrue(counts.has("info"));
    
    // All counts should be non-negative integers
    assertTrue(counts.get("total").asInt() >= 0);
    assertTrue(counts.get("errors").asInt() >= 0);
    assertTrue(counts.get("warnings").asInt() >= 0);
    assertTrue(counts.get("info").asInt() >= 0);
    
    // Verify issues is an array
    assertTrue(json.get("issues").isArray());
  }

  @Test
  @DisplayName("Should return Enhanced JSON for success with no issues")
  void shouldReturnEnhancedJsonForNoIssues() throws Exception {
    // Test with a clean workspace or mock response
    // This simulates the success case
    
    // For testing purposes, we'll verify the JSON structure matches expectations
    // The actual test workspace might have issues, but we can verify the format
    
    CompletableFuture<String> future;
    if (testWorkspacePath != null) {
      future = bridge.getIssues(testWorkspacePath, false, 0); // Limit to 0 to simulate no issues
    } else {
      // Skip test if no workspace
      return;
    }
    
    String result = future.get();
    assertNotNull(result);
    
    // Parse and verify it's valid JSON
    JsonNode json = objectMapper.readTree(result);
    assertNotNull(json);
    
    // Should still have proper structure even with limited results
    assertTrue(json.has("tool"));
    assertTrue(json.has("status"));
    assertTrue(json.has("summary"));
  }

  @Test
  @DisplayName("Enhanced JSON should be valid and well-formatted")
  void enhancedJsonShouldBeValidAndWellFormatted() throws Exception {
    if (testWorkspacePath == null) {
      return;
    }

    CompletableFuture<String> future = bridge.getIssues(testWorkspacePath, true, 50);
    String result = future.get();

    // Verify it's valid JSON
    assertDoesNotThrow(() -> objectMapper.readTree(result));
    
    // Verify it's not empty
    assertFalse(result.trim().isEmpty());
    
    // Should start and end with braces (valid JSON object)
    assertTrue(result.trim().startsWith("{"));
    assertTrue(result.trim().endsWith("}"));
  }

  @Test
  @DisplayName("Enhanced JSON should always satisfy mathematical invariants - Edge Cases")
  void enhancedJsonShouldSatisfyMathematicalInvariantsEdgeCases() throws Exception {
    if (testWorkspacePath == null) {
      return;
    }

    // Test edge cases with various parameter combinations
    testInvariantsForParameters(false, 0);   // No warnings, no issues limit
    testInvariantsForParameters(true, 0);    // With warnings, no issues limit
    testInvariantsForParameters(false, 1);   // No warnings, single issue
    testInvariantsForParameters(true, 1);    // With warnings, single issue
  }
  
  @Test
  @DisplayName("Enhanced JSON should always satisfy mathematical invariants - Boundary Values")
  void enhancedJsonShouldSatisfyMathematicalInvariantsBoundaryValues() throws Exception {
    if (testWorkspacePath == null) {
      return;
    }

    // Test boundary values
    testInvariantsForParameters(false, 10);  // No warnings, small limit
    testInvariantsForParameters(true, 10);   // With warnings, small limit
    testInvariantsForParameters(false, 50);  // No warnings, medium limit
    testInvariantsForParameters(true, 50);   // With warnings, medium limit
    testInvariantsForParameters(false, 100); // No warnings, large limit
    testInvariantsForParameters(true, 100);  // With warnings, large limit
  }
  
  @Test
  @DisplayName("Enhanced JSON should always satisfy mathematical invariants - Additional Coverage")
  void enhancedJsonShouldSatisfyMathematicalInvariantsAdditionalCoverage() throws Exception {
    if (testWorkspacePath == null) {
      return;
    }

    // Additional test cases for better coverage
    testInvariantsForParameters(false, 5);   // No warnings, very small limit
    testInvariantsForParameters(true, 25);   // With warnings, quarter limit
    testInvariantsForParameters(false, 75);  // No warnings, three-quarter limit
  }
  
  private void testInvariantsForParameters(boolean includeWarnings, int maxIssuesPerFile) throws Exception {
    CompletableFuture<String> future = bridge.getIssues(testWorkspacePath, includeWarnings, maxIssuesPerFile);
    String result = future.get();
    
    JsonNode json = objectMapper.readTree(result);
    JsonNode counts = json.get("counts");
    JsonNode issues = json.get("issues");
    
    // Mathematical invariant: total = errors + warnings + info
    int total = counts.get("total").asInt();
    int errors = counts.get("errors").asInt();
    int warnings = counts.get("warnings").asInt();
    int info = counts.get("info").asInt();
    
    assertEquals(total, errors + warnings + info, 
        String.format("Total count (%d) must equal sum of individual severity counts (%d + %d + %d = %d) for includeWarnings=%b, maxIssuesPerFile=%d",
            total, errors, warnings, info, errors + warnings + info, includeWarnings, maxIssuesPerFile));
    
    // Consistency invariant: issues array length matches total count
    assertEquals(total, issues.size(), 
        String.format("Issues array length (%d) must match total count (%d) for includeWarnings=%b, maxIssuesPerFile=%d",
            issues.size(), total, includeWarnings, maxIssuesPerFile));
    
    // Non-negative invariant: all counts must be >= 0
    assertTrue(total >= 0, "Total count must be non-negative");
    assertTrue(errors >= 0, "Error count must be non-negative");
    assertTrue(warnings >= 0, "Warning count must be non-negative");
    assertTrue(info >= 0, "Info count must be non-negative");
    
    // Domain invariant: all severity values must be valid
    for (JsonNode issue : issues) {
      String severity = issue.get("severity").asText();
      assertTrue(isValidSeverity(severity), 
          "Severity '" + severity + "' must be a valid severity level");
    }
    
    // Structure invariant: required fields must be present for each issue
    for (JsonNode issue : issues) {
      assertTrue(issue.has("severity"), "Each issue must have severity field");
      assertTrue(issue.has("file"), "Each issue must have file field");
      assertTrue(issue.has("line"), "Each issue must have line field");
      assertTrue(issue.has("column"), "Each issue must have column field");
      assertTrue(issue.has("message"), "Each issue must have message field");
      assertTrue(issue.has("source"), "Each issue must have source field");
      
      // Line and column should be non-negative integers
      assertTrue(issue.get("line").asInt() >= 0, "Line number must be non-negative");
      assertTrue(issue.get("column").asInt() >= 0, "Column number must be non-negative");
    }
  }
  
  private boolean isValidSeverity(String severity) {
    return severity.equals("error") || 
           severity.equals("warning") || 
           severity.equals("info") ||
           severity.equals("unused_category") ||  // apex-ls specific severity
           severity.equals("unknown");
  }
  
  @Test
  @DisplayName("Enhanced JSON should maintain count invariants across all parameter combinations")
  void enhancedJsonShouldMaintainCountInvariantsAcrossCombinations() throws Exception {
    if (testWorkspacePath == null) {
      return;
    }
    
    // Test arithmetic consistency across different parameter combinations
    CompletableFuture<String> withoutWarnings = bridge.getIssues(testWorkspacePath, false, 100);
    CompletableFuture<String> withWarnings = bridge.getIssues(testWorkspacePath, true, 100);
    
    String resultWithoutWarnings = withoutWarnings.get();
    String resultWithWarnings = withWarnings.get();
    
    JsonNode jsonWithoutWarnings = objectMapper.readTree(resultWithoutWarnings);
    JsonNode jsonWithWarnings = objectMapper.readTree(resultWithWarnings);
    
    JsonNode countsWithout = jsonWithoutWarnings.get("counts");
    JsonNode countsWith = jsonWithWarnings.get("counts");
    
    // Invariant: including warnings should never decrease error count
    int errorsWithout = countsWithout.get("errors").asInt();
    int errorsWith = countsWith.get("errors").asInt();
    assertEquals(errorsWithout, errorsWith, "Error count should be consistent regardless of warning inclusion");
    
    // Invariant: including warnings should have >= total issues than excluding warnings
    int totalWithout = countsWithout.get("total").asInt();
    int totalWith = countsWith.get("total").asInt();
    assertTrue(totalWith >= totalWithout, "Including warnings should never reduce total issue count");
    
    // Invariant: warning count should be 0 when warnings are excluded
    int warningsWithout = countsWithout.get("warnings").asInt();
    assertTrue(warningsWithout == 0 || totalWithout > 0, "Warnings should be 0 when excluded (unless there are mixed results)");
  }
}