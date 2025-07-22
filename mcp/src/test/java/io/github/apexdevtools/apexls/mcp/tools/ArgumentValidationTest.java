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

package io.github.apexdevtools.apexls.mcp.tools;

import static org.junit.jupiter.api.Assertions.*;

import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests for argument validation in Apex MCP tools. Ensures proper error handling when required
 * arguments are missing or invalid.
 */
class ArgumentValidationTest extends BaseMCPToolTest {

  private ApexFindUsagesTool findUsagesTool;
  private ApexGotoDefinitionTool gotoDefinitionTool;

  @BeforeEach
  void setUpTools() {
    findUsagesTool = new ApexFindUsagesTool(bridge);
    gotoDefinitionTool = new ApexGotoDefinitionTool(bridge);
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject missing path argument")
  void findUsagesToolShouldRejectMissingPathArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("line", 1, "offset", 10);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument is required"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject missing line argument")
  void findUsagesToolShouldRejectMissingLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "offset", 10);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument is required"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject missing offset argument")
  void findUsagesToolShouldRejectMissingOffsetArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 1);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'offset' argument is required"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject empty path argument")
  void findUsagesToolShouldRejectEmptyPathArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "", "line", 1, "offset", 10);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument cannot be empty"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject non-integer line argument")
  void findUsagesToolShouldRejectNonIntegerLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", "invalid", "offset", 10);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' and 'offset' arguments must be integers"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject non-integer offset argument")
  void findUsagesToolShouldRejectNonIntegerOffsetArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 1, "offset", "invalid");
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' and 'offset' arguments must be integers"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject negative line argument")
  void findUsagesToolShouldRejectNegativeLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", -1, "offset", 10);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument must be positive (1-based)"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject zero line argument")
  void findUsagesToolShouldRejectZeroLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 0, "offset", 10);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument must be positive (1-based)"));
  }

  @Test
  @DisplayName("ApexFindUsagesTool should reject negative offset argument")
  void findUsagesToolShouldRejectNegativeOffsetArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 1, "offset", -1);
    CallToolResult result = executeTool(findUsagesTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'offset' argument must be non-negative"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject missing path argument")
  void gotoDefinitionToolShouldRejectMissingPathArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("line", 1, "offset", 10);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument is required"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject missing line argument")
  void gotoDefinitionToolShouldRejectMissingLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "offset", 10);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument is required"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject missing offset argument")
  void gotoDefinitionToolShouldRejectMissingOffsetArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 1);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'offset' argument is required"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject empty path argument")
  void gotoDefinitionToolShouldRejectEmptyPathArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "", "line", 1, "offset", 10);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument cannot be empty"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject non-integer line argument")
  void gotoDefinitionToolShouldRejectNonIntegerLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", "invalid", "offset", 10);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' and 'offset' arguments must be integers"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject negative line argument")
  void gotoDefinitionToolShouldRejectNegativeLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", -1, "offset", 10);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument must be positive (1-based)"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject zero line argument")
  void gotoDefinitionToolShouldRejectZeroLineArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 0, "offset", 10);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument must be positive (1-based)"));
  }

  @Test
  @DisplayName("ApexGotoDefinitionTool should reject negative offset argument")
  void gotoDefinitionToolShouldRejectNegativeOffsetArgument() throws Exception {
    Map<String, Object> args = createArgumentsMap("path", "/some/file.cls", "line", 1, "offset", -1);
    CallToolResult result = executeTool(gotoDefinitionTool, args);

    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'offset' argument must be non-negative"));
  }

  @Test
  @DisplayName("Both tools should accept valid arguments and attempt processing")
  void bothToolsShouldAcceptValidArgumentsAndAttemptProcessing() throws Exception {
    String validPath = testWorkspacePath + "/force-app/main/default/classes/TestClass.cls";
    
    // Test ApexFindUsagesTool with valid arguments
    Map<String, Object> referencesArgs = createArgumentsMap("path", validPath, "line", 1, "offset", 10);
    CallToolResult referencesResult = executeTool(findUsagesTool, referencesArgs);
    assertNotNull(referencesResult);
    // Should not have argument validation errors
    assertFalse(getContentAsString(referencesResult).contains("argument is required"));
    assertFalse(getContentAsString(referencesResult).contains("cannot be empty"));
    assertFalse(getContentAsString(referencesResult).contains("must be integers"));
    assertFalse(getContentAsString(referencesResult).contains("must be non-negative"));

    // Test ApexGotoDefinitionTool with valid arguments  
    Map<String, Object> definitionArgs = createArgumentsMap("path", validPath, "line", 1, "offset", 10);
    CallToolResult definitionResult = executeTool(gotoDefinitionTool, definitionArgs);
    assertNotNull(definitionResult);
    // Should not have argument validation errors
    assertFalse(getContentAsString(definitionResult).contains("argument is required"));
    assertFalse(getContentAsString(definitionResult).contains("cannot be empty"));
    assertFalse(getContentAsString(definitionResult).contains("must be integers"));
    assertFalse(getContentAsString(definitionResult).contains("must be non-negative"));
  }
}