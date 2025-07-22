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
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/** Tests for the ArgumentValidator helper class. */
class ArgumentValidatorTest {

  @Test
  @DisplayName("Should validate valid arguments successfully")
  void shouldValidateValidArgumentsSuccessfully() {
    Map<String, Object> args = createArguments("/path/to/file.cls", 5, 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNull(result);
  }

  @Test
  @DisplayName("Should reject missing path argument")
  void shouldRejectMissingPathArgument() {
    Map<String, Object> args = createArguments(null, 5, 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument is required"));
  }

  @Test
  @DisplayName("Should reject missing line argument")
  void shouldRejectMissingLineArgument() {
    Map<String, Object> args = new HashMap<>();
    args.put("path", "/path/to/file.cls");
    args.put("offset", 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument is required"));
  }

  @Test
  @DisplayName("Should reject missing offset argument")
  void shouldRejectMissingOffsetArgument() {
    Map<String, Object> args = new HashMap<>();
    args.put("path", "/path/to/file.cls");
    args.put("line", 5);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'offset' argument is required"));
  }

  @Test
  @DisplayName("Should reject empty path argument")
  void shouldRejectEmptyPathArgument() {
    Map<String, Object> args = createArguments("", 5, 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument cannot be empty"));
  }

  @Test
  @DisplayName("Should reject whitespace-only path argument")
  void shouldRejectWhitespaceOnlyPathArgument() {
    Map<String, Object> args = createArguments("   ", 5, 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'path' argument cannot be empty"));
  }

  @Test
  @DisplayName("Should reject non-integer line argument")
  void shouldRejectNonIntegerLineArgument() {
    Map<String, Object> args = createArguments("/path/to/file.cls", "invalid", 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' and 'offset' arguments must be integers"));
  }

  @Test
  @DisplayName("Should reject non-integer offset argument")
  void shouldRejectNonIntegerOffsetArgument() {
    Map<String, Object> args = createArguments("/path/to/file.cls", 5, "invalid");
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' and 'offset' arguments must be integers"));
  }

  @Test
  @DisplayName("Should reject negative line argument")
  void shouldRejectNegativeLineArgument() {
    Map<String, Object> args = createArguments("/path/to/file.cls", -1, 10);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'line' argument must be positive (1-based)"));
  }

  @Test
  @DisplayName("Should reject negative offset argument")
  void shouldRejectNegativeOffsetArgument() {
    Map<String, Object> args = createArguments("/path/to/file.cls", 5, -1);
    
    CallToolResult result = ArgumentValidator.validateApexToolArguments(args);
    
    assertNotNull(result);
    assertTrue(result.isError());
    assertTrue(getContentAsString(result).contains("'offset' argument must be non-negative"));
  }

  @Test
  @DisplayName("Should accept zero value for offset but reject zero for line")
  void shouldAcceptZeroOffsetButRejectZeroLine() {
    // Test that zero offset is valid
    Map<String, Object> validArgs = createArguments("/path/to/file.cls", 1, 0);
    CallToolResult validResult = ArgumentValidator.validateApexToolArguments(validArgs);
    assertNull(validResult);
    
    // Test that zero line is invalid
    Map<String, Object> invalidArgs = createArguments("/path/to/file.cls", 0, 0);
    CallToolResult invalidResult = ArgumentValidator.validateApexToolArguments(invalidArgs);
    assertNotNull(invalidResult);
    assertTrue(getContentAsString(invalidResult).contains("'line' argument must be positive (1-based)"));
  }

  @Test
  @DisplayName("Should extract validated arguments correctly")
  void shouldExtractValidatedArgumentsCorrectly() {
    String expectedPath = "/path/to/TestClass.cls";
    int expectedLine = 42;
    int expectedOffset = 15;
    Map<String, Object> args = createArguments(expectedPath, expectedLine, expectedOffset);
    
    ArgumentValidator.ValidatedArguments result = ArgumentValidator.extractValidatedArguments(args);
    
    assertNotNull(result);
    assertEquals(expectedPath, result.path);
    assertEquals(expectedLine, result.line);
    assertEquals(expectedOffset, result.offset);
  }

  private Map<String, Object> createArguments(Object path, Object line, Object offset) {
    Map<String, Object> args = new HashMap<>();
    if (path != null) {
      args.put("path", path);
    }
    if (line != null) {
      args.put("line", line);
    }
    if (offset != null) {
      args.put("offset", offset);
    }
    return args;
  }

  private String getContentAsString(CallToolResult result) {
    if (result.content() == null || result.content().isEmpty()) {
      return "";
    }
    var content = result.content().get(0);
    return content.toString();
  }
}