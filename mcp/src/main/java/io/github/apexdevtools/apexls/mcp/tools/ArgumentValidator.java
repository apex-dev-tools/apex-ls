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

import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import java.util.Map;

/**
 * Helper class for validating common MCP tool arguments. Provides reusable validation logic for
 * Apex analysis tools.
 */
public class ArgumentValidator {

  /**
   * Validates the standard Apex tool arguments: path, line, and offset.
   *
   * @param arguments the tool arguments to validate
   * @return null if validation passes, or a CallToolResult with error details if validation fails
   */
  public static CallToolResult validateApexToolArguments(Map<String, Object> arguments) {
    // Validate required arguments
    if (arguments.get("path") == null) {
      return new CallToolResult("Error: 'path' argument is required", true);
    }
    if (arguments.get("line") == null) {
      return new CallToolResult("Error: 'line' argument is required", true);
    }
    if (arguments.get("offset") == null) {
      return new CallToolResult("Error: 'offset' argument is required", true);
    }

    String path = (String) arguments.get("path");
    if (path.trim().isEmpty()) {
      return new CallToolResult("Error: 'path' argument cannot be empty", true);
    }

    int line;
    int offset;
    try {
      line = ((Number) arguments.get("line")).intValue();
      offset = ((Number) arguments.get("offset")).intValue();
    } catch (ClassCastException ex) {
      return new CallToolResult("Error: 'line' and 'offset' arguments must be integers", true);
    }

    if (line < 0) {
      return new CallToolResult("Error: 'line' argument must be non-negative", true);
    }
    if (offset < 0) {
      return new CallToolResult("Error: 'offset' argument must be non-negative", true);
    }

    return null; // Validation passed
  }

  /**
   * Extracts validated arguments from the argument map. Should only be called after
   * validateApexToolArguments() returns null.
   *
   * @param arguments the validated argument map
   * @return ValidatedArguments containing the extracted values
   */
  public static ValidatedArguments extractValidatedArguments(Map<String, Object> arguments) {
    String path = (String) arguments.get("path");
    int line = ((Number) arguments.get("line")).intValue();
    int offset = ((Number) arguments.get("offset")).intValue();
    return new ValidatedArguments(path, line, offset);
  }

  /** Container for validated Apex tool arguments. */
  public static class ValidatedArguments {
    public final String path;
    public final int line;
    public final int offset;

    public ValidatedArguments(String path, int line, int offset) {
      this.path = path;
      this.line = line;
      this.offset = offset;
    }
  }
}
