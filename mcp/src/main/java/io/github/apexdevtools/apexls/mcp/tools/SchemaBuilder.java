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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Utility class for building JSON schemas for MCP tools in a maintainable way. Replaces hardcoded
 * JSON strings with programmatic schema construction.
 */
public class SchemaBuilder {

  private static final ObjectMapper objectMapper = new ObjectMapper();

  /**
   * Creates a standard Apex position schema used by find definition and find usages tools. Includes
   * path, line (1-based), and offset (0-based) parameters.
   */
  public static String createApexPositionSchema() {
    try {
      ObjectNode schema = objectMapper.createObjectNode();
      schema.put("type", "object");

      ObjectNode properties = objectMapper.createObjectNode();

      ObjectNode pathProp = objectMapper.createObjectNode();
      pathProp.put("type", "string");
      pathProp.put("description", "Path to the Apex file");
      properties.set("path", pathProp);

      ObjectNode lineProp = objectMapper.createObjectNode();
      lineProp.put("type", "integer");
      lineProp.put("description", "Line number (1-based) where the identifier is located");
      properties.set("line", lineProp);

      ObjectNode offsetProp = objectMapper.createObjectNode();
      offsetProp.put("type", "integer");
      offsetProp.put(
          "description",
          "Character offset within the line (0-based) that points to any position within the target identifier (class name, method name, variable name, etc.). The position does not need to be at the start of the identifier - any character within the identifier will work.");
      properties.set("offset", offsetProp);

      schema.set("properties", properties);

      ArrayNode required = objectMapper.createArrayNode();
      required.add("path");
      required.add("line");
      required.add("offset");
      schema.set("required", required);

      return objectMapper.writeValueAsString(schema);
    } catch (Exception ex) {
      throw new RuntimeException("Failed to create Apex position schema", ex);
    }
  }

  /** Creates a workspace schema with optional parameters for diagnostics tools. */
  public static String createWorkspaceSchema() {
    try {
      ObjectNode schema = objectMapper.createObjectNode();
      schema.put("type", "object");

      ObjectNode properties = objectMapper.createObjectNode();

      ObjectNode workspaceProp = objectMapper.createObjectNode();
      workspaceProp.put("type", "string");
      workspaceProp.put("description", "Path to the SFDX workspace directory");
      properties.set("workspace", workspaceProp);

      ObjectNode includeWarningsProp = objectMapper.createObjectNode();
      includeWarningsProp.put("type", "boolean");
      includeWarningsProp.put("description", "Include warning-level issues in results");
      includeWarningsProp.put("default", false);
      properties.set("includeWarnings", includeWarningsProp);

      ObjectNode maxIssuesProp = objectMapper.createObjectNode();
      maxIssuesProp.put("type", "integer");
      maxIssuesProp.put("description", "Maximum number of issues to return per file");
      maxIssuesProp.put("default", 100);
      maxIssuesProp.put("minimum", 1);
      properties.set("maxIssuesPerFile", maxIssuesProp);

      schema.set("properties", properties);

      ArrayNode required = objectMapper.createArrayNode();
      required.add("workspace");
      schema.set("required", required);

      return objectMapper.writeValueAsString(schema);
    } catch (Exception ex) {
      throw new RuntimeException("Failed to create workspace schema", ex);
    }
  }

  /** Creates a schema for impacted tests tool that takes changed file paths. */
  public static String createImpactedTestsSchema() {
    try {
      ObjectNode schema = objectMapper.createObjectNode();
      schema.put("type", "object");

      ObjectNode properties = objectMapper.createObjectNode();

      ObjectNode changedPathsProp = objectMapper.createObjectNode();
      changedPathsProp.put("type", "array");
      changedPathsProp.put("description", "Array of file paths that have been changed");
      ObjectNode itemsProp = objectMapper.createObjectNode();
      itemsProp.put("type", "string");
      changedPathsProp.set("items", itemsProp);
      properties.set("changed_paths", changedPathsProp);

      schema.set("properties", properties);

      ArrayNode required = objectMapper.createArrayNode();
      required.add("changed_paths");
      schema.set("required", required);

      return objectMapper.writeValueAsString(schema);
    } catch (Exception ex) {
      throw new RuntimeException("Failed to create impacted tests schema", ex);
    }
  }
}
