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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;
import java.util.function.Function;

/**
 * Configuration class for MCP server startup parameters. Handles command line argument parsing,
 * environment variable fallback, and validation.
 */
public class MCPServerConfig {

  private static final Logger logger = LoggerFactory.getLogger(MCPServerConfig.class);

  private final String loggingLevel;
  private final boolean cacheEnabled;

  // Default configuration values
  public static final String DEFAULT_LOGGING = "info";
  public static final boolean DEFAULT_CACHE_ENABLED = false;

  // Environment variable names
  public static final String LOGGING_ENV_VAR = "APEX_MCP_LOGGING";
  public static final String CACHE_ENV_VAR = "APEX_MCP_CACHE_ENABLED";

  // Valid configuration values
  public static final Set<String> VALID_LOGGING_LEVELS = Set.of("none", "info", "debug", "trace");

  /** Create configuration from command line arguments with system environment fallback. */
  public static MCPServerConfig fromArgs(String[] args) {
    return fromArgs(args, System::getenv);
  }

  /** Create configuration from arguments with custom environment provider (for testing). */
  public static MCPServerConfig fromArgs(String[] args, Function<String, String> envProvider) {
    ParseResult parseResult = parseArguments(args);

    if (parseResult.showHelp) {
      throw new IllegalArgumentException("HELP_REQUESTED");
    }

    // Use CLI values or fall back to environment variables
    String finalLogging =
        parseResult.loggingLevel != null
            ? parseResult.loggingLevel
            : getEnvOrDefault(envProvider, LOGGING_ENV_VAR, DEFAULT_LOGGING);

    boolean finalCache =
        parseResult.cacheEnabled != null
            ? parseResult.cacheEnabled
            : Boolean.parseBoolean(
                getEnvOrDefault(envProvider, CACHE_ENV_VAR, String.valueOf(DEFAULT_CACHE_ENABLED)));

    return new MCPServerConfig(finalLogging, finalCache);
  }

  /** Simple data class for parse results. */
  private static class ParseResult {
    final String loggingLevel;
    final Boolean cacheEnabled;
    final boolean showHelp;

    ParseResult(String loggingLevel, Boolean cacheEnabled, boolean showHelp) {
      this.loggingLevel = loggingLevel;
      this.cacheEnabled = cacheEnabled;
      this.showHelp = showHelp;
    }
  }

  /** Parse command line arguments into structured result. */
  private static ParseResult parseArguments(String[] args) {
    String loggingLevel = null;
    Boolean cacheEnabled = null;
    boolean showHelp = false;

    for (String arg : args) {
      if (arg.startsWith("--logging=")) {
        loggingLevel = arg.substring("--logging=".length()).toLowerCase();
      } else if (arg.equals("--cache-enabled")) {
        cacheEnabled = true;
      } else if (arg.equals("--cache-disabled")) {
        cacheEnabled = false;
      } else if (arg.equals("--help") || arg.equals("-h")) {
        showHelp = true;
      } else if (!arg.isEmpty()) {
        logger.warn("Unknown command line argument: {}", arg);
      }
    }

    return new ParseResult(loggingLevel, cacheEnabled, showHelp);
  }

  /** Get environment variable or return default value. */
  private static String getEnvOrDefault(
      Function<String, String> envProvider, String envVar, String defaultValue) {
    String envValue = envProvider.apply(envVar);
    return (envValue != null && !envValue.trim().isEmpty())
        ? envValue.trim().toLowerCase()
        : defaultValue;
  }

  /** Create configuration with explicit values (for testing). */
  public MCPServerConfig(String loggingLevel, boolean cacheEnabled) {
    this.loggingLevel = loggingLevel.toLowerCase();
    this.cacheEnabled = cacheEnabled;
    validate();
  }

  /** Validate configuration and log resolved values. */
  private void validate() {
    // Validate logging level
    if (!VALID_LOGGING_LEVELS.contains(loggingLevel)) {
      throw new IllegalArgumentException(
          String.format(
              "Invalid logging level '%s'. Valid levels: %s", loggingLevel, VALID_LOGGING_LEVELS));
    }

    logger.trace("MCP Server config resolved: logging={}, cache={}", loggingLevel, cacheEnabled);
  }

  /** Generate usage information string. */
  public static String getUsage() {
    return """
      Apex Language Server MCP (Model Context Protocol) Server

      Usage: java -jar apex-ls-mcp.jar [options]

      Options:
        --logging=LEVEL    Set logging level: none, info, debug, trace
                           Default: info
        --cache-enabled    Enable apex-ls caching
        --cache-disabled   Disable apex-ls caching
                           Default: disabled
        --help, -h         Show this help message

      Environment Variables:
        APEX_MCP_LOGGING        Override logging level
        APEX_MCP_CACHE_ENABLED  Override cache setting (true/false)

      Examples:
        java -jar apex-ls-mcp.jar
        java -jar apex-ls-mcp.jar --logging=debug --cache-enabled
        APEX_MCP_LOGGING=trace java -jar apex-ls-mcp.jar""";
  }

  public String getLoggingLevel() {
    return loggingLevel;
  }

  public boolean isCacheEnabled() {
    return cacheEnabled;
  }

  @Override
  public String toString() {
    return String.format("MCPServerConfig{logging='%s', cache=%s}", loggingLevel, cacheEnabled);
  }
}
