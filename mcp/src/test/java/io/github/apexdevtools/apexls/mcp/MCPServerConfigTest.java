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

import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

public class MCPServerConfigTest {

  /** Test default configuration values when no arguments provided. */
  @Test
  public void testDefaultConfiguration() {
    String[] args = {};
    MCPServerConfig config = MCPServerConfig.fromArgs(args, key -> null);
    
    assertEquals("info", config.getLoggingLevel());
    assertFalse(config.isCacheEnabled());
  }

  /** Test command line argument parsing for logging levels. */
  @Test
  public void testLoggingLevelArguments() {
    // Test all valid logging levels
    String[] validLevels = {"none", "info", "debug", "trace"};
    
    for (String level : validLevels) {
      String[] args = {"--logging=" + level};
      MCPServerConfig config = MCPServerConfig.fromArgs(args, key -> null);
      assertEquals(level, config.getLoggingLevel());
    }
  }

  /** Test cache configuration arguments. */
  @Test
  public void testCacheArguments() {
    // Test cache enabled
    String[] enabledArgs = {"--cache-enabled"};
    MCPServerConfig enabledConfig = MCPServerConfig.fromArgs(enabledArgs, key -> null);
    assertTrue(enabledConfig.isCacheEnabled());
    
    // Test cache disabled
    String[] disabledArgs = {"--cache-disabled"};
    MCPServerConfig disabledConfig = MCPServerConfig.fromArgs(disabledArgs, key -> null);
    assertFalse(disabledConfig.isCacheEnabled());
  }

  /** Test environment variable fallback. */
  @Test
  public void testEnvironmentVariableFallback() {
    Map<String, String> env = new HashMap<>();
    env.put(MCPServerConfig.LOGGING_ENV_VAR, "debug");
    env.put(MCPServerConfig.CACHE_ENV_VAR, "true");
    
    String[] args = {}; // No command line args
    MCPServerConfig config = MCPServerConfig.fromArgs(args, env::get);
    
    assertEquals("debug", config.getLoggingLevel());
    assertTrue(config.isCacheEnabled());
  }

  /** Test command line arguments override environment variables. */
  @Test
  public void testCommandLineOverridesEnvironment() {
    Map<String, String> env = new HashMap<>();
    env.put(MCPServerConfig.LOGGING_ENV_VAR, "debug");
    env.put(MCPServerConfig.CACHE_ENV_VAR, "true");
    
    String[] args = {"--logging=trace", "--cache-disabled"};
    MCPServerConfig config = MCPServerConfig.fromArgs(args, env::get);
    
    assertEquals("trace", config.getLoggingLevel());
    assertFalse(config.isCacheEnabled());
  }

  /** Test invalid logging level throws exception. */
  @Test
  public void testInvalidLoggingLevel() {
    String[] args = {"--logging=invalid"};
    
    assertThrows(IllegalArgumentException.class, () -> {
      MCPServerConfig.fromArgs(args, key -> null);
    });
  }

  /** Test help request throws specific exception. */
  @Test
  public void testHelpRequest() {
    String[] args = {"--help"};
    
    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> {
      MCPServerConfig.fromArgs(args, key -> null);
    });
    
    assertEquals("HELP_REQUESTED", ex.getMessage());
  }

  /** Test getUsage returns non-empty string. */
  @Test
  public void testGetUsage() {
    String usage = MCPServerConfig.getUsage();
    assertNotNull(usage);
    assertFalse(usage.trim().isEmpty());
    assertTrue(usage.contains("Usage:"));
    assertTrue(usage.contains("--logging="));
    assertTrue(usage.contains("--cache-enabled"));
  }

  /** Test configuration toString method. */
  @Test
  public void testToString() {
    MCPServerConfig config = new MCPServerConfig("debug", true);
    String result = config.toString();
    
    assertTrue(result.contains("debug"));
    assertTrue(result.contains("true"));
  }

  /** Test case insensitive logging level parsing. */
  @Test
  public void testCaseInsensitiveLogging() {
    String[] args = {"--logging=DEBUG"};
    MCPServerConfig config = MCPServerConfig.fromArgs(args, key -> null);
    assertEquals("debug", config.getLoggingLevel());
  }
}