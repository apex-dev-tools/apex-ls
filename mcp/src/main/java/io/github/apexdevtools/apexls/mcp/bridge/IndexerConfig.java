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

import java.util.function.Function;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Configuration class for indexer timing parameters. Handles environment variable parsing and
 * validation in a testable way.
 */
public class IndexerConfig {

  private static final Logger logger = LoggerFactory.getLogger(IndexerConfig.class);

  private final long triggerMs;
  private final long quietPeriodMs;

  // Default configuration
  public static final long DEFAULT_TRIGGER_MS = 50;
  public static final long DEFAULT_QUIET_PERIOD_MS = 2000;

  // Environment variable names
  public static final String TRIGGER_ENV_VAR = "APEX_INDEXER_TRIGGER_MS";
  public static final String QUIET_PERIOD_ENV_VAR = "APEX_INDEXER_QUIET_PERIOD_MS";

  /** Create configuration using system environment variables. */
  public IndexerConfig() {
    this(System::getenv);
  }

  /** Create configuration with custom environment variable provider for testing. */
  public IndexerConfig(Function<String, String> envProvider) {
    this.triggerMs = parseEnvLong(envProvider, TRIGGER_ENV_VAR, DEFAULT_TRIGGER_MS);
    this.quietPeriodMs = parseEnvLong(envProvider, QUIET_PERIOD_ENV_VAR, DEFAULT_QUIET_PERIOD_MS);
  }

  /** Create configuration with explicit values (for testing). */
  public IndexerConfig(long triggerMs, long quietPeriodMs) {
    this.triggerMs = triggerMs;
    this.quietPeriodMs = quietPeriodMs;
  }

  /** Safely parse a long value from environment variable with fallback to default. */
  static long parseEnvLong(Function<String, String> envProvider, String envVar, long defaultValue) {
    try {
      String value = envProvider.apply(envVar);
      return value != null ? Long.parseLong(value) : defaultValue;
    } catch (NumberFormatException ex) {
      logger.warn(
          "Invalid value for environment variable {}: '{}', using default: {}",
          envVar,
          envProvider.apply(envVar),
          defaultValue);
      return defaultValue;
    }
  }

  /**
   * Validate the configuration parameters and log warnings for potentially problematic values.
   *
   * @throws IllegalArgumentException if parameters are completely invalid
   */
  public void validate() throws IllegalArgumentException {
    // Validate trigger time
    if (triggerMs < 0) {
      throw new IllegalArgumentException(
          "Indexer trigger time cannot be negative: " + triggerMs + "ms");
    }
    if (triggerMs > 5000) {
      logger.warn(
          "Indexer trigger time is very high ({}ms), filesystem changes may be slow to detect",
          triggerMs);
    }
    if (triggerMs > 0 && triggerMs < 10) {
      logger.warn(
          "Indexer trigger time is very low ({}ms), may cause excessive CPU usage on busy filesystems",
          triggerMs);
    }

    // Validate quiet period
    if (quietPeriodMs < 0) {
      throw new IllegalArgumentException(
          "Indexer quiet period cannot be negative: " + quietPeriodMs + "ms");
    }
    if (quietPeriodMs > 10000) {
      logger.warn(
          "Indexer quiet period is very high ({}ms), changes may take a long time to be detected",
          quietPeriodMs);
    }

    // Validate combination
    if (triggerMs > 0 && quietPeriodMs == 0) {
      logger.warn(
          "Indexer trigger time is set but quiet period is 0, filesystem monitoring may not work correctly");
    }
    if (triggerMs == 0 && quietPeriodMs > 0) {
      logger.warn(
          "Indexer quiet period is set but trigger time is 0, filesystem monitoring is effectively disabled");
    }

    // Log configuration for debugging
    if (triggerMs == 0 && quietPeriodMs == 0) {
      logger.info("Filesystem monitoring is disabled (trigger=0, quiet=0)");
    } else {
      logger.debug("Indexer configuration: trigger={}ms, quiet={}ms", triggerMs, quietPeriodMs);
    }
  }

  public long getTriggerMs() {
    return triggerMs;
  }

  public long getQuietPeriodMs() {
    return quietPeriodMs;
  }

  /** Check if filesystem monitoring is enabled. */
  public boolean isEnabled() {
    return triggerMs > 0 && quietPeriodMs > 0;
  }

  @Override
  public String toString() {
    return String.format(
        "IndexerConfig{trigger=%dms, quiet=%dms, enabled=%s}",
        triggerMs, quietPeriodMs, isEnabled());
  }
}
