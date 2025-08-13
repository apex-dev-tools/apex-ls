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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Statically typed tests for IndexerConfig without using reflection.
 */
class IndexerConfigTest {
    
    @Test
    @DisplayName("Should use default values when environment variables are not set")
    void shouldUseDefaultValuesWhenEnvVarsNotSet() {
        // Mock environment with no variables set
        Function<String, String> emptyEnv = var -> null;
        
        IndexerConfig config = new IndexerConfig(emptyEnv);
        
        assertEquals(IndexerConfig.DEFAULT_TRIGGER_MS, config.getTriggerMs());
        assertEquals(IndexerConfig.DEFAULT_QUIET_PERIOD_MS, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should parse valid environment variable values correctly")
    void shouldParseValidEnvVarValuesCorrectly() {
        Map<String, String> env = new HashMap<>();
        env.put(IndexerConfig.TRIGGER_ENV_VAR, "100");
        env.put(IndexerConfig.QUIET_PERIOD_ENV_VAR, "3000");
        
        IndexerConfig config = new IndexerConfig(env::get);
        
        assertEquals(100, config.getTriggerMs());
        assertEquals(3000, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should handle invalid numeric environment variables gracefully")
    void shouldHandleInvalidNumericEnvVarsGracefully() {
        Map<String, String> env = new HashMap<>();
        env.put(IndexerConfig.TRIGGER_ENV_VAR, "not-a-number");
        env.put(IndexerConfig.QUIET_PERIOD_ENV_VAR, "also-invalid");
        
        IndexerConfig config = new IndexerConfig(env::get);
        
        // Should fall back to defaults
        assertEquals(IndexerConfig.DEFAULT_TRIGGER_MS, config.getTriggerMs());
        assertEquals(IndexerConfig.DEFAULT_QUIET_PERIOD_MS, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should handle mixed valid and invalid environment variables")
    void shouldHandleMixedValidAndInvalidEnvVars() {
        Map<String, String> env = new HashMap<>();
        env.put(IndexerConfig.TRIGGER_ENV_VAR, "200");
        env.put(IndexerConfig.QUIET_PERIOD_ENV_VAR, "invalid");
        
        IndexerConfig config = new IndexerConfig(env::get);
        
        assertEquals(200, config.getTriggerMs());
        assertEquals(IndexerConfig.DEFAULT_QUIET_PERIOD_MS, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should handle zero values correctly")
    void shouldHandleZeroValuesCorrectly() {
        Map<String, String> env = new HashMap<>();
        env.put(IndexerConfig.TRIGGER_ENV_VAR, "0");
        env.put(IndexerConfig.QUIET_PERIOD_ENV_VAR, "0");
        
        IndexerConfig config = new IndexerConfig(env::get);
        
        assertEquals(0, config.getTriggerMs());
        assertEquals(0, config.getQuietPeriodMs());
        assertFalse(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should validate valid configuration parameters")
    void shouldValidateValidConfigurationParameters() {
        IndexerConfig config = new IndexerConfig(50, 2000);
        
        assertDoesNotThrow(config::validate);
        assertEquals(50, config.getTriggerMs());
        assertEquals(2000, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should reject negative trigger time")
    void shouldRejectNegativeTriggerTime() {
        IndexerConfig config = new IndexerConfig(-1, 2000);
        
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, 
                                                  config::validate);
        assertTrue(ex.getMessage().contains("trigger time cannot be negative"));
    }
    
    @Test
    @DisplayName("Should reject negative quiet period")
    void shouldRejectNegativeQuietPeriod() {
        IndexerConfig config = new IndexerConfig(50, -1);
        
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, 
                                                  config::validate);
        assertTrue(ex.getMessage().contains("quiet period cannot be negative"));
    }
    
    @Test
    @DisplayName("Should validate edge case configurations")
    void shouldValidateEdgeCaseConfigurations() {
        // Test zero trigger, positive quiet period
        IndexerConfig config1 = new IndexerConfig(0, 1000);
        assertDoesNotThrow(config1::validate);
        assertFalse(config1.isEnabled());
        
        // Test positive trigger, zero quiet period
        IndexerConfig config2 = new IndexerConfig(100, 0);
        assertDoesNotThrow(config2::validate);
        assertFalse(config2.isEnabled());
        
        // Test both zero
        IndexerConfig config3 = new IndexerConfig(0, 0);
        assertDoesNotThrow(config3::validate);
        assertFalse(config3.isEnabled());
    }
    
    @Test
    @DisplayName("Should handle very large values")
    void shouldHandleVeryLargeValues() {
        IndexerConfig config = new IndexerConfig(10000, 20000);
        
        // Should not throw exception, but may log warnings
        assertDoesNotThrow(config::validate);
        assertEquals(10000, config.getTriggerMs());
        assertEquals(20000, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should handle very small positive values")
    void shouldHandleVerySmallPositiveValues() {
        IndexerConfig config = new IndexerConfig(1, 1);
        
        // Should not throw exception, but may log warnings
        assertDoesNotThrow(config::validate);
        assertEquals(1, config.getTriggerMs());
        assertEquals(1, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
    }
    
    @Test
    @DisplayName("Should provide meaningful toString representation")
    void shouldProvideMeaningfulToStringRepresentation() {
        IndexerConfig config = new IndexerConfig(100, 2000);
        String str = config.toString();
        
        assertNotNull(str);
        assertTrue(str.contains("100"));
        assertTrue(str.contains("2000"));
        assertTrue(str.contains("enabled=true"));
    }
    
    @Test
    @DisplayName("Should handle parseEnvLong method correctly")
    void shouldHandleParseEnvLongMethodCorrectly() {
        // Test with valid numeric value
        Function<String, String> validEnv = var -> "123";
        long result1 = IndexerConfig.parseEnvLong(validEnv, "TEST_VAR", 999);
        assertEquals(123, result1);
        
        // Test with null value
        Function<String, String> nullEnv = var -> null;
        long result2 = IndexerConfig.parseEnvLong(nullEnv, "TEST_VAR", 999);
        assertEquals(999, result2);
        
        // Test with invalid value
        Function<String, String> invalidEnv = var -> "not-a-number";
        long result3 = IndexerConfig.parseEnvLong(invalidEnv, "TEST_VAR", 999);
        assertEquals(999, result3);
        
        // Test with empty string
        Function<String, String> emptyEnv = var -> "";
        long result4 = IndexerConfig.parseEnvLong(emptyEnv, "TEST_VAR", 999);
        assertEquals(999, result4);
    }
    
    @Test
    @DisplayName("Should correctly determine enabled state")
    void shouldCorrectlyDetermineEnabledState() {
        assertTrue(new IndexerConfig(1, 1).isEnabled());
        assertTrue(new IndexerConfig(50, 2000).isEnabled());
        assertFalse(new IndexerConfig(0, 0).isEnabled());
        assertFalse(new IndexerConfig(0, 1000).isEnabled());
        assertFalse(new IndexerConfig(50, 0).isEnabled());
    }
    
    @Test
    @DisplayName("Should handle explicit constructor correctly")
    void shouldHandleExplicitConstructorCorrectly() {
        IndexerConfig config = new IndexerConfig(75, 1500);
        
        assertEquals(75, config.getTriggerMs());
        assertEquals(1500, config.getQuietPeriodMs());
        assertTrue(config.isEnabled());
        assertDoesNotThrow(config::validate);
    }
    
    @Test
    @DisplayName("Should access constants correctly")
    void shouldAccessConstantsCorrectly() {
        assertEquals(50, IndexerConfig.DEFAULT_TRIGGER_MS);
        assertEquals(2000, IndexerConfig.DEFAULT_QUIET_PERIOD_MS);
        assertEquals("APEX_INDEXER_TRIGGER_MS", IndexerConfig.TRIGGER_ENV_VAR);
        assertEquals("APEX_INDEXER_QUIET_PERIOD_MS", IndexerConfig.QUIET_PERIOD_ENV_VAR);
    }
}