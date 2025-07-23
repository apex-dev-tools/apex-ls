# MCP System Tests

This document describes the system tests for the Apex Language Server MCP implementation.

## Overview

The system tests verify that the MCP server can start up correctly and respond to MCP protocol requests. Unlike unit tests that test individual components in isolation, these system tests validate the entire MCP server stack end-to-end.

## Test Structure

### MCPServerSystemTest

Located in `src/test/java/io/github/apexdevtools/apexls/mcp/system/MCPServerSystemTest.java`, this class provides comprehensive system tests using direct MCP protocol communication over STDIO.

**Key Test Cases:**

1. **Server Startup Test** - Verifies the MCP server process starts successfully and accepts the initial MCP protocol handshake
2. **Tool Discovery Test** - Validates that all expected MCP tools are discoverable via the `tools/list` protocol method
3. **Resource Discovery Test** - Confirms that MCP resources are properly exposed via the `resources/list` protocol method  
4. **Tool Execution Test** - Tests end-to-end execution of the `apex_static_analysis` tool with real workspace data

### Test Infrastructure

The system tests:

- Start the MCP server as a separate JVM process using `ProcessBuilder`
- Communicate with the server using the MCP JSON-RPC protocol over STDIO
- Use a real test workspace with sample Apex classes (located in `src/test/resources/test-workspace/`)
- Validate proper MCP protocol compliance including initialization handshake
- Test timeout handling and graceful shutdown

### Alternative Approach Considered

We initially considered using LangChain4j as an MCP test client based on this article: https://glaforge.dev/posts/2025/04/04/mcp-client-and-server-with-java-mcp-sdk-and-langchain4j/

However, we opted for direct MCP protocol testing because:
- It provides more precise control over the test scenarios
- It avoids dependency on external LLM services
- It tests the MCP protocol implementation directly without abstraction layers
- It's more lightweight and focused on our specific MCP server functionality

## Running the Tests

### Prerequisites

1. Ensure the main apex-ls JAR is built:
   ```bash
   cd .. && sbt build
   ```

2. Build the MCP project:
   ```bash
   sbt compile
   ```

### Execution

**Method 1: Direct JUnit Execution (Recommended)**
```bash
java -cp "$(sbt 'export Test/fullClasspath' | tail -n 1)" org.junit.platform.console.ConsoleLauncher --scan-classpath --include-classname=".*MCPServerSystemTest.*"
```

**Method 2: SBT (Known Issue)**
```bash
sbt test
# or for specific tests:
sbt "testOnly io.github.apexdevtools.apexls.mcp.system.*"
```

⚠️ **Note**: There is currently a known issue with SBT/JUnit Jupiter integration where SBT doesn't detect the JUnit 5 tests despite proper configuration with jupiter-interface. The tests themselves work perfectly when run directly (Method 1). This is a common issue with SBT + JUnit Jupiter and doesn't affect the functionality of the tests.

### Test Workspace

The tests use a sample workspace located in `src/test/resources/test-workspace/` which contains:

- `sfdx-project.json` - Standard Salesforce DX project configuration
- `force-app/main/default/classes/TestClass.cls` - Sample Apex class for testing
- `force-app/main/default/classes/AnotherClass.cls` - Additional test class for reference testing
- Associated metadata files (`.cls-meta.xml`)

## Expected Behavior

Successful system tests should demonstrate:

1. **Clean Server Startup** - MCP server starts without errors and becomes ready to accept connections
2. **Protocol Compliance** - Server properly implements MCP protocol handshake and message formats
3. **Tool Registration** - All three expected tools are registered:
   - `apex_static_analysis` - For code analysis and issue detection
   - `apex_find_usages` - For finding symbol usages
   - `apex_find_definition` - For finding symbol definitions
4. **Resource Exposure** - Workspace resources are properly exposed for client access
5. **Functional Tools** - Tools execute successfully and return meaningful results

## Troubleshooting

### Common Issues

**Tests not running**: Ensure JUnit Jupiter and the jupiter-interface are properly configured in `build.sbt`

**Server startup failures**: Check that the apex-ls JAR is available and all dependencies are on the classpath

**Timeout issues**: The tests include generous timeouts (30-60 seconds) to account for workspace analysis time

**Workspace errors**: Verify the test workspace structure is intact in `src/test/resources/test-workspace/`

### Debug Mode

To enable debug logging for the system tests, add this to your test execution:
```bash
sbt -Dorg.slf4j.simpleLogger.defaultLogLevel=debug test
```

## Integration with CI/CD

These system tests are designed to be run in automated CI/CD pipelines and will:
- Fail fast if the MCP server cannot start
- Validate that all expected functionality is working
- Provide clear error messages for debugging
- Run in a reasonable time frame (under 5 minutes for full suite)

The tests are deterministic and should not require external services or network access.