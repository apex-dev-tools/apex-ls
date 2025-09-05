# CLAUDE.md - MCP Server

This file provides MCP-specific guidance for Claude Code when working in the `mcp/` subdirectory of the Apex Language Server project.

## Project Context

This is the MCP (Model Context Protocol) server subproject within the broader Apex Language Server. For general project guidance, see [../CLAUDE.md](../CLAUDE.md).

## Architecture Overview

### Java Version Split Design
- **MCP Server**: Java 17+ (required by MCP Java SDK)
- **Apex Language Server Core**: Java 8+ (parent dependency)
- **Communication**: Bridge pattern using reflection for version compatibility

### Key Components
- **MCPServer.java** - Main entry point and MCP protocol implementation
- **bridge/** - Java 17 ↔ Java 8 apex-ls communication layer
- **tools/** - MCP tools (diagnostics, find usages, find definition, impacted tests)
- **resources/** - MCP resources (workspace access)

### Bridge Architecture
- **ApexLsBridge** - Interface defining async operations contract
- **EmbeddedApexLsBridge** - Reflection-based implementation accessing OrgAPI
- **Workspace caching** - Expensive OrgAPI instances cached per workspace
- **Async operations** - Uses CompletableFuture with Scala Future conversion

### MCP Tools Provided
- `sfdx_code_diagnostics` - Comprehensive static analysis across all Salesforce artifacts
- `apex_find_usages` - Find all references to Apex identifiers workspace-wide
- `apex_find_definition` - Navigate to definition of types, members, variables, platform objects
- `apex_find_impacted_tests` - Identify test classes affected by code changes

### MCP Resources
- `workspace://apex/{workspace_path}` - Access to workspace metadata and configuration

## Package Structure

```
io.github.apexdevtools.apexls.mcp/
├── bridge/                    # Java version bridge components
│   ├── ApexLsBridge          # Interface for apex-ls communication
│   ├── EmbeddedApexLsBridge  # Reflection-based implementation
│   └── IndexerConfig         # Configuration handling
├── tools/                     # MCP tool implementations
│   ├── SfdxCodeDiagnosticsTool
│   ├── ApexFindUsagesTool
│   ├── ApexFindDefinitionTool
│   ├── ApexFindImpactedTestsTool
│   └── ArgumentValidator      # Common validation logic
├── resources/                 # MCP resource implementations
│   └── WorkspaceResource
├── MCPServer                  # Main server entry point
└── MCPServerConfig           # Configuration management
```

## Essential Commands

### Prerequisites
Parent apex-ls project MUST be built first:
```bash
# Option 1: Full build (recommended)
cd .. && sbt build

# Option 2: Package binary only
cd .. && sbt apexlsJVM/packageBin
```

### Building
- `sbt build` - Creates deployable JAR with dependencies (alias for buildStandalone)
- `sbt buildRegular` - Build regular JAR for Maven Central publishing
- `sbt buildStandalone` - Build standalone JAR with all dependencies for distribution
- `sbt clean` - Remove build artifacts
- `sbt javafmt` - Format all Java source files (run before commits)
- `sbt javafmtCheck` - Check if Java files need formatting

### Testing
- `sbt test` - Run full MCP test suite including system tests
- `sbt "testOnly io.github.apexdevtools.apexls.mcp.system.*"` - Run system tests only
- `sbt "testOnly io.github.apexdevtools.apexls.mcp.bridge.*"` - Run bridge tests only
- `sbt "testOnly io.github.apexdevtools.apexls.mcp.tools.*"` - Run tool tests only

### Running/Debugging
- `java -jar target/scala-2.13/apex-ls-mcp-*-standalone.jar` - Run MCP server directly
- `java -jar target/apex-ls-mcp-0.1.0-SNAPSHOT.jar` - Alternative specific version example
- `npx @modelcontextprotocol/inspector java -jar target/scala-2.13/apex-ls-mcp-*-standalone.jar` - Debug with MCP Inspector
- `npx @modelcontextprotocol/inspector java -jar target/apex-ls-mcp-0.1.0-SNAPSHOT.jar` - Debug specific version

## Development Workflow

1. **Prerequisites**: Ensure parent apex-ls is built (`cd .. && sbt build`)
2. **Make changes** in MCP-specific code
3. **Format code**: `sbt javafmt`
4. **Run tests**: `sbt test`
5. **Build artifacts**: `sbt buildStandalone`
6. **Test with inspector** for MCP protocol validation

## Bridge Operations

All bridge operations are async and cached per workspace:
- `getIssues()` - Static analysis for diagnostics
- `findUsages()` - Reference finding
- `getDefinition()` - Go-to-definition
- `getTestClassItemsChanged()` - Impacted test discovery
- `getWorkspaceInfo()` - Workspace metadata

## Testing Strategy

### System Tests
System tests verify that the MCP server can start up correctly and respond to MCP protocol requests. Unlike unit tests that test individual components in isolation, these system tests validate the entire MCP server stack end-to-end.

#### MCPServerSystemTest
Located in `src/test/java/io/github/apexdevtools/apexls/mcp/system/MCPServerSystemTest.java`, this class provides comprehensive system tests using direct MCP protocol communication over STDIO.

**Key Test Cases:**
1. **Server Startup Test** - Verifies the MCP server process starts successfully and accepts the initial MCP protocol handshake
2. **Tool Discovery Test** - Validates that all expected MCP tools are discoverable via the `tools/list` protocol method
3. **Resource Discovery Test** - Confirms that MCP resources are properly exposed via the `resources/list` protocol method  
4. **Tool Execution Test** - Tests end-to-end execution of the `sfdx_code_diagnostics` tool with real workspace data

#### System Test Infrastructure
The system tests:
- Start the MCP server as a separate JVM process using `ProcessBuilder`
- Communicate with the server using the MCP JSON-RPC protocol over STDIO
- Use a real test workspace with sample Apex classes (located in `src/test/resources/test-workspace/`)
- Validate proper MCP protocol compliance including initialization handshake
- Test timeout handling and graceful shutdown

#### Test Workspace Structure
The tests use a sample workspace located in `src/test/resources/test-workspace/` which contains:
- `sfdx-project.json` - Standard Salesforce DX project configuration
- `force-app/main/default/classes/TestClass.cls` - Sample Apex class for testing
- `force-app/main/default/classes/AnotherClass.cls` - Additional test class for reference testing
- Associated metadata files (`.cls-meta.xml`)

#### Expected System Test Behavior
Successful system tests should demonstrate:
1. **Clean Server Startup** - MCP server starts without errors and becomes ready to accept connections
2. **Protocol Compliance** - Server properly implements MCP protocol handshake and message formats
3. **Tool Registration** - All four expected tools are registered:
   - `sfdx_code_diagnostics` - For code analysis and issue detection
   - `apex_find_usages` - For finding symbol usages
   - `apex_find_definition` - For finding symbol definitions
   - `apex_find_impacted_tests` - For finding tests affected by code changes
4. **Resource Exposure** - Workspace resources are properly exposed for client access
5. **Functional Tools** - Tools execute successfully and return meaningful results

### Unit Tests
- Component-level testing for bridge and tools
- Property-based testing with invariant validation
- Tests follow same package structure as main code

## MCP Protocol Integration

The server implements the Model Context Protocol specification:
- **STDIN/STDOUT Communication**: Standard MCP transport
- **Tool Registration**: Dynamic discovery of available analysis tools
- **Resource Registration**: Workspace metadata as MCP resources
- **Error Propagation**: Apex analysis errors surface as MCP errors

### Position Parameter System
- **Line Numbers**: 1-based indexing (first line = 1)
- **Character Offsets**: 0-based indexing within lines
- **Identifier Targeting**: Any position within identifier bounds works
- **Robust Parsing**: Handles various Apex identifier types automatically

## Key Dependencies

### Runtime Dependencies
- MCP Java SDK 0.10.0 (official Model Context Protocol implementation)
- Jackson for JSON processing
- SLF4J + Logback for logging
- Parent apex-ls JAR for core Apex analysis

### Build System
- SBT 1.11.3 with Scala 2.13.16
- JUnit Jupiter 5.11.4 for testing
- Assembly plugin for creating fat JARs

## Workspace Requirements

### SFDX Compatibility
- Must contain valid `sfdx-project.json` file
- Compatible with same workspace format as parent apex-ls project
- Supports all Salesforce DX project structures
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var

### Logging Configuration
- **Production**: Logs to STDERR (avoids STDOUT MCP protocol interference)
- **Test**: Separate `logback-test.xml` configuration
- **Default level**: INFO, DEBUG available for troubleshooting

## MCP-Specific Guidelines

### Adding New Tools
1. Extend base MCP tool pattern in `tools/` package
2. Use bridge interface for apex-ls core communication
3. Return JSON strings for MCP protocol compatibility
4. Add comprehensive unit tests and system test coverage

### Error Handling
- Graceful fallback when apex-ls operations fail
- Bridge errors surface as MCP tool errors
- Workspace validation before operations

## Integration Notes

### SFDX Workspace Requirements
- Must contain valid `sfdx-project.json` file
- Compatible with same workspace format as parent apex-ls project
- Supports all Salesforce DX project structures

### Build System Integration
- SBT 1.11.3 with Scala 2.13.16
- JUnit Jupiter 5.11.4 for testing
- Assembly plugin for creating fat JARs

## Relationship to Parent Project

This MCP server is a separate Java 17 subproject within the larger Apex Language Server ecosystem. It depends on the JVM build artifacts of the main apex-ls project and provides MCP protocol access to the core Scala-based analysis engine.

## System Test Troubleshooting

### Common Issues
- **Tests not running**: Ensure JUnit Jupiter and the jupiter-interface are properly configured in `build.sbt`
- **Server startup failures**: Check that the apex-ls JAR is available and all dependencies are on the classpath
- **Timeout issues**: The tests include generous timeouts (30-60 seconds) to account for workspace analysis time
- **Workspace errors**: Verify the test workspace structure is intact in `src/test/resources/test-workspace/`

### Debug Mode
To enable debug logging for the system tests:
```bash
sbt -Dorg.slf4j.simpleLogger.defaultLogLevel=debug test
```

### CI/CD Integration
These system tests are designed to be run in automated CI/CD pipelines and will:
- Fail fast if the MCP server cannot start
- Validate that all expected functionality is working
- Provide clear error messages for debugging
- Run in a reasonable time frame (under 5 minutes for full suite)
- The tests are deterministic and should not require external services or network access

## Alternative Testing Approaches Considered

We initially considered using LangChain4j as an MCP test client based on this article: https://glaforge.dev/posts/2025/04/04/mcp-client-and-server-with-java-mcp-sdk-and-langchain4j/

However, we opted for direct MCP protocol testing because:
- It provides more precise control over the test scenarios
- It avoids dependency on external LLM services
- It tests the MCP protocol implementation directly without abstraction layers
- It's more lightweight and focused on our specific MCP server functionality

## Documentation Reference

For additional information, see:
- [README.md](../README.md) - Installation, usage, and MCP tools reference