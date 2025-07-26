# CLAUDE.md - MCP Server

This file provides MCP-specific guidance for Claude Code when working in the `mcp/` subdirectory of the Apex Language Server project.

## Project Context

This is the MCP (Model Context Protocol) server subproject within the broader Apex Language Server. For general project guidance, see [../CLAUDE.md](../CLAUDE.md).

For coding guidelines and best practices, see [../GUIDELINES.md](../GUIDELINES.md).

## Essential Commands

### Building
- `sbt buildRegular` - Build regular JAR for Maven Central publishing
- `sbt buildStandalone` - Build standalone JAR with all dependencies for distribution
- `sbt test` - Run MCP-specific tests including system tests
- `sbt javafmt` - Format all Java source files (run before commits)
- `sbt javafmtCheck` - Check if Java files need formatting

### Testing
- `sbt "testOnly io.github.apexdevtools.apexls.mcp.system.*"` - Run system tests only
- `sbt "testOnly io.github.apexdevtools.apexls.mcp.bridge.*"` - Run bridge tests only
- `sbt "testOnly io.github.apexdevtools.apexls.mcp.tools.*"` - Run tool tests only

### Running/Debugging
- `java -jar target/scala-2.13/apex-ls-mcp-*-standalone.jar` - Run MCP server directly
- `npx @modelcontextprotocol/inspector java -jar target/scala-2.13/apex-ls-mcp-*-standalone.jar` - Debug with MCP Inspector

## Architecture Quick Reference

### Key Components
- **MCPServer.java** - Main entry point and MCP protocol implementation
- **bridge/** - Java 17 ↔ Java 8 apex-ls communication layer
- **tools/** - MCP tools (diagnostics, find usages, find definition, impacted tests)
- **resources/** - MCP resources (workspace access)

### Bridge Pattern
- **ApexLsBridge** - Interface for async operations
- **EmbeddedApexLsBridge** - Implementation accessing OrgAPI within same JVM
- Uses CompletableFuture for async operations with Scala Future conversion

## Dependencies

### Build Requirements
- Parent apex-ls project MUST be built first: `cd .. && sbt build`
- Java 17+ for MCP server (uses MCP Java SDK)
- Java 8+ for apex-ls core dependency

### Test Workspace
- Test workspace located at `src/test/resources/test-workspace/`
- Contains sample Apex classes and `sfdx-project.json`
- Used by system tests for full MCP protocol validation

## MCP-Specific Guidelines

### Adding New Tools
1. Extend base MCP tool pattern in `tools/` package
2. Use bridge interface for apex-ls core communication
3. Return JSON strings for MCP protocol compatibility
4. Add comprehensive unit tests and system test coverage

### Bridge Operations
All bridge operations are async and cached per workspace:
- `getIssues()` - Static analysis for diagnostics
- `findUsages()` - Reference finding
- `getDefinition()` - Go-to-definition
- `getTestClassItemsChanged()` - Impacted test discovery
- `getWorkspaceInfo()` - Workspace metadata

## Development Workflow

1. **Make changes** in MCP-specific code
2. **Format code**: `sbt javafmt`
3. **Run tests**: `sbt test`
4. **Build artifacts**: `sbt buildStandalone`
5. **Test with inspector** for MCP protocol validation

## Documentation

For detailed information, see:
- [README.md](README.md) - Installation, usage, and MCP tools reference
- [DEVELOPMENT.md](DEVELOPMENT.md) - Build commands and development workflow
- [ARCHITECTURE.md](ARCHITECTURE.md) - Detailed system design
- [SYSTEM_TESTS.md](SYSTEM_TESTS.md) - System testing documentation