# Development Guide

This document covers build commands, testing strategies, and development workflows for the MCP Server.

## Prerequisites

Requires parent apex-ls project to be built first:
```bash
# From apex-ls root directory:
sbt apexlsJVM/packageBin
```

## Build Commands

### MCP Server Build
```bash
cd mcp
sbt build                    # Creates deployable JAR with dependencies
sbt test                     # Run test suite
sbt clean                    # Remove build artifacts
sbt javafmt                  # Format all Java source files
sbt javafmtCheck             # Check if Java files need formatting
```


## Development Workflow

### Running the MCP Server
```bash
# Direct execution:
java -jar target/apex-ls-mcp-0.1.0-SNAPSHOT.jar

# Debug with MCP Inspector:
npx @modelcontextprotocol/inspector java -jar target/apex-ls-mcp-0.1.0-SNAPSHOT.jar
```

### Testing Strategy
- **System Tests**: Full MCP protocol integration via STDIO (`MCPServerSystemTest`)
- **Unit Tests**: Component-level testing for bridge and tools
- **Test Workspace**: Sample Apex project in `src/test/resources/test-workspace/`

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

## Integration Notes

### SFDX Workspace Requirements
- Must contain valid `sfdx-project.json` file
- Compatible with same workspace format as parent apex-ls project
- Supports all Salesforce DX project structures

### Logging Configuration
- Production: Logs to STDERR (avoids STDOUT MCP protocol interference)
- Test: Separate `logback-test.xml` configuration
- Default level: INFO, DEBUG available for troubleshooting

### Relationship to Parent Project
This MCP server is a separate Java 17 subproject within the larger Apex Language Server ecosystem. It depends on the JVM build artifacts of the main apex-ls project and provides MCP protocol access to the core Scala-based analysis engine.
