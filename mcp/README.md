# Apex Language Server MCP Support

This directory contains the MCP (Model Context Protocol) server implementation for the Apex Language Server. This component provides MCP protocol support for AI tools and applications that want to interact with Apex code analysis capabilities.

## Architecture

The MCP server is built as a separate Java 17 project that depends on the main apex-ls JAR (Java 8). This separation allows us to use the official MCP Java SDK (which requires Java 17) while maintaining backward compatibility for the core apex-ls functionality.

### Components

- **MCPServer** - Main entry point and server implementation
- **Bridge** - Communication layer between Java 17 MCP code and Java 8 apex-ls core
- **Tools** - MCP tools for code analysis operations
- **Resources** - MCP resources for workspace access

### Bridge Architecture

The bridge provides a clean separation between the Java 17 MCP server and the Java 8 apex-ls core:

- **`ApexLsBridge`** - Interface defining async operations (getIssues, findUsages, getDefinition, etc.)
- **`EmbeddedApexLsBridge`** - Implementation that accesses OrgAPI directly within the same JVM
- **Async Communication** - Uses CompletableFuture with Scala Future conversion for non-blocking operations
- **Workspace Caching** - OrgAPI instances are cached per workspace to avoid expensive initialization
- **JSON Serialization** - Results are converted to JSON strings for MCP protocol compatibility

**Bridge Operations:**
- Static analysis (`getIssues`) - Used by SfdxCodeDiagnosticsTool
- Find usages (`findUsages`) - Used by ApexFindUsagesTool  
- Go to definition (`getDefinition`) - Used by ApexFindDefinitionTool
- Test discovery (`getTestClassItemsChanged`) - Used by ApexFindImpactedTestsTool
- Workspace metadata (`getWorkspaceInfo`) - Used by WorkspaceResource

## Installation

### For End Users (Recommended)

Download the standalone JAR from [GitHub Releases](https://github.com/apex-dev-tools/apex-ls/releases):

```bash
curl -L -o apex-ls-mcp.jar \
  "https://github.com/apex-dev-tools/apex-ls/releases/latest/download/apex-ls-mcp-standalone.jar"
java -jar apex-ls-mcp.jar
```

### For Developers

Add as Maven dependency:

```xml
<dependency>
  <groupId>io.github.apex-dev-tools</groupId>
  <artifactId>apex-ls-mcp</artifactId>
  <version>5.9.0</version>
</dependency>
```

### IDE Integration

1. Download standalone JAR (Option 1 above)
2. Configure your IDE:

**VS Code** (`.vscode/mcp.json`):
```json
{
  "apex-ls-mcp": {
    "command": "java",
    "args": ["-jar", "/path/to/apex-ls-mcp.jar"]
  }
}
```

**Claude Desktop**:
```json
{
  "mcpServers": {
    "apex-ls": {
      "command": "java", 
      "args": ["-jar", "/path/to/apex-ls-mcp.jar"]
    }
  }
}
```

## Building from Source

From the apex-ls root directory:

```bash
# Build the main apex-ls project first
sbt build

# Build MCP server (regular JAR for Maven Central)
cd mcp && sbt buildRegular

# Build standalone JAR for distribution
cd mcp && sbt buildStandalone
```

This creates:
- `target/scala-2.13/apex-ls-mcp-*-standalone.jar` - Standalone JAR with all dependencies
- `target/scala-2.13/apex-ls-mcp_*.jar` - Regular JAR for Maven dependencies

## Testing

The MCP server communicates via stdin/stdout using the MCP protocol. 
It can be tested with:

```bash
npx @modelcontextprotocol/inspector java -jar apex-ls-mcp-standalone.jar
```

## MCP Tools

### sfdx_code_diagnostics
Analyzes SFDX projects for code issues, errors, and warnings across all Salesforce development artifacts.

**Parameters:**
- `workspace` (required) - Path to the SFDX workspace directory
- `includeWarnings` (optional) - Include warning-level issues in results (default: false)
- `maxIssuesPerFile` (optional) - Maximum number of issues to return per file (default: 100, minimum: 1)

### apex_find_usages
Locate all references to any Apex identifier across the workspace.

**Parameters:**
- `path` (required) - Path to the Apex file
- `line` (required) - Line number (1-based)
- `offset` (required) - Character offset within the line

### apex_find_definition
Find the definition location for Apex types, members, variables, and platform objects.

**Parameters:**
- `path` (required) - Path to the Apex file
- `line` (required) - Line number (1-based)
- `offset` (required) - Character offset within the line

### apex_find_impacted_tests
Find test classes that should be run based on changes to specific Apex source files.

**Parameters:**
- `changed_paths` (required) - Array of file paths that have been changed

## MCP Resources

### workspace://apex/{workspace_path}
Provides access to Apex workspace information and metadata.

Returns JSON with workspace details including version and status information.

## Requirements

- Java 17+ (for MCP server)
- Java 8+ (for apex-ls core dependency)
- Valid Apex workspace with `sfdx-project.json`

## Development

The MCP implementation is designed to be extended with additional tools and resources. New tools should:

1. Implement the MCP tool specification pattern
2. Use the bridge interface to communicate with apex-ls core
3. Return results as JSON strings for MCP protocol compatibility

## Integration

This MCP server can be integrated with AI tools and applications that support the Model Context Protocol. The server provides standardized access to Apex code analysis capabilities through the MCP protocol.
