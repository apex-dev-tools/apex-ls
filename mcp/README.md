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

The bridge uses reflection to communicate with the apex-ls core:
- `EmbeddedApexLsBridge` - Embedded bridge that calls OrgAPI directly via reflection
- `ApexLsBridge` - Interface defining bridge operations
- Communication is done through JSON strings to avoid class compatibility issues

## Building

From the apex-ls root directory:

```bash
# Build the main apex-ls project first
sbt apexlsJVM/packageBin

# Build the MCP server
cd mcp && sbt build
```

This creates:
- `target/apex-ls-mcp-0.1.0-SNAPSHOT.jar` - The MCP server JAR
- `target/apex-ls_*.jar` - Copy of the apex-ls JAR dependency
- `target/lib/` - All runtime dependencies

## Running

The MCP server communicates via stdin/stdout using the MCP protocol:

```bash
cd mcp/target
java -jar apex-ls-mcp-0.1.0-SNAPSHOT.jar
```

The JAR includes a proper manifest with all dependencies in the classpath, so no additional setup is required.

## MCP Tools

### apex_static_analysis
Performs static analysis on Apex code to find errors, warnings, and unused code.

**Parameters:**
- `workspace` (required) - Path to the Apex workspace directory
- `includeWarnings` (optional) - Include warning-level issues
- `includeUnused` (optional) - Include unused code analysis

### apex_find_references
Finds all references to an identifier at a specific location.

**Parameters:**
- `workspace` (required) - Path to the Apex workspace directory  
- `path` (required) - File path within the workspace
- `line` (required) - Line number (1-based)
- `offset` (required) - Character offset within the line

### apex_goto_definition
Gets the definition location for an identifier (placeholder - not yet implemented).

**Parameters:**
- `workspace` (required) - Path to the Apex workspace directory
- `path` (required) - File path within the workspace  
- `line` (required) - Line number (1-based)
- `offset` (required) - Character offset within the line

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