# Architecture Guide

This document describes the code structure, bridge design, and architectural patterns of the MCP Server.

## Java Version Split Design

- **MCP Server**: Java 17 (required by MCP Java SDK)
- **Apex Language Server Core**: Java 8 (parent dependency)
- **Communication**: Bridge pattern using reflection to communicate between Java versions

## Key Components

### MCPServer
Main entry point running MCP protocol via STDIN/STDOUT

### Bridge Architecture
- `ApexLsBridge` interface defines communication contract
- `EmbeddedApexLsBridge` uses reflection to call apex-ls OrgAPI directly
- Workspace caching to avoid expensive re-initialization

### MCP Tools
- `sfdx_code_diagnostics` - Static analysis and error detection
- `apex_find_usages` - Find all references to identifiers
- `apex_find_definition` - Navigate to identifier definitions

### MCP Resources
- `workspace://apex/{workspace_path}` - Workspace metadata and information

## Package Structure

```
io.github.apexdevtools.apexls.mcp/
├── bridge/              # Java version bridge components
│   ├── ApexLsBridge     # Interface for apex-ls communication
│   └── EmbeddedApexLsBridge # Reflection-based implementation
├── tools/               # MCP tool implementations
│   ├── SfdxCodeDiagnosticsTool
│   ├── ApexFindUsagesTool
│   └── ApexFindDefinitionTool
├── resources/           # MCP resource implementations
└── MCPServer           # Main server entry point
```

Tests follow the same package structure in `src/test/java/`.

## Bridge Pattern Details

The bridge architecture solves the Java version compatibility issue:

1. **Interface Definition**: `ApexLsBridge` defines the contract
2. **Reflection Implementation**: `EmbeddedApexLsBridge` uses reflection to call apex-ls methods
3. **Workspace Caching**: Expensive workspace initialization is cached and reused
4. **Error Handling**: Graceful fallback when apex-ls operations fail

## MCP Protocol Integration

The server implements the Model Context Protocol specification:

- **STDIN/STDOUT Communication**: Standard MCP transport
- **Tool Registration**: Dynamic discovery of available analysis tools
- **Resource Registration**: Workspace metadata as MCP resources
- **Error Propagation**: Apex analysis errors surface as MCP errors

## Position Parameter System

The MCP tools use a flexible position parameter system:

- **Line Numbers**: 1-based indexing (first line = 1)
- **Character Offsets**: 0-based indexing within lines
- **Identifier Targeting**: Any position within identifier bounds works
- **Robust Parsing**: Handles various Apex identifier types automatically