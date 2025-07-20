# Apex Language Server MCP Support

This is a separate Java 17 project that provides MCP (Model Context Protocol) support for the Apex Language Server.

## Architecture

- **Java Version**: Java 17 (required by MCP Java SDK)
- **Dependencies**: Depends on main apex-ls JAR (Java 8)
- **Communication**: Bridge pattern to communicate with apex-ls core
- **Deployment**: Separate JAR (apex-ls-mcp.jar)

## Project Structure

```
mcp/
├── build.sbt                          # SBT build configuration (Java 17)
├── src/main/java/                     # MCP server implementation
│   └── io/github/apexdevtools/apexls/mcp/
│       ├── MCPServer.java             # Main entry point
│       ├── bridge/
│       │   └── ApexLsBridge.java      # Bridge interface
│       ├── tools/                     # MCP tool implementations
│       └── resources/                 # MCP resource providers
└── src/test/java/                     # Tests
```

## Building

From the mcp directory:

```bash
# Build the main apex-ls JAR first
cd ..
sbt build

# Build the MCP JAR
cd mcp
sbt build
```

## Running

```bash
java -jar target/scala-2.13/apex-ls-mcp*.jar
```

## Development Status

- ✅ Project structure created
- ✅ Build configuration with Java 17
- ✅ Bridge interface defined
- ⏳ Bridge implementation (TODO)
- ⏳ MCP tools migration (TODO)
- ⏳ MCP server implementation (TODO)