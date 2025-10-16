# @apexdevtools/apex-ls-mcp

NPX wrapper for the Apex Language Server MCP (Model Context Protocol) server.

## Installation & Usage

### NPX (Recommended)

Run directly without installation:

```bash
npx @apexdevtools/apex-ls-mcp [arguments]
```

### Global Installation

```bash
npm install -g @apexdevtools/apex-ls-mcp
apex-ls-mcp [arguments]
```

### IDE Integration

**VS Code (.vscode/mcp.json):**

```json
{
  "apex-ls-mcp": {
    "command": "npx",
    "args": ["@apexdevtools/apex-ls-mcp"]
  }
}
```

**Claude Desktop:**

```json
{
  "mcpServers": {
    "apex-ls": {
      "command": "npx", 
      "args": ["@apexdevtools/apex-ls-mcp"]
    }
  }
}
```

## Requirements

- **Java 17+** (required for MCP server runtime)
- **Node.js 14+** (for npm/npx)

## How It Works

1. **On Install**: Downloads the apex-ls-mcp JAR to `~/.apex-ls-mcp/`
2. **On Run**: Validates Java 17+ and spawns the JAR with your arguments
3. **Version Management**: Updates JAR automatically when package version changes
4. **Caching**: Reuses downloaded JAR until version changes

## Troubleshooting

### "Java not found"

- Install Java 17+ and ensure it's in your PATH
- Run `java -version` to verify

### "JAR not found or version mismatch"

- Run `npm install @apexdevtools/apex-ls-mcp` to re-download

### Network Issues

- Check internet connection
- Corporate firewalls may block GitHub releases

## Cache Location

JAR files are cached in `~/.apex-ls-mcp/`:

- `apex-ls-mcp.jar` - The MCP server JAR
- `version.txt` - Cached version info

## License

BSD-3-Clause - see the main [apex-ls repository](https://github.com/apex-dev-tools/apex-ls) for details.
