# MCP Subproject Agent Guide

## Scope
This guide applies to all files inside the `mcp/` directory and overrides instructions from parent `AGENTS.md` when conflicts arise.

## Project Notes
- Java 17+ is required for development and runtime.
- Depends on the parent Apex Language Server build artifacts; ensure the root project is built (`sbt build`) before running MCP builds.

## Formatting & Style
- Run `sbt javafmt` after modifying Java sources.
- Keep MCP tool and bridge code consistent with existing naming patterns (`ApexLsBridge`, `SfdxCodeDiagnosticsTool`, etc.).

## Testing & Build Commands
- Run `sbt test` for changes impacting MCP logic when feasible.
- Use targeted suites for focused updates:
  - `sbt "testOnly io.github.apexdevtools.apexls.mcp.bridge.*"`
  - `sbt "testOnly io.github.apexdevtools.apexls.mcp.tools.*"`
  - `sbt "testOnly io.github.apexdevtools.apexls.mcp.system.*"`
- Build artifacts with `sbt buildStandalone` when verifying distribution packaging.

## Operational Guidance
- MCP bridge operations must remain asynchronous and handle workspace caching carefully.
- Tools should validate inputs, surface clear MCP errors, and return JSON-compatible payloads.
- Preserve STDOUT/STDERR separation so MCP protocol traffic is not polluted by logs.

## References
- `mcp/.claude/CLAUDE.md` for detailed architecture, workflow, and troubleshooting notes.
