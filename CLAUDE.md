# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the Apex Language Server, a Salesforce Apex static analysis toolkit built in Scala that provides error checking, type finding, code completion, and other language server features. The project is a cross-build supporting both JVM and ScalaJS platforms, with an additional MCP (Model Context Protocol) server component.

## Essential Commands

### Main Project (apex-ls)
- `sbt build` - Creates packaged jar (JVM) or js bundle (JS) for testing and release
- `sbt test` - Execute full test suite across both platforms  
- `sbt scalafmtAll` - Reformat all code using scalafmt (run before commits)
- `sbt "testOnly com.nawforce.apexlink.cst.ClassModifierTest"` - Run specific test class
- `sbt apexlsJVM/test` - Run JVM-only tests
- `sbt apexlsJS/test` - Run ScalaJS-only tests

### MCP Server (mcp/)
- `cd mcp && sbt buildRegular` - Build regular JAR for Maven Central publishing
- `cd mcp && sbt buildStandalone` - Build standalone JAR with all dependencies for distribution
- `cd mcp && sbt test` - Run MCP-specific tests including system tests
- `cd mcp && sbt "testOnly io.github.apexdevtools.apexls.mcp.system.*"` - Run system tests only

### JavaScript Platform Testing (js/npm/)
- `npm test` - Run Jest tests for JS platform
- `npm run test-samples` - Run system tests against apex-samples repository
- `npm run test-snapshot` - Update test snapshots

## Architecture Overview

### Multi-Platform Design
- **JVM Platform** (`jvm/`): Full-featured language server with file system access
- **JS Platform** (`js/`): Browser/Node.js compatible subset  
- **Shared** (`shared/`): Common code between platforms
- **MCP Server** (`mcp/`): Java 17 MCP protocol server that bridges to Java 8 apex-ls core

### MCP Bridge Architecture
The MCP server uses a bridge pattern to communicate between Java 17 MCP code and Java 8 apex-ls:
- **ApexLsBridge interface** - Defines async operations (getIssues, findUsages, getDefinition, etc.)
- **EmbeddedApexLsBridge** - Implementation accessing OrgAPI directly within same JVM
- **Workspace caching** - OrgAPI instances cached per workspace to avoid expensive re-initialization
- **Async communication** - Uses CompletableFuture with Scala Future conversion

## Key Development Practices

### Testing Requirements
- Sequential test execution enforced across platforms due to shared resources
- System tests validate full MCP protocol compliance for mcp/ component
- Unit tests follow same package structure as main code
- Set `SAMPLES` environment variable to apex-samples repo path for system tests

### Build Dependencies
- Main apex-ls must be built before MCP server: `sbt build` then `cd mcp && sbt build`
- MCP server requires Java 17, core apex-ls requires Java 8
- Always run `sbt scalafmtAll` before committing changes

### Workspace Requirements
- All projects must contain `sfdx-project.json` file for workspace detection
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var

## Quick Reference Shortcuts

For shortcuts and coding guidelines, see [GUIDELINES.md](GUIDELINES.md):
- `qnew` - Review and apply all best practices
- `qplan` - Analyze consistency with codebase  
- `qcode` - Implement with tests and formatting
- `qcheck` - Perform skeptical code review
- `qgit` - Add, commit, and push with conventional commits

For detailed information, see:
- [DEVELOPMENT.md](DEVELOPMENT.md) - Build commands, testing, and workflow
- [ARCHITECTURE.md](ARCHITECTURE.md) - Code structure and key packages  
- [GUIDELINES.md](GUIDELINES.md) - Code style, best practices, and shortcuts
- [mcp/README.md](mcp/README.md) - MCP server installation and usage
- [mcp/SYSTEM_TESTS.md](mcp/SYSTEM_TESTS.md) - MCP system testing documentation