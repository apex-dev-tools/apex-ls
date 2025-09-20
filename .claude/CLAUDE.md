# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the Apex Language Server, a Salesforce Apex static analysis toolkit built in Scala that provides error checking, type finding, code completion, and other language server features. The project is a cross-build supporting both JVM and ScalaJS platforms, with an additional MCP (Model Context Protocol) server component.

## Development Requirements

- **Java 17+** for development (uses Java 17 toolchain and language features)
- **Generated JARs maintain Java 8 runtime compatibility** for end users
- **MCP server requires Java 17+** for both development and runtime

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
- **JVM Platform** (`jvm/src/main/scala`): Full-featured language server with file system access
- **JS Platform** (`js/src/main/scala`): Browser/Node.js compatible subset  
- **Shared** (`shared/src/main/scala`): Common code between platforms
- **MCP Server** (`mcp/`): Java 17 MCP protocol server that bridges to Java 8 apex-ls core

### Key Packages

#### `com.nawforce.apexlink` - Main language server implementation
- `api/` - Public API classes (Org, Package, ServerOps)
- `cst/` - Concrete Syntax Tree handling and analysis
- `org/` - Organization-level operations (completion, hover, references)
- `types/` - Type system (apex, platform, schema, synthetic types)
- `rpc/` - RPC server and protocol handling

#### `com.nawforce.pkgforce` - Package and metadata management
- `sfdx/` - SFDX project handling
- `stream/` - Metadata generators
- `workspace/` - Workspace and layer management

#### `io.github.apexdevtools.apexls` - Command-line tools and entry points

### Cross-Platform Design
The project uses Scala's cross-compilation features to support both JVM and JavaScript platforms:
- **Shared code**: Core language analysis logic that works on both platforms
- **Platform-specific code**: File system access (JVM) vs. in-memory operations (JS)
- **Build system**: SBT with cross-compilation settings for seamless builds

### Type System Architecture
The type system is designed around several key concepts:
- **Apex Types**: User-defined classes, interfaces, enums
- **Platform Types**: Salesforce standard objects and system types
- **Schema Types**: Custom objects and fields from Salesforce metadata
- **Synthetic Types**: Generated types for system functionality

### MCP Bridge Architecture
The MCP server uses a bridge pattern to communicate between Java 17 MCP code and Java 8 apex-ls:
- **ApexLsBridge interface** - Defines async operations (getIssues, findUsages, getDefinition, etc.)
- **EmbeddedApexLsBridge** - Implementation accessing OrgAPI directly within same JVM
- **Workspace caching** - OrgAPI instances cached per workspace to avoid expensive re-initialization
- **Async communication** - Uses CompletableFuture with Scala Future conversion

### Testing Architecture
- **Unit Tests**: Component-level testing following package structure
- **System Tests**: End-to-end testing against real Apex projects
- **Cross-Platform Tests**: Validation that both JVM and JS implementations work identically

## Key Development Practices

### Testing Requirements
- Sequential test execution enforced across platforms due to shared resources
- System tests validate full MCP protocol compliance for mcp/ component
- Unit tests follow same package structure as main code
- Set `SAMPLES` environment variable to apex-samples repo path for system tests

### Build Dependencies
- Main apex-ls must be built before MCP server: `sbt build` then `cd mcp && sbt build`
- Development uses Java 17 toolchain, generated JARs target Java 8 for compatibility
- Always run `sbt scalafmtAll` before committing changes

### Workspace Requirements
- All projects must contain `sfdx-project.json` file for workspace detection
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var

## Development Workflow

### Additional Build Commands
- `sbt clean` - Remove build artifacts
- `sbt apexlsJS/Dev/build` - Create fast optimized JS bundle for debugging

### Running Single Tests
- **SBT**: `sbt "testOnly com.nawforce.apexlink.cst.ClassModifierTest"`
- **NPM**: `npm test -- --testNamePattern="specific test"`

### Code Formatting
Always run `sbt scalafmtAll` before committing changes.

### Testing Against Samples
1. Clone [apex-samples](https://github.com/apex-dev-tools/apex-samples) repository
2. Set `export SAMPLES=<path-to-apex-samples>`
3. Run `npm run test-samples` from `js/npm/`

### Command-Line Usage
The jar is executable for direct analysis:
```sh
java -cp "apex-ls*.jar" io.github.apexdevtools.apexls.CheckForIssues [args]
java -cp "apex-ls*.jar" io.github.apexdevtools.apexls.DependencyReport [args]
```

Common arguments:
- `--workspace/-w` - Project directory (must contain sfdx-project.json)
- `--format/-f` - Output format (text/json/pmd)
- `--detail/-d` - Issue detail level (errors/warnings/unused)
- `--nocache/-n` - Disable caching

### Test Structure
- Unit tests follow the same package structure as main code
- System tests in `js/npm/src/__tests__/` use the apex-samples repository
- Set `SAMPLES` environment variable to apex-samples repo path for system tests

### Important Development Notes
- Sequential test execution enforced across platforms due to shared resources
- Cross-platform build requires both Scala 2.13 and appropriate JS/JVM dependencies
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var

## Quick Reference Shortcuts

For shortcuts and coding guidelines, see [GUIDELINES.md](GUIDELINES.md):
- `qnew` - Review and apply all best practices
- `qplan` - Analyze consistency with codebase  
- `qcode` - Implement with tests and formatting
- `qcheck` - Perform skeptical code review
- `qgit` - Add, commit, and push with conventional commits

For additional information, see:
- [GUIDELINES.md](GUIDELINES.md) - Code style, best practices, and shortcuts  
- [mcp/README.md](../mcp/README.md) - MCP server installation and usage
- [mcp/.claude/CLAUDE.md](../mcp/.claude/CLAUDE.md) - Comprehensive MCP server documentation
