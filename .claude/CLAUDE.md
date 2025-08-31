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
- `sbt clean` - Remove build artifacts
- `sbt apexlsJS/Dev/build` - Create fast optimized JS bundle for debugging

### MCP Server (mcp/)
- `cd mcp && sbt buildStandalone` - Build standalone JAR with all dependencies for distribution
- `cd mcp && sbt test` - Run MCP-specific tests including system tests
- See [mcp/.claude/CLAUDE.md](mcp/.claude/CLAUDE.md) for detailed MCP server guidance

### JavaScript Platform Testing (js/npm/)
- `npm test` - Run Jest tests for JS platform
- `npm run test-samples` - Run system tests against apex-samples repository
- `npm run test-snapshot` - Update test snapshots

### Command-Line Usage
The jar is executable for direct analysis:
```bash
java -cp "apex-ls*.jar" io.github.apexdevtools.apexls.CheckForIssues [args]
java -cp "apex-ls*.jar" io.github.apexdevtools.apexls.DependencyReport [args]
```

Common arguments:
- `--workspace/-w` - Project directory (must contain sfdx-project.json)
- `--format/-f` - Output format (text/json/pmd)
- `--detail/-d` - Issue detail level (errors/warnings/unused)
- `--nocache/-n` - Disable caching

## Architecture Overview

### Multi-Platform Design
- **JVM Platform** (`jvm/`): Full-featured language server with file system access
- **JS Platform** (`js/`): Browser/Node.js compatible subset  
- **Shared** (`shared/`): Common code between platforms
- **MCP Server** (`mcp/`): Separate Java 17 MCP protocol server subproject

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


## Key Development Practices

### Testing Requirements
- Sequential test execution enforced across platforms due to shared resources
- Unit tests follow same package structure as main code
- Set `SAMPLES` environment variable to apex-samples repo path for system tests

### Testing Architecture
- **Unit Tests**: Component-level testing following package structure
- **System Tests**: End-to-end testing against real Apex projects
- **Cross-Platform Tests**: Validation that both JVM and JS implementations work identically
- **System tests**: In `js/npm/src/__tests__/` use the apex-samples repository

### Build Dependencies
- Development uses Java 17 toolchain, generated JARs target Java 8 for compatibility
- Always run `sbt scalafmtAll` before committing changes
- MCP server requires main apex-ls to be built first
- **Update CHANGELOG.md** when adding features, fixes, or breaking changes - focus on user-visible impacts with GitHub issue references

### Workspace Requirements
- All projects must contain `sfdx-project.json` file for workspace detection
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var

### Testing Against Samples
1. Clone [apex-samples](https://github.com/apex-dev-tools/apex-samples) repository
2. Set `export SAMPLES=<path-to-apex-samples>`
3. Run `npm run test-samples` from `js/npm/`

## Implementation Best Practices

### Before Coding
- **BP-1 (MUST)** Ask the user clarifying questions
- **BP-2 (SHOULD)** Draft and confirm an approach for complex work
- **BP-3 (SHOULD)** If ≥ 2 approaches exist, list clear pros and cons

### While Coding
- **C-1 (MUST)** Follow TDD: scaffold stub → write failing test → implement
- **C-2 (MUST)** Name functions with existing domain vocabulary for consistency
- **C-3 (SHOULD NOT)** Introduce classes when small testable functions suffice
- **C-4 (SHOULD)** Prefer simple, composable, testable functions
- **C-7 (SHOULD NOT)** Add comments except for critical caveats; rely on self‑explanatory code
- **C-9 (SHOULD NOT)** Extract a new function unless it will be reused elsewhere, is the only way to unit-test otherwise untestable logic, or drastically improves readability of an opaque block

### Testing Best Practices
- **T-4 (SHOULD)** Prefer integration tests over heavy mocking
- **T-5 (SHOULD)** Unit-test complex algorithms thoroughly
- **T-6 (SHOULD)** Test the entire structure in one assertion if possible

#### Test Quality Checklist
1. SHOULD parameterize inputs; never embed unexplained literals such as 42 or "foo" directly in the test
2. SHOULD NOT add a test unless it can fail for a real defect. Trivial asserts (e.g., expect(2).toBe(2)) are forbidden
3. SHOULD ensure the test description states exactly what the final expect verifies. If the wording and assert don't align, rename or rewrite
4. SHOULD compare results to independent, pre-computed expectations or to properties of the domain, never to the function's output re-used as the oracle
5. SHOULD follow the same lint, type-safety, and style rules as prod code (prettier, ESLint, strict types)
6. SHOULD express invariants or axioms (e.g., commutativity, idempotence, round-trip) rather than single hard-coded cases whenever practical. Use `fast-check` library e.g.
7. SHOULD test edge cases, realistic input, unexpected input, and value boundaries
8. SHOULD NOT test conditions that are caught by the type checker

### Function Quality Checklist
When evaluating whether a function you implemented is good or not:
1. Can you read the function and HONESTLY easily follow what it's doing? If yes, then stop here
2. Does the function have very high cyclomatic complexity? (number of independent paths, or, in a lot of cases, number of nesting if if-else as a proxy). If it does, then it's probably sketchy
3. Are there any common data structures and algorithms that would make this function much easier to follow and more robust? Parsers, trees, stacks / queues, etc
4. Are there any unused parameters in the function?
5. Are there any unnecessary type casts that can be moved to function arguments?
6. Is the function easily testable without mocking core features? If not, can this function be tested as part of an integration test?
7. Does it have any hidden untested dependencies or any values that can be factored out into the arguments instead? Only care about non-trivial dependencies that can actually change or affect the function
8. Brainstorm 3 better function names and see if the current name is the best, consistent with rest of codebase

### Tooling Gates
- **G-1 (MUST)** Code formatting check passes

### Git Practices
- **GH-1 (MUST)** Use Conventional Commits format when writing commit messages: https://www.conventionalcommits.org/en/v1.0.0
- **GH-2 (SHOULD NOT)** Refer to Claude or Anthropic in commit messages


## Quick Reference Shortcuts

This project includes Claude Code slash commands in `.claude/commands/`:
- `qnew` - Review and apply all best practices
- `qplan` - Analyze consistency with codebase  
- `qcode` - Implement with tests and formatting
- `qcheck` - Perform skeptical code review
- `qgit` - Add, commit, and push with conventional commits

## Documentation Reference

For additional information, see:
- [mcp/.claude/CLAUDE.md](mcp/.claude/CLAUDE.md) - MCP server specific guidance
- [mcp/README.md](mcp/README.md) - MCP server installation and usage
- [SFDX.md](SFDX.md) - SFDX metadata folder requirements and validation rules