# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an MCP (Model Context Protocol) Server implementation that provides a bridge between AI tools and the Apex Language Server. It exposes Apex code analysis capabilities through the standardized MCP protocol, enabling AI assistants to perform static analysis, find references, and navigate Apex codebases.

## Architecture

### Java Version Split Design
- **MCP Server**: Java 17 (required by MCP Java SDK)
- **Apex Language Server Core**: Java 8 (parent dependency)
- **Communication**: Bridge pattern using reflection to communicate between Java versions

### Key Components
- **MCPServer**: Main entry point running MCP protocol via STDIN/STDOUT
- **Bridge Architecture**: 
  - `ApexLsBridge` interface defines communication contract
  - `EmbeddedApexLsBridge` uses reflection to call apex-ls OrgAPI directly
  - Workspace caching to avoid expensive re-initialization
- **MCP Tools**: `apex_static_analysis`, `apex_find_references`, `apex_goto_definition`
- **MCP Resources**: `workspace://apex/{workspace_path}` for workspace metadata

## Build Commands

### Prerequisites
Requires parent apex-ls project to be built first:
```bash
# From apex-ls root directory:
sbt apexlsJVM/packageBin
```

### MCP Server Build
```bash
cd mcp
sbt build                    # Creates deployable JAR with dependencies
sbt test                     # Run test suite
sbt clean                    # Remove build artifacts
```

### Alternative Test Execution
Due to SBT/JUnit Jupiter integration issues, direct JUnit execution is recommended:
```bash
java -cp "$(sbt 'export Test/fullClasspath' | tail -n 1)" org.junit.platform.console.ConsoleLauncher --scan-classpath
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

### Package Structure
- `io.github.apexdevtools.apexls.mcp` - Main MCP server implementation
  - `bridge/` - Java version bridge components
  - `tools/` - MCP tool implementations (static analysis, references, definitions)
  - `resources/` - MCP resource implementations
- Tests follow same package structure in `src/test/java/`

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

### Workspace Requirements
- Must contain valid `sfdx-project.json` file
- Compatible with same workspace format as parent apex-ls project
- Supports all Salesforce DX project structures

### Logging Configuration
- Production: Logs to STDERR (avoids STDOUT MCP protocol interference)
- Test: Separate `logback-test.xml` configuration
- Default level: INFO, DEBUG available for troubleshooting

### Relationship to Parent Project
This MCP server is a separate Java 17 subproject within the larger Apex Language Server ecosystem. It depends on the JVM build artifacts of the main apex-ls project and provides MCP protocol access to the core Scala-based analysis engine.


# Code Guidelines
See https://www.sabrina.dev/p/ultimate-ai-coding-guide-claude-code

## Implementation Best Practices

### 0 — Purpose

These rules ensure maintainability, safety, and developer velocity.
**MUST** rules are enforced by CI; **SHOULD** rules are strongly recommended.

---

### 1 — Before Coding

- **BP-1 (MUST)** Ask the user clarifying questions.
- **BP-2 (SHOULD)** Draft and confirm an approach for complex work.
- **BP-3 (SHOULD)** If ≥ 2 approaches exist, list clear pros and cons.

---

### 2 — While Coding

- **C-1 (MUST)** Follow TDD: scaffold stub -> write failing test -> implement.
- **C-2 (MUST)** Name functions with existing domain vocabulary for consistency.
- **C-3 (SHOULD NOT)** Introduce classes when small testable functions suffice.
- **C-4 (SHOULD)** Prefer simple, composable, testable functions.
- **C-7 (SHOULD NOT)** Add comments except for critical caveats; rely on self‑explanatory code.
- **C-9 (SHOULD NOT)** Extract a new function unless it will be reused elsewhere, is the only way to unit-test otherwise untestable logic, or drastically improves readability of an opaque block.

---

### 3 — Testing

- **T-4 (SHOULD)** Prefer integration tests over heavy mocking.
- **T-5 (SHOULD)** Unit-test complex algorithms thoroughly.
- **T-6 (SHOULD)** Test the entire structure in one assertion if possible

---

### 6 — Tooling Gates

- **G-1 (MUST)** `prettier --check` passes.

---

### 7 - Git

- **GH-1 (MUST**) Use Conventional Commits format when writing commit messages: https://www.conventionalcommits.org/en/v1.0.0
- **GH-2 (SHOULD NOT**) Refer to Claude or Anthropic in commit messages.

---

## Writing Functions Best Practices

When evaluating whether a function you implemented is good or not, use this checklist:

1. Can you read the function and HONESTLY easily follow what it's doing? If yes, then stop here.
2. Does the function have very high cyclomatic complexity? (number of independent paths, or, in a lot of cases, number of nesting if if-else as a proxy). If it does, then it's probably sketchy.
3. Are there any common data structures and algorithms that would make this function much easier to follow and more robust? Parsers, trees, stacks / queues, etc.
4. Are there any unused parameters in the function?
5. Are there any unnecessary type casts that can be moved to function arguments?
6. Is the function easily testable without mocking core features? If not, can this function be tested as part of an integration test?
7. Does it have any hidden untested dependencies or any values that can be factored out into the arguments instead? Only care about non-trivial dependencies that can actually change or affect the function.
8. Brainstorm 3 better function names and see if the current name is the best, consistent with rest of codebase.

IMPORTANT: you SHOULD NOT refactor out a separate function unless there is a compelling need, such as:
- the refactored function is used in more than one place
- the refactored function is easily unit testable while the original function is not AND you can't test it any other way
- the original function is extremely hard to follow and you resort to putting comments everywhere just to explain it

## Writing Tests Best Practices

When evaluating whether a test you've implemented is good or not, use this checklist:

1. SHOULD parameterize inputs; never embed unexplained literals such as 42 or "foo" directly in the test.
2. SHOULD NOT add a test unless it can fail for a real defect. Trivial asserts (e.g., expect(2).toBe(2)) are forbidden.
3. SHOULD ensure the test description states exactly what the final expect verifies. If the wording and assert don’t align, rename or rewrite.
4. SHOULD compare results to independent, pre-computed expectations or to properties of the domain, never to the function’s output re-used as the oracle.
5. SHOULD follow the same lint, type-safety, and style rules as prod code (prettier, ESLint, strict types).
6. SHOULD express invariants or axioms (e.g., commutativity, idempotence, round-trip) rather than single hard-coded cases whenever practical. Use `fast-check` library e.g.
10. SHOULD test edge cases, realistic input, unexpected input, and value boundaries.
11. SHOULD NOT test conditions that are caught by the type checker.

## Remember Shortcuts

Remember the following shortcuts which the user may invoke at any time.

### QNEW

When I type "qnew", this means:

```
Understand all BEST PRACTICES listed in CLAUDE.md.
Your code SHOULD ALWAYS follow these best practices.
```

### QPLAN
When I type "qplan", this means:
```
Analyze similar parts of the codebase and determine whether your plan:
- is consistent with rest of codebase
- introduces minimal changes
- reuses existing code
```

## QCODE

When I type "qcode", this means:

```
Implement your plan and make sure your new tests pass.
Always run tests to make sure you didn't break anything else.
Always run `prettier` on the newly created files to ensure standard formatting.
```

### QCHECK

When I type "qcheck", this means:

```
You are a SKEPTICAL senior software engineer.
Perform this analysis for every MAJOR code change you introduced (skip minor changes):

1. CLAUDE.md checklist Writing Functions Best Practices.
2. CLAUDE.md checklist Writing Tests Best Practices.
3. CLAUDE.md checklist Implementation Best Practices.
```

### QCHECKF

When I type "qcheckf", this means:

```
You are a SKEPTICAL senior software engineer.
Perform this analysis for every MAJOR function you added or edited (skip minor changes):

1. CLAUDE.md checklist Writing Functions Best Practices.
```

### QCHECKT

When I type "qcheckt", this means:

```
You are a SKEPTICAL senior software engineer.
Perform this analysis for every MAJOR test you added or edited (skip minor changes):

1. CLAUDE.md checklist Writing Tests Best Practices.
```

### QUX

When I type "qux", this means:

```
Imagine you are a human UX tester of the feature you implemented. 
Output a comprehensive list of scenarios you would test, sorted by highest priority.
```

### QGIT

When I type "qgit", this means:

```
Add all changes to staging, create a commit, and push to remote.

Follow this checklist for writing your commit message:
- SHOULD use Conventional Commits format: https://www.conventionalcommits.org/en/v1.0.0
- SHOULD NOT refer to Claude or Anthropic in the commit message.
- SHOULD structure commit message as follows:
<type>[optional scope]: <description>
[optional body]
[optional footer(s)]
- commit SHOULD contain the following structural elements to communicate intent: 
fix: a commit of the type fix patches a bug in your codebase (this correlates with PATCH in Semantic Versioning).
feat: a commit of the type feat introduces a new feature to the codebase (this correlates with MINOR in Semantic Versioning).
BREAKING CHANGE: a commit that has a footer BREAKING CHANGE:, or appends a ! after the type/scope, introduces a breaking API change (correlating with MAJOR in Semantic Versioning). A BREAKING CHANGE can be part of commits of any type.
types other than fix: and feat: are allowed, for example @commitlint/config-conventional (based on the Angular convention) recommends build:, chore:, ci:, docs:, style:, refactor:, perf:, test:, and others.
footers other than BREAKING CHANGE: <description> may be provided and follow a convention similar to git trailer format.
