# Repository Guidelines

## Project Structure & Module Organization
Source code lives under `shared/src`, `jvm/src`, and `js/src`, with platform-specific adaptations layered on top of shared logic. The MCP bridge resides in `mcp/`, and JavaScript packaging lives in `js/npm/`. Tests follow the same tree (`*/test/scala` for Scala, `js/npm/src/__tests__/` for Jest). Sample input projects are under `samples/` for local experiments.

## Build, Test, and Development Commands
- `sbt build`: Cross-builds JVM and Scala.js artifacts—run before packaging or invoking MCP builds.
- `sbt test`: Executes the complete Scala test matrix; use `sbt apexlsJVM/test` or `sbt apexlsJS/test` for platform-only runs.
- `npm test` (from `js/npm/`): Runs the Jest suite for the JS platform bundle.
- `cd mcp && sbt buildStandalone`: Produces the MCP all-in-one JAR for distribution. Use `buildRegular` when publishing to Maven Central.

## Coding Style & Naming Conventions
Scala code relies on `scalafmt`; run `sbt scalafmtAll` before committing. Favor descriptive, domain-aligned names (e.g., `OrgAPI`, `PackageLoader`) and keep new utilities within existing package hierarchies (`com.nawforce.apexlink.*`, `com.nawforce.pkgforce.*`). JavaScript follows the repo ESLint/Prettier defaults; use PascalCase for types, camelCase for values.

## Testing Guidelines
Scala tests use ScalaTest; name suites after the unit under test (e.g., `ClassModifierTest`). Prefer scenario-focused specs over excessive mocking. For JS, Jest test files sit in `__tests__` and should mirror the source filename. System tests rely on the `apex-samples` repo—set `SAMPLES=/path/to/apex-samples` before running `npm run test-samples`.

## Commit & Pull Request Guidelines
Commits must follow Conventional Commit semantics (`feat:`, `fix:`, `chore:`, etc.) and avoid tooling-specific references. Pull requests should describe the change, note impacted modules (shared/jvm/js/mcp), and call out test coverage (`sbt test`, `npm test`, or targeted suites). Include screenshots only when UI-related tools are touched, otherwise provide command output summaries.

## Reference Materials
- `.claude/CLAUDE.md`: High-level repository overview, key commands, and architectural context.
- `.claude/GUIDELINES.md`: Day-to-day implementation, testing, and git workflow expectations.
- `.claude/MODULES.md`: Proposed modularization plan; review before reorganizing packages.

## Agent-Specific Notes
Keep memory of long builds in check by reusing incremental `sbt` shells when possible. When editing generated or cache-backed files, confirm whether changes belong under `modules/` proposals or existing trees. Always respect the repository’s Java 17 toolchain while preserving Java 8 runtime compatibility for published jars.
