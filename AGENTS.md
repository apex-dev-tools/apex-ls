# Apex Language Server Agent Guide

## Scope
This guide applies to all changes in the repository unless a more specific `AGENTS.md` exists deeper in the tree.

## Project Overview
- Scala cross-build project targeting JVM and Scala.js platforms.
- Requires Java 17+ for development while published artifacts remain Java 8 compatible.

## Development Expectations
- Prefer small, composable functions and follow the existing domain terminology.
- Avoid unnecessary refactors or comment noise; rely on self-explanatory code.
- Keep new dependencies minimal and justified.

## Formatting & Linting
- Run `sbt scalafmtAll` after modifying Scala sources.
- Format Markdown or JSON when editing them (use existing style as reference).

## Testing
- Run `sbt test` for Scala/JVM/JS changes when feasible.
- For sample-project regression tests, use the sibling `../apex-samples` checkout:
  1. Build the JVM distribution/classpath first: `sbt apexlsJVM/build`
  2. Run samples from the npm package directory:
     `cd js/npm && SAMPLES=$(cd ../../../apex-samples && pwd) npm run test-samples -- --runInBand`
  3. If snapshot changes are expected, update with:
     `cd js/npm && SAMPLES=$(cd ../../../apex-samples && pwd) npm run test-snapshot -- --runInBand`
- Document any skipped or partial test coverage in the PR if full test suite is impractical.

## Git & PR Etiquette
- Use Conventional Commit messages (e.g., `feat:`, `fix:`, `chore:`).
- Summaries should focus on user-facing impact and avoid mentioning AI assistants.
- Ensure PR descriptions include the commands/tests that were run.
