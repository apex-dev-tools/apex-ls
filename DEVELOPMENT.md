# Development Guide

This document covers build commands, testing strategies, and development workflows for the Apex Language Server.

## Build Commands

### SBT Commands (Primary Build System)
- `sbt build` - Creates packaged jar (JVM) or js bundle (JS) for testing and release
- `sbt test` - Execute full test suite across both platforms
- `sbt apexlsJVM/test` - Run JVM-only tests
- `sbt apexlsJS/test` - Run ScalaJS-only tests
- `sbt scalafmtAll` - Reformat all code using scalafmt
- `sbt clean` - Remove build artifacts
- `sbt apexlsJS/Dev/build` - Create fast optimized JS bundle for debugging

### NPM Commands (JS Platform Testing)
Located in `js/npm/`:
- `npm test` - Run Jest tests for JS platform
- `npm run test-samples` - Run system tests against apex-samples repository
- `npm run test-snapshot` - Update test snapshots

## Development Workflow

### Running Single Tests
- SBT: `sbt "testOnly com.nawforce.apexlink.cst.ClassModifierTest"`
- NPM: `npm test -- --testNamePattern="specific test"`

### Code Formatting
Always run `sbt scalafmtAll` before committing changes.

### Testing Against Samples
1. Clone [apex-samples](https://github.com/apex-dev-tools/apex-samples) repository
2. Set `export SAMPLES=<path-to-apex-samples>`
3. Run `npm run test-samples` from `js/npm/`

## Command-Line Usage

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

## Test Structure

- Unit tests follow the same package structure as main code
- System tests in `js/npm/src/__tests__/` use the apex-samples repository
- Set `SAMPLES` environment variable to apex-samples repo path for system tests

## Important Development Notes

- Sequential test execution enforced across platforms due to shared resources
- Cross-platform build requires both Scala 2.13 and appropriate JS/JVM dependencies
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var