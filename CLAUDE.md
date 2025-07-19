# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the Apex Language Server, a Salesforce Apex static analysis toolkit built in Scala that provides error checking, type finding, code completion, and other language server features. The project is a cross-build supporting both JVM and ScalaJS platforms.

## Key Build Commands

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

## Architecture

### Core Structure
- **JVM Platform** (`jvm/src/main/scala`): Full-featured language server with file system access
- **JS Platform** (`js/src/main/scala`): Browser/Node.js compatible subset
- **Shared** (`shared/src/main/scala`): Common code between platforms

### Key Packages
- `com.nawforce.apexlink` - Main language server implementation
  - `api/` - Public API classes (Org, Package, ServerOps)
  - `cst/` - Concrete Syntax Tree handling and analysis
  - `org/` - Organization-level operations (completion, hover, references)
  - `types/` - Type system (apex, platform, schema, synthetic types)
  - `rpc/` - RPC server and protocol handling
- `com.nawforce.pkgforce` - Package and metadata management
  - `sfdx/` - SFDX project handling
  - `stream/` - Metadata generators
  - `workspace/` - Workspace and layer management
- `io.github.apexdevtools.apexls` - Command-line tools and entry points

### Test Structure
- Unit tests follow the same package structure as main code
- System tests in `js/npm/src/__tests__/` use the apex-samples repository
- Set `SAMPLES` environment variable to apex-samples repo path for system tests

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

## Important Notes

- Sequential test execution enforced across platforms due to shared resources
- Cross-platform build requires both Scala 2.13 and appropriate JS/JVM dependencies
- Cache directory defaults to `$HOME/.apexlink_cache` or `APEXLINK_CACHE_DIR` env var
- All projects must contain `sfdx-project.json` file for workspace detection