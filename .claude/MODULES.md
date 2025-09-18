# Apex-LS Modularization Plan

This document outlines the proposed modularization of the apex-ls codebase to improve maintainability, dependency management, and development workflow while preserving the existing cross-compilation architecture and single JAR distribution model.

## Current Architecture

The apex-ls project currently uses a monolithic cross-project structure:
- `shared/` - Cross-platform shared code 
- `jvm/` - JVM-specific implementations
- `js/` - JavaScript platform code
- `mcp/` - Model Context Protocol server (separate subproject)

## Proposed Module Structure

```
apex-ls/
├── modules/
│   ├── core/                    # Core types and foundational abstractions  
│   ├── diagnostics/             # Issue management and diagnostics
│   ├── parsing/                 # Parser infrastructure and AST handling
│   ├── type-system/             # Apex type system and resolution
│   ├── analysis/                # Static analysis and CST processing  
│   ├── language-server/         # LSP protocol and RPC handling
│   ├── workspace/               # SFDX project and workspace management
│   └── cli/                     # Command-line tools and entry points
├── shared/                      # Cross-platform shared code (unchanged)
├── js/                          # JavaScript platform (unchanged)
├── jvm/                         # JVM platform aggregator
└── mcp/                         # MCP server (unchanged)
```

## Module Definitions

### 1. **`core`** - Foundation Layer
**Purpose**: Core abstractions, utilities, and cross-cutting concerns

**Contents**:
- `com.nawforce.pkgforce.memory.*` - Caching, identity management, intern cache
- `com.nawforce.pkgforce.names.*` - Type names, identifiers, encoded names
- `com.nawforce.pkgforce.path.*` - Path abstractions and location handling
- `com.nawforce.runtime.platform.*` - Environment, platform utilities

**Dependencies**: None (foundation layer)

**Key Responsibilities**:
- Memory management and caching infrastructure
- Name resolution and encoding
- Platform abstraction layer
- Core utility functions

### 2. **`diagnostics`** - Issue Management  
**Purpose**: Centralized diagnostic and issue reporting

**Contents**:
- `com.nawforce.pkgforce.diagnostics.*` - Issues, loggers, managers, duplicates
- `com.nawforce.apexlink.diagnostics.*` - Issue operations

**Dependencies**: `core`

**Key Responsibilities**:
- Issue collection and management
- Diagnostic reporting and logging
- Error categorization and filtering

### 3. **`parsing`** - Parser Infrastructure
**Purpose**: Source code parsing and AST construction

**Contents**:
- `com.nawforce.runtime.parsers.*` - Code/page parsers, source handling
- `com.nawforce.pkgforce.parsers.*` - Apex summaries, UTF8 handling
- `com.nawforce.runtime.xml.*` - XML document handling

**Dependencies**: `core`, `diagnostics`

**Key Responsibilities**:
- Source code parsing (Apex, VF, XML)
- AST construction and validation
- Parser error handling and recovery

### 4. **`type-system`** - Type Infrastructure
**Purpose**: Type declarations, resolution, and management

**Contents**:
- `com.nawforce.apexlink.types.*` - All type declarations (apex, platform, schema, synthetic)
- `com.nawforce.runtime.types.*` - Platform type handling
- `com.nawforce.apexlink.finding.*` - Type resolution and finding
- `com.nawforce.apexlink.names.*` - Type naming and identifiers

**Dependencies**: `core`, `diagnostics`, `parsing`

**Key Responsibilities**:
- Type system implementation
- Type resolution and lookup
- Platform and schema type handling
- Synthetic type generation

### 5. **`analysis`** - Static Analysis
**Purpose**: CST processing, semantic analysis, and validation

**Contents**:
- `com.nawforce.apexlink.cst.*` - Concrete syntax tree processing
- `com.nawforce.apexlink.opcst.*` - Outline parser CST handling
- `com.nawforce.apexlink.analysis.*` - Organization analysis
- `com.nawforce.apexlink.memory.*` - Analysis-specific caching

**Dependencies**: `core`, `diagnostics`, `parsing`, `type-system`

**Key Responsibilities**:
- Semantic analysis and validation
- Control flow analysis
- Dependency tracking
- Code quality checks

### 6. **`workspace`** - Project Management
**Purpose**: SFDX project handling and workspace operations

**Contents**:
- `com.nawforce.pkgforce.sfdx.*` - SFDX project support
- `com.nawforce.pkgforce.workspace.*` - Workspace/layer management
- `com.nawforce.pkgforce.documents.*` - Metadata documents
- `com.nawforce.pkgforce.stream.*` - Metadata generators
- `com.nawforce.runtime.workspace.*` - Workspace utilities

**Dependencies**: `core`, `diagnostics`, `parsing`

**Key Responsibilities**:
- SFDX project structure handling
- Workspace and layer management
- Metadata document processing
- File system abstraction

### 7. **`language-server`** - LSP Implementation
**Purpose**: Language server protocol and organization-level operations

**Contents**:
- `com.nawforce.apexlink.rpc.*` - RPC server, protocol handling
- `com.nawforce.apexlink.org.*` - Completion, hover, references
- `com.nawforce.apexlink.api.*` - Public API classes
- `com.nawforce.apexlink.plugins.*` - Plugin system

**Dependencies**: `core`, `diagnostics`, `parsing`, `type-system`, `analysis`, `workspace`

**Key Responsibilities**:
- Language Server Protocol implementation
- Code completion and navigation
- Hover information and documentation
- Plugin architecture

### 8. **`cli`** - Command Line Interface
**Purpose**: Command-line tools and entry points

**Contents**:
- `io.github.apexdevtools.apexls.*` - CheckForIssues, DependencyReport, Server
- `com.nawforce.runtime.cmds.*` - Indexer commands
- `com.nawforce.apexlink.indexer.*` - Indexing functionality

**Dependencies**: `language-server` (and transitively all others)

**Key Responsibilities**:
- Command-line interface
- Batch processing tools
- Application entry points

## SBT Configuration Strategy

### Multi-Module Structure
```scala
// build.sbt structure maintaining single JAR output
lazy val commonSettings = Seq(
  scalaVersion := "2.13.16",
  organization := "io.github.apex-dev-tools",
  // ... other common settings
)

// Core foundation module
lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "apex-ls-core",
    libraryDependencies ++= Seq(
      // Core dependencies only
    )
  )

// Diagnostics module  
lazy val diagnostics = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/diagnostics"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-diagnostics"
  )

// Parsing module
lazy val parsing = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/parsing"))
  .dependsOn(core, diagnostics)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-parsing",
    libraryDependencies ++= Seq(
      "io.github.apex-dev-tools" % "apex-parser" % "4.3.1",
      "io.github.apex-dev-tools" % "vf-parser" % "1.1.0"
    )
  )

// Type system module
lazy val typeSystem = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/type-system"))
  .dependsOn(core, diagnostics, parsing)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-type-system",
    libraryDependencies ++= Seq(
      "io.github.apex-dev-tools" % "sobject-types" % "64.0.0",
      "io.github.apex-dev-tools" % "standard-types" % "64.0.0"
    )
  )

// Analysis module
lazy val analysis = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/analysis"))
  .dependsOn(core, diagnostics, parsing, typeSystem)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-analysis"
  )

// Workspace module
lazy val workspace = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/workspace"))
  .dependsOn(core, diagnostics, parsing)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-workspace"
  )

// Language server module
lazy val languageServer = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/language-server"))
  .dependsOn(core, diagnostics, parsing, typeSystem, analysis, workspace)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-language-server",
    libraryDependencies ++= Seq(
      "com.github.nawforce" %%% "scala-json-rpc" % "1.1.0",
      "com.github.nawforce" %%% "scala-json-rpc-upickle-json-serializer" % "1.1.0"
    )
  )

// CLI module
lazy val cli = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/cli"))
  .dependsOn(languageServer)
  .settings(commonSettings)
  .settings(
    name := "apex-ls-cli",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "mainargs" % "0.5.4"
    )
  )

// Main aggregator project (maintains existing API)
lazy val apexls = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .dependsOn(cli) // Transitively includes all modules
  .settings(commonSettings)
  .settings(
    name := "apex-ls",
    // Existing settings for backward compatibility
    libraryDependencies ++= Seq(
      "io.github.apex-dev-tools" %%% "outline-parser" % "1.3.0",
      "com.lihaoyi" %%% "upickle" % "1.2.0",
      "org.scalatest" %%% "scalatest" % "3.2.0" % Test
    )
  )
  .jvmSettings(
    // Assembly plugin to create single JAR
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x if x.endsWith(".class") => MergeStrategy.first
      case x if x.endsWith(".properties") => MergeStrategy.first
      case _ => MergeStrategy.first
    },
    // Existing JVM settings
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0",
      "io.methvin" %% "directory-watcher-better-files" % "0.18.0"
    )
  )
```

### Assembly Configuration
The main project will use the SBT Assembly plugin to create a single JAR containing all modules, maintaining backward compatibility:

```scala
// project/plugins.sbt
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
```

## Dependency Flow

```
cli → language-server → analysis → type-system → parsing → diagnostics → core
     ↘ workspace ↗
```

This dependency graph ensures:
- No circular dependencies
- Clear layered architecture
- Minimal coupling between modules
- Controlled access to functionality

## Migration Strategy

### Phase 1: Create Module Structure
1. Create `modules/` directory structure
2. Move source files to appropriate modules based on package structure
3. Update `build.sbt` with module definitions
4. Ensure all tests still pass

### Phase 2: Dependency Cleanup
1. Review and clean up dependencies between modules
2. Remove any circular dependencies
3. Optimize library dependencies per module

### Phase 3: Testing and Validation
1. Verify single JAR assembly still works
2. Confirm cross-compilation works for all modules
3. Validate MCP server integration
4. Run full test suite

## Key Benefits

1. **Clear Separation of Concerns**: Each module has a single, well-defined responsibility
2. **Controlled Dependencies**: Prevents circular dependencies and enforces layered architecture  
3. **Faster Compilation**: SBT can compile modules in parallel and cache unchanged modules
4. **Easier Testing**: Focused unit tests per module with clear boundaries
5. **Maintainable Architecture**: New features can be added to appropriate modules
6. **Single JAR Output**: Assembly plugin aggregates all modules into the existing single JAR
7. **Cross-Platform Preserved**: Each module supports both JVM and JS platforms
8. **Backward Compatibility**: Existing build artifacts and public API remain unchanged

## Compatibility Guarantees

- **Build Commands**: All existing `sbt` commands continue to work
- **JAR Output**: Single JAR distribution model preserved
- **Public API**: No changes to public interfaces
- **Cross-Platform**: Both JVM and JS builds continue to work
- **MCP Integration**: MCP server remains a separate subproject
- **Test Structure**: Existing test organization preserved

This modularization provides a foundation for future growth while maintaining all existing functionality and build processes.