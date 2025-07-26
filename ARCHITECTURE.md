# Architecture Guide

This document describes the code structure, key packages, and architectural patterns of the Apex Language Server.

## Core Structure

- **JVM Platform** (`jvm/src/main/scala`): Full-featured language server with file system access
- **JS Platform** (`js/src/main/scala`): Browser/Node.js compatible subset
- **Shared** (`shared/src/main/scala`): Common code between platforms

## Key Packages

### `com.nawforce.apexlink` - Main language server implementation
- `api/` - Public API classes (Org, Package, ServerOps)
- `cst/` - Concrete Syntax Tree handling and analysis
- `org/` - Organization-level operations (completion, hover, references)
- `types/` - Type system (apex, platform, schema, synthetic types)
- `rpc/` - RPC server and protocol handling

### `com.nawforce.pkgforce` - Package and metadata management
- `sfdx/` - SFDX project handling
- `stream/` - Metadata generators
- `workspace/` - Workspace and layer management

### `io.github.apexdevtools.apexls` - Command-line tools and entry points

## Cross-Platform Design

The project uses Scala's cross-compilation features to support both JVM and JavaScript platforms:

- **Shared code**: Core language analysis logic that works on both platforms
- **Platform-specific code**: File system access (JVM) vs. in-memory operations (JS)
- **Build system**: SBT with cross-compilation settings for seamless builds

## Type System Architecture

The type system is designed around several key concepts:

- **Apex Types**: User-defined classes, interfaces, enums
- **Platform Types**: Salesforce standard objects and system types
- **Schema Types**: Custom objects and fields from Salesforce metadata
- **Synthetic Types**: Generated types for system functionality

## Testing Architecture

- **Unit Tests**: Component-level testing following package structure
- **System Tests**: End-to-end testing against real Apex projects
- **Cross-Platform Tests**: Validation that both JVM and JS implementations work identically