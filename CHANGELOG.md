# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- CHANGELOG.md to track project changes (#375)
- @AuraEnabled method validation - ensures methods are public or global (#333)
- Test modifier validation for @IsTest and @TestSetup annotations (#333)
  - Classes must be annotated with @IsTest if methods are
  - @IsTest and testMethod methods must be static
- Library project support in sfdx-project.json (#363)
  - Suppresses unused warnings for public entities in library projects
  - Support for external metadata paths configuration
  - Plugin system refactoring to handle library-specific behavior
- Break/continue statement validation outside loops (#378)
- ForceIgnore version configuration in sfdx-project.json (#371)
- ForceIgnoreV2 with exact node-ignore 5.3.2 compatibility (#369)
  - Now the default ignore handler for improved pattern matching
- Interface method overload validation (#368)
- MCP (Model Context Protocol) server for AI tool integration (#356)
- npm wrapper package for MCP server deployment and related CI/test improvements (#379)

### Fixed
- Loop variable unused detection now correctly flags variables only used in for-loop conditions/increments (#330)

### Removed
- MDAPI workspace support (#366)
  - All projects now require sfdx-project.json configuration

### Fixed
- Improved forceIgnoreVersion validation and cleanup of legacy code
- Enhanced pattern matching optimization in ForceIgnoreV2

### Changed
- Refactored logger architecture for improved modularity, extensibility, and plugin support


## [5.10.0] - 2025-07-24

### Added
- SObject definition navigation support
- API v64 platform type updates

### Fixed
- SObject map assignment validation errors (#340)
- SuppressWarnings('Unused') annotation now properly applies to local variables (#353)
- Unused warnings for local variables used in for loops (#350)
- Missing toString method implementation for custom enums (#349)

### Changed
- Improved generic parameter assignability checking
- Enhanced performance of package type finding
- Updated to use lazy val for class metadata hashing
- Improved reference handling for type references, fields, properties, and enum constants

## [5.9.0] - 2025-03-13

### Added
- Parent field support for related SObject types in special tracking objects

### Fixed
- Ghosted SObject type handling in schema and describe results (#315)
- References to ghosted types on schema fields
- Platform type compatibility issues

### Changed
- Updated to API v63 platform types
- Updated Scala and plugin versions
- Fixed security vulnerabilities in cross-spawn dependency

## [5.8.0] - 2025-01-23

### Added
- Refresh listener functionality to org API

### Fixed
- High priority refresh requests getting lost in indexer queue
- Single high priority requests not flushing to cache properly

### Changed
- Improved completion handling of visibility modifiers

## [5.7.0] - 2024-12-13

### Added
- Enhanced validation failure logging with Apex code context (#281)
- Custom Type Declaration cache for improved schema search performance
- Performance improvements for type finding and EncodedName construction

### Fixed
- Declaration cache removal issues
- Over-size limits handling for Decimal, Long, and Integer types

### Changed
- Centralized validate exception logging in TypeDeclaration
- Improved super class and interface declaration caching
- Enhanced type finding performance optimizations

## [5.6.1] - 2024-11-28

### Fixed
- Missing toString default method for enum types
- Updated platform types to API version 62.0.1

## [5.6.0] - 2024-11-28

### Added
- Method override change detection for v61 API updates (#285)
- Enhanced warnings for GACK scenarios and ineffective overrides  
- Support for sobject type references in switch statements
- Missing EventUuid standard field for platform events

### Fixed
- Non-deterministic metadata file validation errors
- Method visibility handling corrections
- Private method override behavior adapted for v61 changes

### Changed
- Converted private method override errors to warnings
- Refactored MethodMap for better performance
- Updated to apex-parser 4.3.1
- Enhanced method visibility validation rules

## [5.5.0] - 2024-09-19

### Added
- Warning notifications for reserved method names
- Enhanced completion handling for field/variable declarations
- Updated to apex-parser 4.2.0 with new language features

### Changed
- Updated build pipelines to use Node.js 20
- Fixed security vulnerabilities in package dependencies
- Improved completion support for various contexts

## [5.4.0] - 2024-08-12

### Added
- Cache directory override option for CLI tools
- Reserved identifier validation for method names and formal parameters
- Protection against empty cache paths

### Fixed
- While statement creation when conditions exist
- Method name identifier verification
- Formal parameter identifier validation

### Changed
- Enhanced CLI with cache directory configuration options

## [5.3.0] - 2024-07-01

### Changed
- Updated GitHub Actions build configurations
- Enhanced publish actions with base64 secret handling
- Updated platform API types to latest versions

## [5.2.0] - 2024-04-24

### Added
- Updated to apex-parser 4.0 for trigger parsing corrections (#269)
- Enhanced CST construction improvements

### Changed
- Corrected import handling and CST construction
- Updated platform types and dependencies

## [5.1.0] - 2024-03-25

### Added
- Null coalesce operator support (#264)
- Updated platform APIs to latest version (#267)

### Fixed
- Custom settings missing static methods (#266)

### Changed
- Enhanced platform API compatibility
- Improved language feature support with parser updates

---

*For older changes, please refer to the git commit history or GitHub releases.*
