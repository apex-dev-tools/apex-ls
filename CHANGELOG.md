# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- A `CheckForIssues` `--detail errors-and-unused` level that reports errors and unused findings
  while excluding ordinary warnings, for batch/CI consumers that want unused findings to affect
  output and exit status without accepting all warnings. The text, JSON and PMD reports and the
  exit status are now all derived from one selected set of issues, so what is reported and what the
  exit status claims can no longer disagree. `errors`, `warnings` and `unused` keep their existing
  meanings and the exit codes are unchanged (#481)
- A repeatable workspace load benchmark, a `benchmark-load` JVM batch command reporting total load
  time, phase breakdown, size profile and full effective configuration as JSON, driven by
  `tools/load-benchmark` for a fresh JVM per run, fixed heap, repetition, interleaving, bounded
  parallelism and optional JFR capture, with a committed `apex-samples` v1.4.0 baseline (#539)
- Errors for the `@AuraEnabled` shape and placement rules the platform enforces: the full set of
  platform types that cannot be used as an `@AuraEnabled` return, parameter or field type (only
  `Set` was recognised before, so `Map<SObjectType, List<SObject>>` and other common cases were
  missed), `AuraEnabled fields do not support type of X` for fields (properties stay unrestricted),
  `AuraEnabled fields cannot be static` (static properties stay legal), and
  `Non static AuraEnabled methods must be named with a prefix 'get'` (#326)
- Explicit `Location.point`, `Location.wholeLine`, and half-open `Location.span` factories, plus
  source extraction that clamps whole-line sentinels before line endings (#362)
- Source-accurate, half-open lexical ranges for successfully parsed XML elements on JVM and
  Scala.js, exposed through `XMLElementLike.location` while retaining the existing `line` API
  (#535)
- Targeted warnings when a SOQL or child-relationship RecordSet is passed to a scalar SObject
  parameter, where Salesforce runtime success depends on the RecordSet containing exactly one row
  (#394)
- Org-scoped `OpenOptions.withUnusedOnError` configuration for batch/CI callers that need unused
  diagnostics alongside errors, while preserving IDE-friendly suppression by default (#517)
- A versioned JVM batch dispatcher with centralized workspace and cache options, stable JSON
  result/error envelopes, and machine-clean stdout for one-shot analysis integrations (#527)
- JVM batch commands for deterministic dependency reports, scoped dependency counts, and dependency
  bomb rankings (#528)
- Public impacted and declared test-class discovery APIs, plus a deterministic `test-classes` JVM
  batch command with dependency explanations and namespace-qualified names (#529)
- Recognition of `@SuppressWarnings(value='PMD')`, the named form of the annotation parameter. It
  is legal Apex and carries the same meaning as the bare `@SuppressWarnings('PMD')`, but was
  previously ignored, so the suppression had no effect (#541)
- Errors when an inaccessible nested Apex class, interface, or enum is written explicitly in
  source, covering declared, parameter, return, construction, cast, `instanceof`, type literal,
  qualifier, catch, for-loop, switch, extends and implements positions, honouring same-file access
  and the `@TestVisible` unit-test exception. Type reference positions are checked for qualified
  names such as `Outer.Hidden`; a name Apex resolves unqualified through a superclass in another
  file is not reported there (#341)

### Changed

- Annotation parameters are now read as a structured list of names, values, separators and
  locations rather than as a flattened string, from both the outline parser and the ANTLR parser.
  The ANTLR path previously read only a bare value, so a named parameter such as
  `@AuraEnabled(cacheable=true)` was seen as having no parameters at all. No new validation is
  applied to what is read (#541)
- Updated `outline-parser` to 2.1.0 and `apex-parser` to 5.2.0. The apex-parser annotation grammar
  is tightened to match the platform: `@Schema.AuraEnabled`, nested annotations, array-initialiser
  values such as `label={'a','b'}`, and non-literal values such as `@AuraEnabled(cacheable=foo)`
  are now syntax errors (#541)
- Labels, inline SObject components, and XML-derived metadata validation diagnostics now use exact
  element ranges; standalone metadata components continue to use whole-file locations (#362)
- The private parent class check on `extends` now reports `Type is not visible: <type>` against the
  written parent type name rather than the class identifier, and applies the same `@TestVisible`
  and same-file rules as every other explicit type reference (#341)

## [6.2.0] - 2026-07-29

### Added

- Warnings when a static method implements an interface method or inherited abstract method, since
  static implementations of instance contracts can be confusing (#351)
- Documentation and clarified diagnostics for the deliberate difference between apex-ls package-directory handling and Salesforce's merged deployment behaviour (#339)
- `dependencyCountAliases` `sfdx-project.json` option to define named (and optionally nested) presets for `// MaxDependencyCount(...)`, so `// MaxDependencyCount(med)` or `// MaxDependencyCount(group.name)` can be used instead of repeating raw numbers across files (#324)

### Fixed

- Completions from test classes now include accessible `@TestVisible` members (#522)
- Static final assignments from instance initializer blocks now receive targeted advisory guidance
  matching Apex legality, and final-assignment warnings distinguish enforced fields from
  deliberately stricter guidance for locals and parameters (#124)
- `System.FormulaValidationException` and `System.FormulaEvaluationException` now resolve in valid
  catch clauses (#514)
- Duplicate annotations on declarations are now reported as errors, including annotations with different or unrecognized arguments (#287)
- Abstract and override methods with omitted or private visibility now report an error, matching the latest Apex compiler behaviour (#386)
- Set assignments and List/Set constructor initializers now enforce the platform's collection element-type compatibility rules (#318)
- Classes implementing an interface method whose parameter or return type is a ghosted (unavailable dependency package) type no longer report a false `Non-abstract class must implement method` diagnostic when the implementation uses a different, project-local type (#327)
- `addAll`/`removeAll`/`retainAll` on `List`/`Set` (e.g. `Set<Id>.addAll(List<String>)`, such as the result of `String.split`) now require the collection argument's type parameter to exactly match, requiring an explicit cast, matching the Apex compiler; the sole exception, `List<T>.addAll(List<T>)`, is unaffected (#293)

### Removed

- Internal Service Provider Interface (SPI) support used to integrate external analysis tools such as the now-deprecated apex-ls-pmd has been removed. Public APIs that configured external analysis (`ServerOps.getExternalAnalysis`/`setExternalAnalysis`, `OpenOptions.withExternalAnalysisMode`, and the RPC method `setExternalAnalysisMode`) are retained as deprecated no-op stubs for backward compatibility, and now log a deprecation notice (#439)

## [6.1.0] - 2026-07-03

### Added

- Bound variables are now extracted from SOQL `WHERE FORMULA(...)` comparisons (e.g. `WHERE FORMULA('EndDate - StartDate') > :days`), following the apex-parser WHERE AST split; `FORMULA(...)` remains rejected in `HAVING` (#495)
- Recognition of Summer '26 test annotations `@IntegrationTest` and `@TearDown`, including test-class and unused-analysis handling (#468)
- Acceptance of Summer '26 multi-line string literals (`'''...'''`) in expressions and `switch` when clauses (#447)
- Targeted diagnostic for malformed multi-line string literals such as `'''abc'''` and `''''''`, replacing the generic "mismatched input" syntax error (#443)
- Qualified enum constants are now permitted in `switch` when clauses (e.g. `when MyEnum.A`) (#441)
- Distinct exit codes from `CheckForIssues` to separate warnings-only and unused-only outcomes (#440)
- Improved lexer error messages for invalid escape sequences, including the offending sequence

### Fixed

- Missing nested Apex type diagnostics now clarify when the outer type exists but the referenced nested type is not declared (#149)
- Stabilized diagnostic column spans for missing type-reference and duplicate visibility diagnostics, avoiding context-dependent diff churn (#487)
- Nested subclasses now resolve unqualified names to accessible enclosing static fields before inherited private superclass fields, avoiding false `Field is not visible` diagnostics (#488)
- Removed use of the deprecated Apex parser `CaseInsensitiveInputStream` from JVM parsing (#489)
- Private and protected overloaded methods called with ghosted-type arguments no longer report false `Method is not visible` diagnostics (#484)
- Nested classes extending an externally nested base no longer resolve unqualified static field names through the base class's enclosing type, avoiding false `Field is not visible` diagnostics (#482)
- Unused method/field/type warnings are now recomputed for cache-loaded classes instead of replaying the cached result, so they reflect actual usage in the current workspace rather than a stale whole-program result captured when the cache was written (#477)
- Private and protected field/method accesses from unrelated classes are now reported as visibility errors (#474)
- SOQL queries using deprecated `WITH SECURITY_ENFORCED` now report a warning recommending `WITH USER_MODE` (#466)
- Private `@TestVisible` methods, fields, and constructors are now only visible from `@IsTest` callers, matching Salesforce visibility rules for non-test and `@IntegrationTest` code (#471)
- `@IntegrationTest` classes no longer reject ordinary helper members, and calls to `@IntegrationTest` methods from non-integration contexts are reported as warnings instead of deploy-blocking errors (#470)
- Unused warnings for public static methods now call out their static nature and explain how to document intentional external entry points (#406)
- Unused warnings for virtual/override method hierarchies now include context when all related overrides are unused (#403)
- Global interface methods now inherit their containing interface visibility, preventing valid implementations from being incorrectly flagged as unused (#405)
- Verbose parser diagnostics at end-of-file now report `Unexpected end of input` while preserving concise expected-token messages (#457)
- Cascading syntax errors after an unclosed method body are now suppressed (#422)
- Static methods on inner classes are now properly validated and flagged (@metalshark)
- Public/global methods implementing interfaces from external namespaces are no longer incorrectly flagged as unused (#401)
- Methods invoked only from triggers are no longer flagged as unused
- Test class discovery now follows interface use relationships, so test classes referenced via interfaces are correctly included
- Outline parser column offsets aligned with outline-parser 2.0 0-based half-open columns, removing off-by-one diagnostics
- Outline parser validation failures now retry via an ANTLR fallback to preserve diagnostics

### Changed

- Upgraded platform types to Salesforce Summer '26 via standard-types and sobject-types 67.0.0
- Removed the ANTLR-first parsing mode; OutlineParser is now the sole parser path. The `--antlr` / ANTLR parser option is deprecated and a no-op (#433)
- Upgraded to apex-parser 5.1.0 and the antlr4 4.13 runtime
- Slimmed the JS module surface to the published apex-ls facades
- Removed JS-portability shims `CodeParser.toScala(value)` and `CodeParser.getText(...)` from the CST construction layer; call sites now use `Option(...)` directly (#449)
- Replaced cascading `Option(...).orElse(...)` chains in `Literal.construct` and `ClassBodyDeclaration.construct` with extractor-based pattern matching (#449)

### Deprecated

- The npm distribution (`@apexdevtools/apex-ls`) is no longer published by default and is planned for removal; the JVM artifact on Maven Central is the supported distribution. Contact the maintainers if you still require an npm build.

### Removed

- MCP server support and packaging (#454)
- v1 ForceIgnore implementation (V2 has been the default since 6.0.0)

### Security

- Upgraded the JS runtime's `@xmldom/xmldom` dependency from 0.7.9 to 0.8.13, clearing several XML serialization-injection and denial-of-service advisories

## [6.0.2] - 2025-11-25

### Fixed

- Loop variables used only in iteration control (condition/increment) are no longer incorrectly flagged as unused (#397)
- Variables used in ghosted type list initializers are no longer incorrectly flagged as unused (#398)

## [6.0.1] - 2025-10-21

- Functionally identical to 6.0.0, testing release process

## [6.0.0] - 2025-10-16

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
- Prevent validation map generation failures when trigger or class validation throws (#312)
- For loop iteration over getSObjects() method results now works correctly (#328)
- Improved forceIgnoreVersion validation and cleanup of legacy code
- Enhanced pattern matching optimization in ForceIgnoreV2

### Changed

- Refactored logger architecture for improved modularity, extensibility, and plugin support
- Updated to API v65 platform types

### Removed

- MDAPI workspace support (#366)
  - All projects now require sfdx-project.json configuration

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
