<!--
Sync Impact Report
Version change: N/A → 1.0.0
Modified principles: (initial publication)
Added sections: Core Principles, Platform Constraints, Development Workflow, Governance
Removed sections: none
Templates requiring updates:
✅ .specify/templates/plan-template.md (constitution gates aligned with v1.0.0)
✅ .specify/templates/spec-template.md (reviewed; no change needed)
✅ .specify/templates/tasks-template.md (constitution references verified)
Follow-up TODOs: none
-->
# Apex Language Server Constitution

## Core Principles

### I. Salesforce Fidelity (NON-NEGOTIABLE)
- MUST keep analyzer output identical to Salesforce Apex and metadata semantics for all constructs covered by the apex-samples regression suite; merge is blocked until `sbt test` and `npm run test-samples` both pass on the change.
- MUST default to the last released behavior when introducing divergent semantics; new flags or configuration keys require migration notes in the release summary and README.
- MUST surface known deviations in documentation prior to release so downstream IDEs can guard against them.
Rationale: Partner IDEs and CI pipelines adopt apex-ls only when it is a drop-in proxy for Salesforce analysis.

### II. Cross-Platform Parity
- MUST deliver matching features, CLI exits, and JSON schemas across JVM and Scala.js builds; if an API is platform-specific it must be clearly namespaced and feature-flagged.
- MUST add parity tests or shared fixtures whenever new platform-specific code is introduced; PRs fail if one target lacks equivalent coverage.
- MUST track bundle size and dependency drift; variance >10% versus the previous release requires an issue with mitigation steps before merge.
Rationale: Consumers embed either target interchangeably, so divergence immediately fractures integrations.

### III. Deterministic Analysis & Cache Hygiene
- MUST ensure identical inputs (workspace, API version, configuration, dependency digests) always yield identical diagnostics and cache artifacts.
- MUST restrict writable state to `APEXLINK_CACHE_DIR`, workspace-local `.apexlink_cache`, or explicitly documented temp directories; no other global paths permitted.
- MUST use file locks or atomic moves when multiple clients/watchers may touch the same cache entry to prevent corruption.
Rationale: Teams run apex-ls in CI and editors concurrently; determinism keeps results trustworthy.

### IV. Regression Safety Nets
- MUST pin a failing regression test (ScalaTest or Jest) for every bug fix before marking the issue resolved.
- MUST keep `sbt test`, targeted `sbt apexlsJVM/test`, `sbt apexlsJS/test`, and `npm test` green on the main branch; partial passes are not treated as success criteria.
- MUST capture snapshot diffs from `npm run test-samples` in the PR discussion when behavior changes, with explicit reviewer sign-off.
Rationale: Continuous regression evidence is the only guard against silent analyzer drift.

### V. Performance & Stability at Org Scale
- MUST baseline runtime and peak memory for `CheckForIssues` and `DependencyReport` on apex-samples; deltas >10% require profiling artifacts attached to the PR.
- MUST fail fast with actionable errors when encountering Salesforce-imposed limits (e.g., dependency count) instead of degrading silently.
- MUST ensure incremental analysis completes under the concurrency model documented in `README.md`; blocking operations need progress logging at INFO level.
Rationale: Enterprise Salesforce orgs depend on predictable performance and transparent failure modes.

## Platform Constraints
- Source of truth lives in `shared/src`, `jvm/src`, and `js/src`; shared logic belongs in `shared/src` unless platform APIs prevent parity.
- Scala 2.13 cross-compilation is mandatory; any Scala 3 or JVM-only feature requests must be rejected unless the Modules plan in `.claude/MODULES.md` is amended first.
- JVM artifacts must retain Java 8 bytecode compatibility while building under Java 17; release checks include validating class file targets.
- JavaScript packaging under `js/npm/` ships CommonJS modules; changes impacting bundling require updating the NPM README and sample configuration.
- MCP bridge code in `mcp/` MUST stay aligned with the published Model Context Protocol schema; experimental endpoints need documentation tags and fallback behavior.

## Development Workflow
- Run `sbt build` before publishing or invoking MCP builds to validate cross-target packaging.
- Execute `sbt test`, `sbt apexlsJVM/test`, `sbt apexlsJS/test`, and `npm test` locally before merging; CI failures cannot be waived without a maintainer vote recorded in the PR.
- Record performance baseline metrics when touching parser, dependency resolution, or cache code, and store them with the PR for future comparisons.
- Use `sbt scalafmtAll` prior to commit to enforce formatting, and ensure sample-based system tests (`npm run test-samples`) pass with snapshots reviewed.
- Document new configuration options or flags in `README.md` and `doc/` before release tagging.

## Governance
- Amendments require approval from two maintainers plus one reviewer from the downstream tooling community (e.g., VSCode or MCP consumers) and must document migration steps.
- Version bumps follow semantic rules: MAJOR for principle removals or redefinitions, MINOR for new principles or sections, PATCH for clarifications.
- Compliance is reviewed during code review by referencing the Constitution Check in feature plans and confirmed in release retrospectives every quarter.
- Ratified principles supersede conflicting workflow docs; inconsistencies must be resolved by amending the conflicting doc or the constitution.

**Version**: 1.0.0 | **Ratified**: 2025-10-05 | **Last Amended**: 2025-10-05
