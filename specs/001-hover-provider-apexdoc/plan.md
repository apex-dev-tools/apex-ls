# Implementation Plan: Hover Provider ApexDoc Rendering

**Branch**: `[001-hover-provider-apexdoc]` | **Date**: 2025-10-05 | **Spec**: specs/001-hover-provider-apexdoc/spec.md
**Input**: Feature specification from `/specs/001-hover-provider-apexdoc/spec.md`

## Execution Flow (/plan command scope)
```
1. Load feature spec from Input path
   → If not found: ERROR "No feature spec at {path}"
2. Fill Technical Context (scan for NEEDS CLARIFICATION)
   → Detect Project Type from file system structure or context (web=frontend+backend, mobile=app+api)
   → Set Structure Decision based on project type
3. Fill the Constitution Check section based on the content of the constitution document.
4. Evaluate Constitution Check section below
   → If violations exist: Document in Complexity Tracking
   → If no justification possible: ERROR "Simplify approach first"
   → Update Progress Tracking: Initial Constitution Check
5. Execute Phase 0 → research.md
   → If NEEDS CLARIFICATION remain: ERROR "Resolve unknowns"
6. Execute Phase 1 → contracts, data-model.md, quickstart.md, agent-specific template file (e.g., `CLAUDE.md` for Claude Code, `.github/copilot-instructions.md` for GitHub Copilot, `GEMINI.md` for Gemini CLI, `QWEN.md` for Qwen Code, or `AGENTS.md` for all other agents).
7. Re-evaluate Constitution Check section
   → If new violations: Refactor design, return to Phase 1
   → Update Progress Tracking: Post-Design Constitution Check
8. Plan Phase 2 → Describe task generation approach (DO NOT create tasks.md)
9. STOP - Ready for /tasks command
```

**IMPORTANT**: The /plan command STOPS at step 7. Phases 2-4 are executed by other commands:
- Phase 2: /tasks command creates tasks.md
- Phase 3-4: Implementation execution (manual or via tools)

## Summary
Enhance hover responses so developers see Salesforce-compliant ApexDoc details (summary, tags, sanitized links) for classes, constructors, and methods while ignoring non-conformant comments and preserving existing signature/location output.

## Technical Context
**Language/Version**: Scala 2.13 (cross-project JVM & Scala.js)  
**Primary Dependencies**: Existing Apex parser & ApexDocumentService utilities within apex-link (review reuse)  
**Storage**: N/A (in-memory analysis only)  
**Testing**: ScalaTest (unit/integration), potential Jest regression if JS parity requires fixtures  
**Target Platform**: JVM & Scala.js consumers via shared analysis library  
**Project Type**: Single cross-platform Scala project (shared/, jvm/, js/)  
**Performance Goals**: Hover completion remains sub-100ms for typical files (no noticeable regression)  
**Constraints**: Preserve deterministic output and existing cache behavior; sanitize Markdown-compatible text  
**Scale/Scope**: Apex projects at enterprise scale (thousands of symbols) served via IDE & MCP clients

## Constitution Check
*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- **Salesforce Fidelity**: Confirm ApexDoc parsing mirrors Salesforce grammar; review existing ApexDocumentService or introduce tests against authoritative examples; extend regression suite so `sbt test` covers compliant & malformed docs and ensure `npm run test-samples` remains green.
- **Cross-Platform Parity**: Audit Scala.js build path for hover support; ensure shared doc extraction is reusable or provide Scala.js equivalent with matching output formatting; document any unavoidable divergence and create parity tests.
- **Deterministic Analysis & Cache Hygiene**: Ensure ApexDoc extraction is pure on parsed AST without new mutable caches; verify repeated hovers produce identical sanitized Markdown.
- **Regression Safety Nets**: Add ScalaTest coverage capturing hover output for compliant, partially compliant, malformed, and link-containing ApexDoc; update or create golden expectations so failures catch regressions; rerun `sbt apexlsJVM/test`, `sbt apexlsJS/test`, `npm test`.
- **Performance & Stability at Org Scale**: Measure hover latency impact on representative classes (optional microbenchmark or log capture); confirm no repeated parsing per hover; document findings in research.md and plan profiling if >10% overhead observed.

## Project Structure

### Documentation (this feature)
```
specs/001-hover-provider-apexdoc/
├── plan.md              # This file (/plan command output)
├── research.md          # Phase 0 output (/plan command)
├── data-model.md        # Phase 1 output (/plan command)
├── quickstart.md        # Phase 1 output (/plan command)
├── contracts/           # Phase 1 output (/plan command)
└── tasks.md             # Phase 2 output (/tasks command - NOT created by /plan)
```

### Source Code (repository root)
```
shared/src/main/scala/com/nawforce/apexlink/    # Shared analysis utilities (evaluate for ApexDoc parsing helpers)
jvm/src/main/scala/com/nawforce/apexlink/org/   # HoverProvider, OPM hover wiring
shared/src/main/scala/com/nawforce/apexlink/rpc/ # Hover payload formatting (consider moving HoverItem if needed)
js/src/main/scala/com/nawforce/apexlink/         # Verify Scala.js hover path or add equivalent doc extraction
jvm/src/test/scala/com/nawforce/apexlink/org/    # New hover-focused regression tests
js/src/test/scala/com/nawforce/apexlink/         # Add parity tests if Scala.js hover path updated
```

**Structure Decision**: Single cross-project Scala codebase; core ApexDoc extraction should live in shared logic so JVM and Scala.js builds emit identical hover payloads.

## Phase 0: Outline & Research
1. **Extract unknowns from Technical Context** above:
   - Investigate existing ApexDocumentService/StandardApexDocumentService APIs; determine compatibility with hover usage.
   - Identify current hover support for Scala.js build; confirm call sites in shared vs jvm modules.
   - Review existing sanitization utilities (Markdown-safe rendering) to avoid duplicating logic.

2. **Generate and dispatch research agents**:
   ```
   Research StandardApexDocumentService for retrieving compliant ApexDoc blocks.
   Audit Scala.js hover handling to ensure parity or plan new implementation.
   Identify existing sanitization helpers within project or add lightweight sanitizer requirements.
   ```

3. **Consolidate findings** in `research.md` using format:
   - Decision: Parser/service selected for ApexDoc extraction
   - Rationale: Alignment with Salesforce grammar & reuse
   - Alternatives considered: e.g., custom parser vs existing service; sanitizer libraries

**Output**: research.md with all NEEDS CLARIFICATION resolved

## Phase 1: Design & Contracts
*Prerequisites: research.md complete*

1. **Document data representation** in `data-model.md`:
   - Define hover payload schema (existing signature + ApexDoc segments)
   - Describe sanitized link handling and partial parsing outcomes

2. **Model workflow** in `quickstart.md`:
   - Steps to run analyzer, trigger hover, and observe ApexDoc inclusion
   - Include verification steps for compliant vs malformed doc blocks

3. **Design contracts**:
   - Outline expected hover JSON payload structure in `/contracts/` (example request/response for IDE clients)
   - Capture examples for classes, constructors, methods, and partial ApexDoc cases

4. **Validation assets**:
   - Plan ScalaTest suites that construct Apex sources, trigger hover, and assert sanitized Markdown
   - Determine if snapshot-style fixtures needed for Scala.js or CLI parity testing

5. **Update agent context**:
   - Run `.specify/scripts/bash/update-agent-context.sh codex` to track new tech/process references

**Output**: data-model.md, /contracts/*, failing tests, quickstart.md, agent-specific file

## Phase 2: Task Planning Approach
*This section describes what the /tasks command will do - DO NOT execute during /plan*

**Task Generation Strategy**:
- Load `.specify/templates/tasks-template.md` as base
- Generate tasks from Phase 1 design docs (contracts, data model, quickstart)
- Each contract scenario → hover regression test task [P]
- Sanitization rules → implementation & validation tasks
- Scala.js parity evaluation → dedicated task(s) as needed
- Implementation tasks to integrate ApexDoc extraction and Markdown rendering while preserving existing behavior

**Ordering Strategy**:
- Setup/infra (research assimilation, agent context) → Tests (ScalaTest fixtures, potential JS parity) → Shared utilities (ApexDoc extraction & sanitizer) → HoverProvider integration → Documentation updates & polish
- Respect dependency chain: tests written first, fail until implementation completes
- Mark [P] for tasks touching independent files (e.g., ScalaTest vs documentation)

**Estimated Output**: 15-20 numbered, ordered tasks in tasks.md

**IMPORTANT**: This phase is executed by the /tasks command, NOT by /plan

## Phase 3+: Future Implementation
*These phases are beyond the scope of the /plan command*

**Phase 3**: Task execution (/tasks command creates tasks.md)  
**Phase 4**: Implementation (execute tasks.md following constitutional principles)  
**Phase 5**: Validation (run tests, execute quickstart.md, performance validation)

## Complexity Tracking
*Fill ONLY if Constitution Check has violations that must be justified*

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none)    | —          | —                                   |
