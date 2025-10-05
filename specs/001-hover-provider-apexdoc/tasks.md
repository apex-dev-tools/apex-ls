# Tasks: Hover Provider ApexDoc Rendering

**Input**: Design documents from `/specs/001-hover-provider-apexdoc/`
**Prerequisites**: plan.md (required), research.md, data-model.md, contracts/

## Execution Flow (main)
```
1. Load plan.md from feature directory
   → If not found: ERROR "No implementation plan found"
   → Extract: tech stack, libraries, structure
2. Load optional design documents:
   → data-model.md: Extract entities → model tasks
   → contracts/: Each file → contract test task
   → research.md: Extract decisions → setup tasks
3. Generate tasks by category:
   → Setup: project init, dependencies, linting
   → Tests: contract tests, integration tests
   → Core: models, services, CLI commands
   → Integration: DB, middleware, logging
   → Polish: unit tests, performance, docs
4. Apply task rules:
   → Different files = mark [P] for parallel
   → Same file = sequential (no [P])
   → Tests before implementation (TDD)
5. Number tasks sequentially (T001, T002...)
6. Generate dependency graph
7. Create parallel execution examples
8. Validate task completeness:
   → All contracts have tests?
   → All entities have model tasks?
   → All endpoints implemented?
9. Return: SUCCESS (tasks ready for execution)
```

## Phase 3.1: Setup
- [ ] T001 Summarize ApexDoc parser + sanitizer decisions in `specs/001-hover-provider-apexdoc/research.md` (already captured; verify and append baseline timing plan if missing).
- [ ] T002 Run `.specify/scripts/bash/update-agent-context.sh codex` after research confirmation to capture any new tech/process notes.

## Phase 3.2: Tests First (TDD)
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation**
- [ ] T003 Add ScalaTest `HoverProviderApexDocTest` covering compliant ApexDoc hover output in `jvm/src/test/scala/com/nawforce/apexlink/org/HoverProviderApexDocTest.scala`.
- [ ] T004 Extend the same suite with partial ApexDoc coverage asserting malformed sections are omitted yet valid summary renders.
- [ ] T005 Add sanitized link coverage to the suite ensuring external links render safely (no raw HTML) in hover Markdown output.
- [ ] T006 Add invalid-doc regression case to the suite confirming non-ApexDoc comments return default hover payload only.
- [ ] T007 [P] Introduce shared-format expectations (e.g., helper assertions) in `shared/src/test/scala/com/nawforce/apexlink/hover/ApexDocFormatterTest.scala` for sanitizer edge cases.
- [ ] T008 [P] Create Scala.js parity check (mirroring sanitizer behavior) in `js/src/test/scala/com/nawforce/apexlink/hover/ApexDocFormatterJSTest.scala` to keep JVM/JS outputs aligned.

## Phase 3.3: Core Implementation (ONLY after tests are failing)
- [ ] T009 Implement ApexDoc extraction & partial parsing helper in `shared/src/main/scala/com/nawforce/apexlink/hover/ApexDocFormatter.scala` using existing Apex parser services.
- [ ] T010 Add Markdown sanitization utilities (including safe link formatting) in `shared/src/main/scala/com/nawforce/apexlink/hover/MarkdownSanitizer.scala` and export for JVM/JS use.
- [ ] T011 Wire `HoverProvider` in `jvm/src/main/scala/com/nawforce/apexlink/org/HoverProvider.scala` to call the new formatter and combine sanitized ApexDoc with existing signature/location data.
- [ ] T012 Ensure Scala.js hover pathway (or equivalent shared service) consumes the same formatter; update/create entry point in `js/src/main/scala/com/nawforce/apexlink/hover/HoverDocProvider.scala`.
- [ ] T013 Preserve JSON serialization by updating `HoverItem` handling if needed in `jvm/src/main/scala/com/nawforce/apexlink/rpc/HoverItem.scala` (include Markdown indicator/documentation).
- [ ] T014 Document sanitizer behavior within the shared module (scaladoc) noting Salesforce ApexDoc compliance expectations.

## Phase 3.4: Integration & Performance
- [ ] T015 Validate end-to-end hover behavior via integration harness (existing hover invocation) and capture before/after samples in `specs/001-hover-provider-apexdoc/quickstart.md`.
- [ ] T016 Measure hover latency using representative org sample, record results in `specs/001-hover-provider-apexdoc/research.md`, and confirm <10% regression.
- [ ] T017 Execute Scala.js packaging (`sbt apexlsJS/test`) to ensure sanitizer changes compile and pass parity tests.
- [ ] T018 Execute JVM test suite (`sbt apexlsJVM/test`) verifying new hover logic passes.
- [ ] T019 Run full regression commands (`sbt test`, `npm test`, `npm run test-samples`) and attach summary outputs to the PR.

## Phase 3.5: Polish
- [ ] T020 Update developer documentation to describe ApexDoc hover support (e.g., add section to `README.md` and `doc/API_Updates.md`).
- [ ] T021 Review for deterministic output by re-running targeted hover scenarios twice and documenting identical results in `specs/001-hover-provider-apexdoc/quickstart.md`.
- [ ] T022 Final refactor/cleanup: remove dead code, ensure scalafmt via `sbt scalafmtAll`, and prepare changelog entry in `CHANGELOG.md`.

## Dependencies
- Tests T003-T008 must precede implementation tasks T009-T013.
- T009 depends on research decisions (T001).
- T011 depends on sanitizer and formatter implementations (T009, T010).
- T012 depends on shared helpers (T009, T010) and parity test scaffolding (T008).
- Integration/performance tasks (T015-T019) depend on all code changes being complete (T009-T013) and tests passing.
- Polish tasks (T020-T022) follow successful integration validation.

## Parallel Example
```
# After completing T009-T010:
Task: "T015 Validate end-to-end hover behavior via integration harness"
Task: "T016 Measure hover latency using representative org sample"
Task: "T017 Execute Scala.js packaging (sbt apexlsJS/test)"
Task: "T018 Execute JVM test suite (sbt apexlsJVM/test)"
```

## Notes
- [P] tasks = different files, no dependencies
- Verify tests fail before implementing
- Commit after each task
- Avoid: vague tasks, same file conflicts

## Validation Checklist
*GATE: Checked by main() before returning*

- [ ] All contracts have corresponding tests
- [ ] All entities have model tasks
- [ ] All tests come before implementation
- [ ] Parallel tasks truly independent
- [ ] Each task specifies exact file path
- [ ] No task modifies same file as another [P] task
- [ ] JVM and Scala.js changes include explicit parity tasks or justifications
- [ ] Performance-sensitive work includes tasks for baseline comparison or profiling artifacts
- [ ] Cache or CLI updates include deterministic verification or snapshot refresh tasks
