# Phase 0 Research – Hover Provider ApexDoc Rendering

## Decisions

### 1. ApexDoc Source Retrieval
- **Decision**: Use the existing `SourceOps.loadFullSourceAndType` pipeline to fetch both the raw class source string and the `ApexFullDeclaration`; reuse the declaration’s `source: Source` metadata when needing slice-level context.
- **Rationale**: `HoverProvider` already calls `loadFullSourceAndType`, giving us the full file in-memory without additional disk IO, and `ApexFullDeclaration` exposes precise `Location` values for members. Leveraging these avoids duplicating parsing logic and keeps behavior deterministic across JVM/Scala.js.
- **Alternatives Considered**:
  1. Re-read files from disk on demand – rejected because it breaks determinism under unsaved-editor content and contradicts cache hygiene.
  2. Extend `ApexMethodDeclaration` to cache doc text during CST construction – rejected for Phase 0 since it requires wide refactors and risks touching validation orderings.

### 2. ApexDoc Parsing Strategy
- **Decision**: Implement a lightweight ApexDoc parser in `shared/src/main/scala/com/nawforce/apexlink/hover`, scanning upward from the target `Location` to capture the nearest `/** … */` block, then tokenize by tags (`@param`, `@return`, `@throws`, `@author`, etc.).
- **Rationale**: No existing parser is available in the codebase, and Salesforce ApexDoc aligns closely with JavaDoc, making a deterministic tokenizer feasible. Working in `shared` ensures a single implementation feeds both JVM and Scala.js builds, satisfying parity and maintenance goals.
- **Alternatives Considered**:
  1. Depend on Salesforce’s `StandardApexDocumentService` (seen only in tests) – rejected because that implementation is packaged with proprietary binaries and not available cross-platform.
  2. Embed a third-party JavaDoc parser – rejected for license complexity and likely lack of Scala.js compatibility.

### 3. Handling Partially Malformed Docs
- **Decision**: Parse the block line-by-line; if the opening `/**` or closing `*/` is missing the block is ignored, but if specific tags fail validation we drop only those lines while keeping the rest. Missing tag arguments (e.g., `@param` without a name) mark that tag invalid.
- **Rationale**: Aligns with the clarified requirement to surface valid segments even when the overall block is imperfect, while still guaranteeing users never see malformed structures.
- **Alternatives Considered**:
  1. Strict all-or-nothing parsing – rejected because it contradicts the clarified acceptance criteria.
  2. Best-effort regex extraction – rejected for being too brittle and hard to unit test across nested/multiline tags.

### 4. Markdown Sanitization & Link Handling
- **Decision**: Produce Markdown output by escaping HTML entities and normalizing whitespace. For links we will retain only `http`/`https` URIs, encode brackets/parentheses, and render as `[label](url)`; anything else falls back to plain text.
- **Rationale**: Keeps hover payloads safe for Markdown renderers while meeting the requirement to expose external links in sanitized form. Sanitizing in the shared module maintains JVM/Scala.js parity and keeps unit tests consistent.
- **Alternatives Considered**:
  1. Allow raw HTML from ApexDoc – rejected for injection risk.
  2. Strip all links – rejected because the clarified request explicitly asks to display them.

### 5. Output Composition Strategy
- **Decision**: Build hover payloads as a Markdown string composed of the existing signature (`td.toString`) followed by an ApexDoc section (summary, parameters, returns, throws, author, since). Empty sections are omitted, and headings are localized (e.g., `**Summary**`).
- **Rationale**: Maintains backward compatibility for clients relying on the signature while appending structured documentation in a predictable format that unit tests can snapshot.
- **Alternatives Considered**:
  1. Replace the signature entirely with documentation – rejected to avoid regressions in IDE tooltips.
  2. Return structured JSON in `HoverItem` – rejected because it requires protocol changes across clients.

### 6. Cross-Platform Parity Plan
- **Decision**: Place the formatter and sanitizer utilities in `shared` under the new `com.nawforce.apexlink.hover` package, then expose minimal entry points consumed by JVM (`HoverProvider`) and Scala.js (future hover handlers). Mirrored ScalaTest/Jest tests will assert identical Markdown outputs across platforms.
- **Rationale**: Satisfies the constitution’s parity clause and ensures a single source of truth for documentation rendering.
- **Alternatives Considered**:
  1. Implement separate JVM and Scala.js formatters – rejected for maintenance overhead and risk of divergence.

### 7. Performance & Determinism Baseline
- **Decision**: Measure hover latency on a representative apex-samples class before and after the change, logging the results in `research.md` and ensuring deviation remains under 10%. Doc extraction will operate on in-memory strings without extra IO to preserve determinism.
- **Rationale**: Aligns with the Performance & Stability principle and provides evidence the new parsing work does not regress hover responsiveness.
- **Alternatives Considered**:
  1. Skip measurement – rejected per constitution requirements.

## Performance Baseline Plan
- Baseline hover command: `scripts/hover-cli.sh samples/classes/InvoiceProcessor.cls 15 5 --format markdown` (pre-change).
- Capture median/95th percentile latency across five runs and record under "Hover Latency" table in this file.
- Repeat the same measurement post-change; regression above 10% requires profiling notes and mitigation before merge.

## Open Questions
- None; all clarifications received are reflected above.

## Next Steps
1. Draft `ApexDocFormatter` API surface in shared module (inputs: source text, `Location`, `Locatable` kind; outputs: structured ApexDoc model + Markdown string).
2. Capture baseline hover timings against apex-samples before implementing formatter to compare later.
3. Move to Phase 1 deliverables (data-model.md, contracts, quickstart) using the decisions documented here.
