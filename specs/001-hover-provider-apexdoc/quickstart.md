# Quickstart — Hover Provider ApexDoc Rendering

## Prerequisites
- Java 17 toolchain with Java 8 target compatibility (per project standard).
- `sbt` and `npm` available on PATH.
- Optional: `SAMPLES` environment variable pointing to apex-samples checkout for regression timing.

## Verify Current Baseline
1. Run `sbt "project apexlsJVM" testOnly *Hover*` and note existing hover-related test output (expected to ignore ApexDoc today).
2. Record baseline hover latency for a representative class:
   ```bash
   ./scripts/hover-benchmark.sh path/to/Class.cls 15 5  # custom helper you create locally
   ```
   Capture median latency in `specs/001-hover-provider-apexdoc/research.md`.

## Implement ApexDoc Formatter
1. Create failing ScalaTest suites per tasks T003–T008:
   ```bash
   sbt "project apexlsJVM" testOnly com.nawforce.apexlink.org.HoverProviderApexDocTest
   sbt "project apexlsJS"  testOnly com.nawforce.apexlink.hover.ApexDocFormatterJSTest
   ```
   Ensure failures confirm missing ApexDoc rendering.
2. Implement shared formatter and sanitizer modules in `shared/src/main/scala/com/nawforce/apexlink/hover/`.
3. Update `HoverProvider` and Scala.js hover entry point to consume the formatter.

## Validate Feature End-to-End
1. Re-run unit suites:
   ```bash
   sbt apexlsJVM/test
   sbt apexlsJS/test
   npm test --prefix js/npm
   ```
2. Execute hover smoke test via CLI client or IDE:
   ```bash
   scripts/hover-cli.sh path/to/Class.cls 18 3 --format markdown
   ```
   Confirm the output includes formatted Summary/Parameters/Returns sections with sanitized links.
3. Re-run latency measurement and compare against baseline (<10% regression). Document results in `research.md`.

## Regression Sweep
1. Run the cross-build to ensure packaging succeeds:
   ```bash
   sbt build
   ```
2. Execute sample-based tests (optional but recommended):
   ```bash
   export SAMPLES=/abs/path/to/apex-samples
   npm run test-samples --prefix js/npm
   ```
3. Update documentation (README, API notes) to announce ApexDoc hover support.

## Hand-off Checklist
- [ ] All new tests pass (and were failing before implementation).
- [ ] Deterministic hover outputs verified by re-running hover scenarios twice without diff.
- [ ] Performance notes and sanitized output examples added to `quickstart.md` and PR description.
