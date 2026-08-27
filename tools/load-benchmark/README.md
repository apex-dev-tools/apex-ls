# Workspace load benchmark

Cold workspace load is the wait before a developer gets any feedback. This harness measures it
repeatably so that a performance claim can cite a number instead of an impression.

It measures only. Nothing here changes how loading works, and there is no CI gate — shared runners
are too noisy to gate on until the baseline variance is understood.

## Parts

| Part | Where | Owns |
|---|---|---|
| `benchmark-load` batch command | `io.github.apexdevtools.apexls.Batch` | one measured load, in JSON |
| `load-benchmark.py` | this directory | fresh JVM per run, heap, repetition, interleaving, statistics, JFR |

The split matters. A timing loop inside sbt would run in a JVM that is already warm and shares a
heap and a garbage collector with the build, so it cannot produce a cold measurement. The driver
starts a new JVM for every run. Because it drives a classpath rather than the build, it can also
measure a released distribution, which is what makes tracking across releases possible.

## Quick start

```sh
export JAVA_HOME=$(/usr/libexec/java_home -v 17)
export SAMPLES=/path/to/apex-samples          # absolute, checked out at v1.4.0

sbt apexlsJVM/build                            # build the classpath to measure

# public baseline corpus
python3 tools/load-benchmark/load-benchmark.py --corpus apex-samples --repetitions 5

# any single workspace, public or private
python3 tools/load-benchmark/load-benchmark.py --workspace /path/to/workspace --repetitions 5
```

The report goes to stdout, progress to stderr. Use `--out FILE` to write it to a file.

## What "cold" means here

Three things vary independently, and a published figure has to say which were controlled.

| | Controlled by | Default |
|---|---|---|
| parsed cache | `--cache off` and an isolated temporary cache directory | off |
| JIT and JVM state | a fresh JVM per run | always |
| OS page cache | not practically controllable, so the first run is discarded | discarded |

The headline measurement is **fresh JVM, no parsed cache, warm page cache, first run discarded**.
That is reproducible and still matches "developer reopens a project".

The cache directory is always an isolated temporary directory created per variant and deleted
afterwards, so a developer's real cache is never read or written. With `--cache on` that directory
starts empty, is written by the discarded first run and read by the rest.

Cache write time is measured but is **not** in the headline: `benchmark-load` loads with autoflush
disabled, times `Org.newOrg` as `totalLoadMs`, and then times the explicit flush separately as
`cacheFlushMs`. Writing the cache is not part of the wait for first feedback.

## Two settings that are easy to get wrong

Both are why this is a designed harness rather than a shell loop around `time`. Neither is left
implicit: `benchmark-load` fails if you do not pass them, and it reports what was used.

1. **Parser.** `CheckForIssues` pins `.withParser("OutlineSingle")`, but the `ServerOps` default,
   and so the editor path, is `OutlineParserMultithreaded`. Timing the CLI as it stands does not
   represent the developer experience. Pass `--parser OutlineMulti` (the harness default) for the
   editor path, `--parser OutlineSingle` to reproduce the CLI.
2. **Unused analysis.** `CheckForIssues` ties it to the detail level, so `-d errors` turns it off,
   while the LSP path defaults it on. Pass `--unused true` (the harness default) for the editor
   path. Run both to size the cost of the pass:

   ```sh
   python3 tools/load-benchmark/load-benchmark.py --workspace WS \
     --variant 'unused-on:unused=true' --variant 'unused-off:unused=false'
   ```

## Reading the results

Load is multithreaded by design, so run to run variance is larger than for a single threaded
benchmark and results are machine dependent.

- Every result records CPU core count, JVM version, heap settings, OS, garbage collectors and the
  apex-ls commit. **Never compare numbers across machines.**
- The driver reports `medianMs` with `minMs`, `maxMs`, `spreadMs` and `madMs`, plus every sample.
  Quote the median with the spread, never a single figure.
- For an A/B comparison use `--variant` twice. Variants are interleaved (ABAB) rather than batched
  (AAABBB), so drift during the run affects both equally.
- **Treat a difference that does not clear the observed spread as no difference.**
- `consistent` is false when two runs of a variant disagreed about the workspace size profile or
  issue counts, which means they were not measuring the same thing. Investigate before quoting.

### Phase breakdown

`phases` aggregates the timed spans inside the load. `totalMs` is summed over every occurrence, so
nested phases overlap by design — `orgCreate` covers the whole load and `classParseFile` is the sum
over individual files, which exceeds wall time when parsing runs on several threads.

| Phase | Covers |
|---|---|
| `orgCreate` | the whole `Org.newOrg` call, cross check against `totalLoadMs` |
| `workspaceScan` | reading `sfdx-project.json` and building the document indexes |
| `moduleDeploy` | deploying every module, the bulk of a cold load |
| `classParseBatch` / `classParseFile` | the class parse pass, in total and per file |
| `triggerParseBatch` | the trigger parse pass |
| `summaryCacheLoad` | loading summary types from the parsed cache |
| `validateFile` | per type validation |
| `validateBodyDeclarations` | detail checking of a type's body declarations, within `validateFile` |
| `validateOuterDependencies` | outer dependency propagation, within `validateFile` |
| `validateMethodMap` | building a type's method map, forced by whatever needs it first |
| `validateConstructorMap` | building a type's constructor map, forced the same way |
| `unusedAnalysis` | the plugin close pass that produces unused diagnostics |

Only these phases are collected. The underlying spans are labelled with file paths, so anything
outside this list is dropped rather than recorded.

### Size profile

`size` describes the workspace itself, so a timing from a codebase nobody else can see stays
interpretable. `packageCount` counts packages including ghosted namespaces declared in
`sfdx-project.json`, `moduleCount` counts the metadata directories actually indexed, and
`metadataFileCount` counts every indexed file including the meta XML that accompanies Apex.
`issues` is a cheap check that two runs analysed the same thing.

### Validation counts

`validation` counts the type resolutions performed while validating, split into the lookups the per
type cache answered and those that reached the resolver, plus the number of validation contexts the
work was spread over. These describe apex-ls' own behaviour rather than the workspace, so they are
safe to report for a private codebase.

### Parallelism

`--parallelism N` bounds the pool the parallel parts of the load run on, by passing
`-Dscala.concurrent.context.{num,min,max}Threads=N` to the fresh JVM. The class parse pool reads
`maxThreads` as an explicit level, so a sweep can go above its default bound of four threads. Every
result records the requested level, the properties actually set, the machine's core count, and
`observedParseThreads`, the number of distinct threads that ran per file parse spans.

`observedParseThreads` is the ground truth: it reports what happened rather than what was asked
for. A speedup versus thread count curve is one run:

```sh
python3 tools/load-benchmark/load-benchmark.py --workspace WS --parallelism 1,2,4,8
```

## Private workspaces

A private codebase can be measured without disclosing anything about it. By default the output
contains **no source, no identifiers and no paths**: the workspace appears as a truncated SHA-256
of its canonical path, the recorded invocation has `<workspace>`, `<cache>`, `<classpath>` and
`<recording>` substituted in, and JVM arguments whose value looks like a path are redacted.

The size profile is reported in full — file counts, Apex type count, module count and total source
bytes — so that a number measured on a codebase nobody else can see is still interpretable later.

Pass `--include-paths` to opt out of the redaction for your own local use.

## Corpus

Two tiers, because the largest realistic codebases are not public.

**Public baseline** is `apex-samples` pinned at `v1.4.0`, the same ref both apex-ls and apex-parser
pin in CI. `corpora.json` names three projects spanning the size range:

| Project | Rough size |
|---|---|
| `apex-recipes` | small, ~140 classes |
| `EDA` | medium, ~625 classes |
| `Cumulus` | large, ~1000 classes |

The driver checks that `$SAMPLES` is at the pinned ref and refuses to run otherwise. Do not update
the checkout to make it pass — the numbers are only comparable against the same content.

The committed baseline is in `baselines/`. It is a record of one machine at one commit, not a
threshold; reproduce it locally before comparing anything to it.

**Private corpus** is any workspace by path, reported as above.

## Profile capture

`--jfr DIR` adds `-XX:StartFlightRecording=filename=...,settings=profile` to every run and writes
one recording per run. JFR ships with the JDK and needs no install.

Recordings are artifacts, not conclusions. Read them with `jfr summary` and `jfr print`, or open
them in VisualVM. The harness deliberately does not parse them.

## Options

| Option | Default | |
|---|---|---|
| `--workspace PATH` | | workspace to measure, repeatable |
| `--corpus NAME` | | named corpus from `corpora.json`, `--list-corpora` to see them |
| `--name NAME` | | name for a single `--workspace` in the report |
| `--repetitions N` | 5 | runs per variant, the first is discarded |
| `--keep-first` | off | keep the first run, for looking at page cache effects |
| `--parser` | `OutlineMulti` | `OutlineMulti` or `OutlineSingle` |
| `--unused` | `true` | unused analysis on or off |
| `--unused-on-error` | `false` | report unused diagnostics on files with errors |
| `--logging` | `none` | `none`, `info`, `debug` or `trace`, anything above `none` distorts timings |
| `--cache` | `off` | `off` for the cold headline, `on` to measure with a warm isolated cache |
| `--parallelism LIST` | | one level, or a comma separated list producing one variant per level |
| `--variant LABEL:K=V,...` | | extra variant to interleave, keys `parser`, `unused`, `unusedOnError`, `logging`, `cache`, `parallelism` |
| `--heap SIZE` | `4g` | `-Xms` and `-Xmx`, recorded in the output |
| `--java PATH` | `$JAVA_HOME/bin/java` | |
| `--classpath GLOB` | `jvm/target/scala-2.13/*` | point at a released distribution to compare versions |
| `--jfr DIR` | | write a Flight Recorder recording per run |
| `--out FILE` | stdout | |
| `--note TEXT` | | free text recorded with the report, e.g. what else the machine was doing |
| `--include-paths` | off | include real paths in the output |

## Running the command directly

The driver is the supported entry point, but one measurement is a plain batch invocation:

```sh
java -Xms4g -Xmx4g -cp 'jvm/target/scala-2.13/*' io.github.apexdevtools.apexls.Batch \
  benchmark-load --workspace /path/to/workspace --no-cache \
  --parser OutlineMulti --unused true --logging none
```

`--no-cache` or `--cache-dir` is required: without one the load would use the developer's real
cache, which would be neither cold nor safe.
