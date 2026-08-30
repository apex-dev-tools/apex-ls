#!/usr/bin/env python3
"""Repeatable workspace load benchmark driver for apex-ls.

Owns everything that cannot be done inside a single JVM: a fresh JVM per run, fixed heap
settings, repetition, interleaving of variants, statistics and optional JDK Flight Recorder
capture. The measurement itself is the `benchmark-load` batch command, which reports total load
time, a phase breakdown, the workspace size profile and the full effective configuration.

See README.md for usage and for the conventions a published figure has to state.
"""

from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import statistics
import subprocess
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path

SCHEMA_VERSION = 2
BATCH_MAIN = "io.github.apexdevtools.apexls.Batch"
TOOL_DIR = Path(__file__).resolve().parent
REPO_DIR = TOOL_DIR.parent.parent

VARIANT_KEYS = (
    "parser",
    "unused",
    "unusedOnError",
    "logging",
    "cache",
    "parallelism",
    "blockPrefetchThreads",
)


class BenchmarkError(Exception):
    pass


# --- configuration ----------------------------------------------------------------------------


def parse_args(argv):
    parser = argparse.ArgumentParser(
        description="Measure apex-ls workspace load time with a fresh JVM per run.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--workspace",
        action="append",
        default=[],
        metavar="PATH",
        help="workspace directory to measure, repeatable",
    )
    parser.add_argument("--corpus", metavar="NAME", help="named corpus from corpora.json")
    parser.add_argument("--list-corpora", action="store_true", help="list corpora and exit")
    parser.add_argument("--name", metavar="NAME", help="name for a single --workspace")
    parser.add_argument(
        "--repetitions", type=int, default=5, metavar="N", help="runs per variant (default 5)"
    )
    parser.add_argument(
        "--keep-first",
        action="store_true",
        help="include the first run, which is normally discarded to warm the OS page cache",
    )
    parser.add_argument("--parser", default="OutlineMulti", choices=["OutlineMulti", "OutlineSingle"])
    parser.add_argument("--unused", default="true", choices=["true", "false"])
    parser.add_argument("--unused-on-error", default="false", choices=["true", "false"])
    parser.add_argument("--logging", default="none", choices=["none", "info", "debug", "trace"])
    parser.add_argument(
        "--cache",
        default="off",
        choices=["on", "off"],
        help="'off' is the cold headline measurement, 'on' uses an isolated temporary cache",
    )
    parser.add_argument(
        "--parallelism",
        default="",
        metavar="LIST",
        help="bounded parallelism, a single level or a comma separated list producing one variant "
        "per level",
    )
    parser.add_argument(
        "--block-prefetch-threads",
        default="",
        metavar="N",
        help="threads used to parse method bodies ahead of validation, one of 0, 2 or 4",
    )
    parser.add_argument(
        "--variant",
        action="append",
        default=[],
        metavar="LABEL:KEY=VALUE,...",
        help="additional variant to interleave, keys: " + ", ".join(VARIANT_KEYS),
    )
    parser.add_argument("--heap", default="4g", metavar="SIZE", help="-Xms and -Xmx (default 4g)")
    parser.add_argument("--java", default=default_java(), metavar="PATH")
    parser.add_argument(
        "--classpath",
        default=str(REPO_DIR / "jvm" / "target" / "scala-2.13" / "*"),
        metavar="GLOB",
        help="classpath to measure, point at a released distribution to track regressions",
    )
    parser.add_argument("--jfr", metavar="DIR", help="write a Flight Recorder recording per run")
    parser.add_argument("--out", metavar="FILE", help="write the report here instead of stdout")
    parser.add_argument(
        "--note",
        metavar="TEXT",
        help="free text recorded with the report, e.g. what else the machine was doing",
    )
    parser.add_argument(
        "--include-paths",
        action="store_true",
        help="include workspace and cache paths in output, off by default so a private workspace "
        "can be measured without disclosing anything about it",
    )
    return parser.parse_args(argv)


def default_java():
    java_home = os.environ.get("JAVA_HOME")
    return str(Path(java_home) / "bin" / "java") if java_home else "java"


def load_corpora():
    with open(TOOL_DIR / "corpora.json", encoding="utf-8") as handle:
        return json.load(handle)


def resolve_corpus(name):
    corpora = load_corpora()
    if name not in corpora:
        raise BenchmarkError(f"Unknown corpus '{name}', known: {', '.join(sorted(corpora))}")
    corpus = corpora[name]
    root = os.path.expandvars(corpus["root"])
    if root.startswith("$"):
        raise BenchmarkError(f"Corpus '{name}' needs {corpus['root']} to be set")
    root_path = Path(root).resolve()
    commit = verify_corpus_ref(name, root_path, corpus["ref"])
    workspaces = []
    for project in corpus["projects"]:
        path = root_path / project["path"]
        if not (path / "sfdx-project.json").is_file():
            raise BenchmarkError(f"Corpus '{name}' project '{project['name']}' is not at {path}")
        workspaces.append((project["name"], path))
    return workspaces, {"name": name, "ref": corpus["ref"], "commit": commit}


def verify_corpus_ref(name, root_path, ref):
    """The corpus is pinned, a moved checkout would silently change what the numbers mean."""
    head = git(root_path, "rev-parse", "HEAD")
    pinned = git(root_path, "rev-parse", f"{ref}^{{commit}}")
    if head is None or pinned is None:
        raise BenchmarkError(f"Corpus '{name}' at {root_path} is not a git checkout")
    if head != pinned:
        raise BenchmarkError(
            f"Corpus '{name}' at {root_path} is at {head[:12]}, expected {ref} ({pinned[:12]}). "
            "Check the corpus out at the pinned ref rather than updating it."
        )
    return head


def git(directory, *args):
    try:
        result = subprocess.run(
            ["git", "-C", str(directory), *args],
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return None
    return result.stdout.strip() if result.returncode == 0 else None


def build_variants(args):
    levels = [level.strip() for level in args.parallelism.split(",") if level.strip()]
    base = {
        "parser": args.parser,
        "unused": args.unused,
        "unusedOnError": args.unused_on_error,
        "logging": args.logging,
        "cache": args.cache,
        "parallelism": levels[0] if len(levels) == 1 else "",
        "blockPrefetchThreads": args.block_prefetch_threads,
    }

    if args.variant:
        if len(levels) > 1:
            raise BenchmarkError("Use either a --parallelism list or --variant, not both")
        return [parse_variant(spec, base) for spec in args.variant]
    if len(levels) > 1:
        return [(f"parallelism-{level}", dict(base, parallelism=level)) for level in levels]
    return [("default", base)]


def parse_variant(spec, base):
    label, _, settings = spec.partition(":")
    if not label:
        raise BenchmarkError(f"Variant '{spec}' needs a label")
    variant = dict(base)
    for setting in filter(None, (part.strip() for part in settings.split(","))):
        key, _, value = setting.partition("=")
        if key not in VARIANT_KEYS:
            raise BenchmarkError(f"Variant '{label}' has unknown key '{key}'")
        variant[key] = value
    return label, variant


# --- running ----------------------------------------------------------------------------------


def batch_command(args, variant, workspace, cache_dir):
    command = [
        "benchmark-load",
        "--workspace",
        str(workspace),
        "--parser",
        variant["parser"],
        "--unused",
        variant["unused"],
        "--unused-on-error",
        variant["unusedOnError"],
        "--logging",
        variant["logging"],
    ]
    command += ["--cache-dir", str(cache_dir)] if variant["cache"] == "on" else ["--no-cache"]
    if variant["parallelism"]:
        command += ["--parallelism", variant["parallelism"]]
    if variant["blockPrefetchThreads"]:
        command += ["--block-prefetch-threads", variant["blockPrefetchThreads"]]
    if args.include_paths:
        command.append("--include-paths")
    return command


def jvm_command(args, variant, jfr_file):
    """Fixed heap, so runs are comparable, and a bounded pool when a level was asked for."""
    command = [args.java, f"-Xms{args.heap}", f"-Xmx{args.heap}"]
    if variant["parallelism"]:
        for name in ("numThreads", "minThreads", "maxThreads"):
            command.append(f"-Dscala.concurrent.context.{name}={variant['parallelism']}")
    if jfr_file is not None:
        command.append(f"-XX:StartFlightRecording=filename={jfr_file},settings=profile")
    return command + ["-cp", args.classpath, BATCH_MAIN]


def run_once(args, variant, workspace, cache_dir, jfr_file):
    command = jvm_command(args, variant, jfr_file) + batch_command(args, variant, workspace, cache_dir)
    completed = subprocess.run(command, capture_output=True, text=True, check=False)
    try:
        envelope = json.loads(completed.stdout.strip().splitlines()[-1])
    except (ValueError, IndexError):
        raise BenchmarkError(
            f"Run produced no JSON document (status {completed.returncode}): {completed.stderr[-2000:]}"
        )
    if not envelope.get("ok"):
        raise BenchmarkError(f"Run failed: {json.dumps(envelope.get('error'))}")
    return envelope["result"], command


def measure(args, workspaces, variants):
    """Interleave variants (ABAB) rather than batching them, so drift affects both equally."""
    plans = []
    for name, workspace in workspaces:
        for label, variant in variants:
            plans.append({"name": name, "workspace": workspace, "label": label, "variant": variant})

    cache_dirs = []
    try:
        for plan in plans:
            plan["cache_dir"] = tempfile.mkdtemp(prefix="apex-ls-benchmark-cache-")
            cache_dirs.append(plan["cache_dir"])
            plan["runs"] = []
        for repetition in range(args.repetitions):
            for plan in plans:
                jfr_file = jfr_path(args, plan, repetition)
                result, command = run_once(
                    args, plan["variant"], plan["workspace"], plan["cache_dir"], jfr_file
                )
                plan["runs"].append(
                    {
                        "index": repetition,
                        "discarded": repetition == 0 and not args.keep_first,
                        "result": result,
                        "command": command,
                        "jfr": jfr_file,
                    }
                )
                report_progress(plan, repetition, result)
    finally:
        for directory in cache_dirs:
            shutil.rmtree(directory, ignore_errors=True)
    return plans


def jfr_path(args, plan, repetition):
    if args.jfr is None:
        return None
    directory = Path(args.jfr)
    directory.mkdir(parents=True, exist_ok=True)
    return str(directory / f"{plan['name']}-{plan['label']}-{repetition}.jfr")


def report_progress(plan, repetition, result):
    print(
        f"[{plan['name']}/{plan['label']}] run {repetition}: "
        f"{result['timings']['totalLoadMs']:.1f}ms",
        file=sys.stderr,
    )


# --- reporting --------------------------------------------------------------------------------


def summarise(samples):
    if not samples:
        return None
    median = statistics.median(samples)
    return {
        "n": len(samples),
        "medianMs": round(median, 3),
        "minMs": round(min(samples), 3),
        "maxMs": round(max(samples), 3),
        "spreadMs": round(max(samples) - min(samples), 3),
        "madMs": round(statistics.median([abs(sample - median) for sample in samples]), 3),
        "samplesMs": [round(sample, 3) for sample in samples],
    }


def variant_report(args, plan):
    kept = [run for run in plan["runs"] if not run["discarded"]]
    if not kept:
        raise BenchmarkError("Every run was discarded, use --repetitions 2 or more")
    last = kept[-1]["result"]
    flush = [run["result"]["timings"]["cacheFlushMs"] for run in kept]
    return {
        "label": plan["label"],
        "configuration": last["configuration"],
        "parallelism": {
            key: value for key, value in last["parallelism"].items() if key != "observedParseThreads"
        },
        "size": last["size"],
        "issues": last["issues"],
        "validation": last.get("validation"),
        "environment": last["environment"],
        "invocation": redact_command(args, plan, kept[-1]["command"]),
        "statistics": {
            "totalLoadMs": summarise([run["result"]["timings"]["totalLoadMs"] for run in kept]),
            "cacheFlushMs": summarise([value for value in flush if value is not None]),
        },
        "consistent": is_consistent(kept),
        "runs": [run_report(args, run) for run in plan["runs"]],
    }


def is_consistent(runs):
    """Two runs that saw different metadata or issues were not measuring the same thing."""
    keys = {(json.dumps(run["result"]["size"], sort_keys=True),
             json.dumps(run["result"]["issues"], sort_keys=True)) for run in runs}
    return len(keys) == 1


def run_report(args, run):
    result = run["result"]
    return {
        "index": run["index"],
        "discarded": run["discarded"],
        "totalLoadMs": result["timings"]["totalLoadMs"],
        "cacheFlushMs": result["timings"]["cacheFlushMs"],
        "observedParseThreads": result["parallelism"]["observedParseThreads"],
        "phases": result["timings"]["phases"],
        "validation": result.get("validation"),
        "jfr": run["jfr"] if args.include_paths else jfr_name(run["jfr"]),
    }


def jfr_name(path):
    return Path(path).name if path else None


def redact_command(args, plan, command):
    if args.include_paths:
        return command
    replacements = {
        str(plan["workspace"]): "<workspace>",
        str(plan["cache_dir"]): "<cache>",
        args.classpath: "<classpath>",
        args.java: "<java>",
    }
    redacted = []
    for token in command:
        for original, replacement in replacements.items():
            token = token.replace(original, replacement)
        redacted.append(redact_jfr(token))
    return redacted


def redact_jfr(token):
    if token.startswith("-XX:StartFlightRecording=") and "filename=" in token:
        head, _, tail = token.partition("filename=")
        _, _, rest = tail.partition(",")
        return f"{head}filename=<recording>" + (f",{rest}" if rest else "")
    return token


def build_report(args, plans, corpus):
    workspaces = {}
    for plan in plans:
        entry = workspaces.setdefault(
            plan["name"],
            {
                "name": plan["name"],
                "identity": plan["runs"][-1]["result"]["workspace"]["identity"],
                "path": plan["runs"][-1]["result"]["workspace"]["path"],
                "variants": [],
            },
        )
        entry["variants"].append(variant_report(args, plan))

    return {
        "schemaVersion": SCHEMA_VERSION,
        "generated": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "harness": {
            "repetitions": args.repetitions,
            "firstRunDiscarded": not args.keep_first,
            "heap": args.heap,
            "java": java_version(args.java),
            "classpath": args.classpath if args.include_paths else "<classpath>",
            "jfr": args.jfr is not None,
            "note": args.note,
        },
        "apexLs": {
            "commit": git(REPO_DIR, "rev-parse", "HEAD"),
            "describe": git(REPO_DIR, "describe", "--tags", "--always"),
        },
        "corpus": corpus,
        "workspaces": list(workspaces.values()),
    }


def java_version(java):
    try:
        result = subprocess.run([java, "-version"], capture_output=True, text=True, check=False)
    except OSError as error:
        raise BenchmarkError(f"Cannot run '{java}': {error}")
    return result.stderr.strip().splitlines()[0] if result.stderr else ""


# --- entry point ------------------------------------------------------------------------------


def resolve_workspaces(args):
    if args.corpus:
        if args.workspace:
            raise BenchmarkError("Use either --corpus or --workspace, not both")
        return resolve_corpus(args.corpus)
    if not args.workspace:
        raise BenchmarkError("One of --corpus or --workspace is required")

    workspaces = []
    for index, directory in enumerate(args.workspace):
        path = Path(directory).resolve()
        if not (path / "sfdx-project.json").is_file():
            raise BenchmarkError(f"Workspace {path} does not contain sfdx-project.json")
        name = args.name if args.name and len(args.workspace) == 1 else f"workspace-{index + 1}"
        workspaces.append((name, path))
    return workspaces, None


def main(argv):
    args = parse_args(argv)
    if args.list_corpora:
        for name, corpus in sorted(load_corpora().items()):
            print(f"{name}\t{corpus['ref']}\t{corpus['description']}")
        return 0
    if args.repetitions < 1:
        raise BenchmarkError("--repetitions must be at least 1")
    if not glob.glob(args.classpath):
        raise BenchmarkError(
            f"Nothing on the classpath {args.classpath}, run 'sbt apexlsJVM/build' first"
        )

    workspaces, corpus = resolve_workspaces(args)
    variants = build_variants(args)
    plans = measure(args, workspaces, variants)
    report = build_report(args, plans, corpus)

    text = json.dumps(report, indent=2) + "\n"
    if args.out:
        Path(args.out).write_text(text, encoding="utf-8")
        print(f"Wrote {args.out}", file=sys.stderr)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except BenchmarkError as error:
        print(f"error: {error}", file=sys.stderr)
        sys.exit(1)
