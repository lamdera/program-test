#!/usr/bin/env python3
"""Compare two versions of lamdera/program-test against a real app's end-to-end tests.

For a given app (a Lamdera project whose tests are written with Effect.Test), this:

1. Copies the app into a work directory and adds a tiny `port module PerfRunner`
   that runs the app's tests with Effect.Test.startHeadless.
2. Compiles it twice: once with the baseline program-test and once with the new
   program-test. Both are injected into the ELM_HOME package cache as version
   3.0.0 so the app's elm.json doesn't need to change.
3. Runs each compiled program N times in node (alternating baseline/new) and
   verifies both versions produce byte-identical test output.
4. Prints a timing/memory comparison.

Example:

    python3 perf-harness/run.py \
        --app ../at-chat \
        --import E2ETests \
        --expr E2ETests.setup \
        --baseline 3.0.0 \
        --runs 3

--baseline / --new accept either a git rev of this repository (e.g. "3.0.0" or
"HEAD") or a path to a program-test checkout. --new defaults to the working tree.
"""

import argparse
import json
import os
import pathlib
import shutil
import statistics
import subprocess
import sys
import tempfile

HARNESS_DIR = pathlib.Path(__file__).resolve().parent
REPO_DIR = HARNESS_DIR.parent

RUNNER_TEMPLATE = """port module PerfRunner exposing (main)

import Effect.Test
import Json.Encode
import {import_}


port output : Json.Encode.Value -> Cmd msg


main =
    Effect.Test.startHeadless output ({expr})
"""


def run(cmd, **kwargs):
    print("$ " + " ".join(str(c) for c in cmd))
    return subprocess.run([str(c) for c in cmd], **kwargs)


def materialize_program_test(spec, dest):
    """Copy a program-test version (git rev of this repo, or a directory) to dest."""
    dest.mkdir(parents=True)
    as_path = pathlib.Path(spec)
    if (as_path / "elm.json").exists():
        shutil.copy(as_path / "elm.json", dest / "elm.json")
        shutil.copytree(as_path / "src", dest / "src")
    else:
        tar = subprocess.run(
            ["git", "-C", str(REPO_DIR), "archive", spec, "elm.json", "src"],
            check=True,
            stdout=subprocess.PIPE,
        )
        subprocess.run(["tar", "-x", "-C", str(dest)], input=tar.stdout, check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--app", required=True, help="path to the app repository")
    parser.add_argument("--import", dest="import_", required=True, help="module containing the tests, e.g. E2ETests")
    parser.add_argument(
        "--expr",
        required=True,
        help="Elm expression of type ViewerWith (List (EndToEndTest ...)), e.g. "
        "'E2ETests.setup' or 'Effect.Test.viewerWith Tests.tests'",
    )
    parser.add_argument("--baseline", default="3.0.0", help="git rev or path for the baseline program-test")
    parser.add_argument("--new", dest="new", default=str(REPO_DIR), help="git rev or path for the new program-test")
    parser.add_argument("--runs", type=int, default=3, help="timed runs per version")
    parser.add_argument("--lamdera", default="lamdera", help="lamdera compiler binary")
    parser.add_argument("--work", default=None, help="work directory (default: temp dir)")
    parser.add_argument(
        "--node-arg",
        action="append",
        default=[],
        help="extra node flag for the timed runs, e.g. --node-arg=--initial-old-space-size=2048 "
        "(repeatable; useful for separating GC-heuristic effects from actual compute)",
    )
    parser.add_argument(
        "--pkg-version",
        default="3.0.0",
        help="lamdera/program-test version the app's elm.json pins (both versions are injected as this)",
    )
    args = parser.parse_args()

    app_src = pathlib.Path(args.app).resolve()
    work = pathlib.Path(args.work).resolve() if args.work else pathlib.Path(tempfile.mkdtemp(prefix="pt-perf-"))
    work.mkdir(parents=True, exist_ok=True)
    print(f"work directory: {work}")

    # 1. Copy the app and add the runner module.
    app = work / "app"
    if app.exists():
        shutil.rmtree(app)
    shutil.copytree(app_src, app, ignore=shutil.ignore_patterns(".git", "elm-stuff", "node_modules"))
    (app / "perf-runner").mkdir()
    (app / "perf-runner" / "PerfRunner.elm").write_text(
        RUNNER_TEMPLATE.format(import_=args.import_, expr=args.expr)
    )
    elm_json_path = app / "elm.json"
    elm_json = json.loads(elm_json_path.read_text())
    elm_json["source-directories"].append("perf-runner")
    deps = elm_json["dependencies"]["direct"]
    if deps.get("lamdera/program-test") != args.pkg_version:
        print(
            f"note: app pins lamdera/program-test {deps.get('lamdera/program-test')}, "
            f"overriding to {args.pkg_version}"
        )
        deps["lamdera/program-test"] = args.pkg_version
    elm_json_path.write_text(json.dumps(elm_json, indent=4))

    # 2. Compile once per version, swapping the package cache contents in between.
    elm_home = work / "elm-home"
    pkg_dir = elm_home / "0.19.1" / "packages" / "lamdera" / "program-test" / args.pkg_version
    outputs = {}
    for name, spec in [("baseline", args.baseline), ("new", args.new)]:
        if pkg_dir.exists():
            shutil.rmtree(pkg_dir)
        materialize_program_test(spec, pkg_dir)
        shutil.rmtree(app / "elm-stuff", ignore_errors=True)
        out = work / f"runner-{name}.js"
        result = run(
            [args.lamdera, "make", "perf-runner/PerfRunner.elm", "--output", str(out)],
            cwd=app,
            env={**os.environ, "ELM_HOME": str(elm_home)},
        )
        if result.returncode != 0:
            print(f"FAILED to compile with {name} program-test ({spec})")
            sys.exit(1)
        outputs[name] = out

    # 3. Run both alternately.
    results = {"baseline": [], "new": []}
    for i in range(args.runs):
        for name in ["baseline", "new"]:
            proc = run(
                ["node", *args.node_arg, str(HARNESS_DIR / "run-tests.js"), str(outputs[name]), str(app)],
                stdout=subprocess.PIPE,
                text=True,
            )
            line = proc.stdout.strip().splitlines()[-1] if proc.stdout.strip() else "{}"
            data = json.loads(line)
            if not data.get("ok"):
                print(f"{name} run failed: {data}")
                sys.exit(1)
            results[name].append(data)
            status = "all passed" if data["output"] is None else "FAILURES REPORTED"
            print(
                f"  {name:>8} run {i + 1}: {data['elapsed_ms']:>7} ms, "
                f"max rss {data['max_rss_mb']:>5} MB, heap {data['heap_used_mb']:>5} MB ({status})"
            )

    # 4. Verify identical output and summarize.
    baseline_out = {json.dumps(r["output"]) for r in results["baseline"]}
    new_out = {json.dumps(r["output"]) for r in results["new"]}
    print()
    if len(baseline_out) > 1 or len(new_out) > 1:
        print("WARNING: output differed between runs of the same version (flaky tests?)")
    if baseline_out == new_out:
        sample = results["baseline"][0]["output"]
        print("OK: baseline and new produced identical test output")
        print("    (" + ("all tests passed" if sample is None else "same failure report from both") + ")")
    else:
        print("MISMATCH: baseline and new produced different test output!")
        print("  baseline:", list(baseline_out)[0][:1000])
        print("  new:     ", list(new_out)[0][:1000])
        sys.exit(1)

    def med(name, key):
        return statistics.median(r[key] for r in results[name])

    b_ms, n_ms = med("baseline", "elapsed_ms"), med("new", "elapsed_ms")
    b_rss, n_rss = med("baseline", "max_rss_mb"), med("new", "max_rss_mb")
    b_heap, n_heap = med("baseline", "heap_used_mb"), med("new", "heap_used_mb")
    print()
    print(f"median over {args.runs} runs      baseline        new")
    print(f"  elapsed             {b_ms:>9.0f} ms {n_ms:>9.0f} ms  ({(n_ms - b_ms) / b_ms * 100:+.1f}%)")
    print(f"  max rss             {b_rss:>9.0f} MB {n_rss:>9.0f} MB  ({(n_rss - b_rss) / b_rss * 100:+.1f}%)")
    print(f"  final heap          {b_heap:>9.0f} MB {n_heap:>9.0f} MB  ({(n_heap - b_heap) / b_heap * 100:+.1f}%)")


if __name__ == "__main__":
    main()
