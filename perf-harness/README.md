# program-test perf harness

Compares two versions of `lamdera/program-test` by running a real app's
Effect.Test end-to-end suite headlessly with both, verifying the test output is
identical, and comparing wall time and memory.

Requirements: `python3`, `node`, and the `lamdera` compiler on PATH (or pass
`--lamdera /path/to/lamdera`).

## Examples

```bash
# at-chat (tests/E2ETests.elm exposes `setup : ViewerWith (List (EndToEndTest ...))`)
python3 perf-harness/run.py --app ../at-chat \
    --import E2ETests --expr E2ETests.setup

# meetdown (src/Tests.elm exposes `tests : List (EndToEndTest ...)`)
python3 perf-harness/run.py --app ../meetdown \
    --import Tests --expr "Effect.Test.viewerWith Tests.tests"
```

`--baseline` and `--new` accept a git rev of this repository (default baseline:
the `3.0.0` tag) or a path to a checkout (default new: this working tree).
`--runs` (default 3) timed runs per version are interleaved to keep the
comparison fair.

The app's test module must expose its tests (add `tests` — or a `setup` value of
type `ViewerWith` — to the module's `exposing` list if it only exposes `main`).
Apps whose tests require WebGL textures (`addTexture*`) can't run headlessly in
node, and apps pinned to a different major version of program-test won't
compile; the harness reports the compile error in that case.

## How it works

- The app is copied to a work dir and a generated `port module PerfRunner`
  (`Effect.Test.startHeadless output <expr>`) is added.
- Both program-test versions are injected into a private ELM_HOME package cache
  as version 3.0.0, so the app's elm.json is untouched; the app is compiled once
  per version.
- `run-tests.js` executes the compiled worker in node. File loads made by
  `addStringFile`/`addBytesFile` are served from the app directory through a
  small XMLHttpRequest shim.
- Output (null = all passed, otherwise the failure report) must be identical
  across versions or the harness exits nonzero.

`seed-elm-home.py` is only needed in sandboxes where GitHub archive downloads
are blocked; it pre-fills an ELM_HOME from a donor cache and jsDelivr.
