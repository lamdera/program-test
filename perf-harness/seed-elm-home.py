#!/usr/bin/env python3
"""Populate an ELM_HOME package cache without using GitHub zipball downloads.

Normally the lamdera compiler downloads package sources itself and this script is
unnecessary. It exists for sandboxed environments where github.com archive
endpoints are blocked: it reads an application elm.json and fills the target
ELM_HOME with every direct + indirect dependency, copying from a donor ELM_HOME
when possible and otherwise downloading the files from cdn.jsdelivr.net (which
mirrors tagged GitHub repositories).

Usage:
    python3 seed-elm-home.py <app-elm.json> <target-elm-home> [--donor ~/.elm] [--ca <ca-bundle>]
"""

import argparse
import json
import pathlib
import shutil
import ssl
import urllib.request


def fetch(url, ctx):
    req = urllib.request.Request(url, headers={"User-Agent": "elm-home-seed"})
    with urllib.request.urlopen(req, context=ctx, timeout=120) as r:
        return r.read()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("app_elm_json")
    parser.add_argument("target_elm_home")
    parser.add_argument("--donor", default=str(pathlib.Path.home() / ".elm"))
    parser.add_argument("--ca", default=None, help="CA bundle for https (e.g. a proxy's CA)")
    args = parser.parse_args()

    ctx = ssl.create_default_context(cafile=args.ca) if args.ca else ssl.create_default_context()
    elm_json = json.loads(pathlib.Path(args.app_elm_json).read_text())
    deps = {}
    for section in elm_json["dependencies"].values():
        deps.update(section)

    target = pathlib.Path(args.target_elm_home) / "0.19.1" / "packages"
    donor = pathlib.Path(args.donor) / "0.19.1" / "packages"

    for pkg, version in sorted(deps.items()):
        if pkg == "lamdera/program-test":
            continue  # injected separately by run.py
        dest = target / pkg / version
        if (dest / "src").exists():
            print(f"{pkg}@{version}: already present")
            continue
        donor_dir = donor / pkg / version
        if (donor_dir / "src").exists():
            shutil.rmtree(dest, ignore_errors=True)
            # Don't copy build artifacts; the compiler rebuilds them per ELM_HOME.
            shutil.copytree(donor_dir, dest, ignore=shutil.ignore_patterns("artifacts.dat", "artifacts.x.dat"))
            print(f"{pkg}@{version}: copied from donor")
            continue
        listing = json.loads(fetch(f"https://data.jsdelivr.com/v1/packages/gh/{pkg}@{version}?structure=flat", ctx))
        wanted = [
            f["name"]
            for f in listing["files"]
            if f["name"].startswith("/src/") or f["name"] in ("/elm.json", "/LICENSE", "/README.md")
        ]
        for name in wanted:
            file_dest = dest / name.lstrip("/")
            file_dest.parent.mkdir(parents=True, exist_ok=True)
            file_dest.write_bytes(fetch(f"https://cdn.jsdelivr.net/gh/{pkg}@{version}{name}", ctx))
        print(f"{pkg}@{version}: downloaded {len(wanted)} files from jsDelivr")


if __name__ == "__main__":
    main()
