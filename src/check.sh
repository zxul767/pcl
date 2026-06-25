#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)

cd "$repo_root"
# Run Roswell with Quicklisp enabled (-Q), load the script (-l), then quit (-q).
exec ros -Q \
    -e "(pushnew :run-main *features*)" \
    -l "$script_dir/check.lisp" \
    -q
