#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)

cd "$repo_root"
ros_args=()
if [[ -n "${LISP:-}" ]]; then
    ros_args+=(-L "$LISP")
fi

# Run Roswell with Quicklisp enabled (-Q), load the script (-l), then quit (-q).
if ((${#ros_args[@]})); then
    exec ros "${ros_args[@]}" -Q \
        -e "(pushnew :run-main *features*)" \
        -l "$script_dir/check.lisp" \
        -q
else
    exec ros -Q \
        -e "(pushnew :run-main *features*)" \
        -l "$script_dir/check.lisp" \
        -q
fi
