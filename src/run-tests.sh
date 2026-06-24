#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

echo "run-tests.sh is deprecated; use check.sh instead." >&2
exec "$script_dir/check.sh"
