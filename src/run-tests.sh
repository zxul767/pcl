#!/usr/bin/env bash
set -euo pipefail

printenv

run-fiveam -e t -l functools/tests \
    dev.zxul767.functools-tests:master-suite \
    && echo "ALL TESTS PASSED"
