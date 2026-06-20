#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workflow="$repo_root/.github/workflows/ci.yml"

if [ ! -f "$workflow" ]; then
  echo "CI workflow not found: .github/workflows/ci.yml" >&2
  exit 1
fi

if ! grep -q 'CHROME_BIN=' "$workflow"; then
  echo "CI must export CHROME_BIN for browser memory tests" >&2
  exit 1
fi

if ! grep -q 'run_browser_memory_test.js' "$repo_root/test/dune"; then
  echo "dune runtest must include the browser memory test runner" >&2
  exit 1
fi

if ! grep -q -- '--expose-gc' "$repo_root/test/dune"; then
  echo "JS memory tests must run with GC exposed" >&2
  exit 1
fi
