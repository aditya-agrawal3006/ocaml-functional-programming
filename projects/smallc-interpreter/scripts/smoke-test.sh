#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
dune build

# Make sure the first two stages can process the same input.
dune exec bin/interface.exe -- lex examples/function-call.c >/dev/null
dune exec bin/interface.exe -- parse examples/function-call.c >/dev/null
printf 'PASS lexer and parser\n'

check() {
  local file="$1" expected="$2" actual
  actual="$(dune exec bin/interface.exe -- eval "examples/$file")"
  if [[ "$actual" != "$expected" ]]; then
    printf 'FAIL %s\nExpected: %q\nActual:   %q\n' "$file" "$expected" "$actual" >&2
    exit 1
  fi
  printf 'PASS %s\n' "$file"
}

check function-call.c '25'
check sum-loop.c '15'
check while-and-if.c $'0\n1\n2\ntrue'
printf 'All smoke checks passed.\n'
