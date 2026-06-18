#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
upstream_pss_dir="${UPSTREAM_PSS_DIR:-"$repo_root/../persistent-sorted-set"}"

common_benches=(
  conj-10K
  disj-10K
  contains-10K
  doseq-300K
  next-300K
  reduce-300K
)

if [ "$#" -gt 0 ]; then
  benches=("$@")
else
  benches=("${common_benches[@]}")
fi

cd "$repo_root"
dune build --profile release bench/bench_pss.exe bench/bench_pss.bc.js

printf "\n== ocaml-native ==\n"
"$repo_root/_build/default/bench/bench_pss.exe" "${benches[@]}"

printf "\n== js_of_ocaml ==\n"
node "$repo_root/_build/default/bench/bench_pss.bc.js" "${benches[@]}"

printf "\n== upstream-cljs-js ==\n"
(
  cd "$upstream_pss_dir"
  script/bench_cljs.sh "${benches[@]}"
)
