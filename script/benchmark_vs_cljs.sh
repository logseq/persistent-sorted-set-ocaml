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
for bench in "${benches[@]}"; do
  "$repo_root/_build/default/bench/bench_pss.exe" "$bench" | tail -n +2
done

printf "\n== js_of_ocaml ==\n"
for bench in "${benches[@]}"; do
  node "$repo_root/_build/default/bench/bench_pss.bc.js" "$bench" | tail -n +2
done

printf "\n== upstream-cljs-js ==\n"
(
  cd "$upstream_pss_dir"
  yarn shadow-cljs release bench
  for bench in "${benches[@]}"; do
    node target/bench.js "$bench" | tail -n +2
  done
)
